{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Description : Generate Haskell from Wayland xml protocol files.
--
-- This API is internal, so it is quite messy and is highly unstable
-- You can still use it if you want to implement your own protocols.
module Sayland.Internal.Codegen (module Sayland.Internal.Codegen) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor (bimap, first)
import Data.Binary
import Data.Bits
import Data.Bool
import Data.ByteString qualified as BS
import Data.Char (isSpace, toUpper)
import Data.Functor
import Data.List
import Data.List qualified as L
import Data.Maybe (catMaybes, fromJust)
import Data.Proxy
import Data.String
import GHC.Generics (Generic)
import GHC.TypeError (Unsatisfiable)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (quoteExp))
import Language.Haskell.TH.Syntax
import Sayland.Internal.Core
import Sayland.Internal.Prelude
import Sayland.Wire
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Show qualified
import Text.XML.Light

qname :: String -> QName
qname x = QName x Nothing Nothing

-- | Look up a required attribute, failing the splice with the element's name when it is absent.
attr :: String -> Element -> Q String
attr n el =
  maybe
    (fail $ "sayland: element `" <> qName (elName el) <> "` has no `" <> n <> "` attribute")
    pure
    (findAttr (qname n) el)

-- | Interfaces created by a @new_id@ argument somewhere in this protocol.
createdInterfaces :: Element -> [String]
createdInterfaces e =
  [ iface
  | int <- findChildren (qname "interface") e
  , msg <- findChildren (qname "request") int <> findChildren (qname "event") int
  , arg <- findChildren (qname "arg") msg
  , findAttr (qname "type") arg == Just "new_id"
  , Just iface <- [findAttr (qname "interface") arg]
  ]

-- | Whether an interface has a usable 'Object' instance. I.e it exists, and it isn't an 'Unsatisfiable' placeholder.
-- Also throw error if an object is `Unsatisfiable` but the global instance of it isn't.
isImplemented :: Name -> Q Bool
isImplemented ty =
  reifyInstances ''Object [ConT ty] >>= \case
    [InstanceD _ ctx _ _]
      | any isUnsatisfiable ctx -> do
          hasGlobal <- isInstance ''Global [ConT ty]
          unless hasGlobal
            . fail
            $ "sayland: "
            <> nameBase ty
            <> " is a global with an Unsatisfiable `Object` instance. declare the same for `Global`."
          pure False
      | otherwise -> pure True
    _ -> pure False
  where
    isUnsatisfiable (AppT (ConT n) _) = n == ''Unsatisfiable
    isUnsatisfiable _ = False

-- | Generates the table for the given protocol, using formatter to format
-- interface type names - as they are to be defined by the user. Only globals are
-- listed: an interface created by a @new_id@ is not registry-bindable. Also emits
-- the `Global` instances for those globals whose only field is @wlid@.
generateProtocolTable :: Bool -> Element -> (String -> String) -> Q [Dec]
generateProtocolTable isIO e formatter = do
  protocol <- attr "name" e
  names <- traverse (attr "name") (findChildren (qname "interface") e)
  let tname = mkName $ protocol <> "Table"
  implementedGlobals <-
    let
      created = createdInterfaces e
      globals = filter (\n -> n /= "wl_display" && n `notElem` created) names
     in
      if isIO
        then pure globals
        else filterM (\n -> lookupTypeName (formatter n) >>= maybe (pure False) isImplemented) globals
  instances <- if isIO then pure [] else concat <$> traverse globalInstance implementedGlobals
  defs <- traverse entry implementedGlobals
  pure
    $ instances
    <> [ SigD tname (ConT ''ProtocolTable)
       , ValD (VarP tname) (NormalB $ ListE defs) []
       ]
  where
    globalInstance n =
      lookupTypeName (formatter n) >>= \case
        Just ty -> deriveGlobal ty
        Nothing -> fail $ "sayland: protocol declares global `" <> n <> "` but no type `" <> formatter n <> "` is in scope."
    entry x = do
      oid <- newName "oid"
      pure
        $ TupE
          [ Just $ AppE (VarE 'getInterfaceName) (proxy x)
          , -- InterfaceEntry <version> <constructor>
            Just
              $ AppE
                (AppE (ConE 'InterfaceEntry) (AppE (VarE 'getInterfaceVersion) (proxy x)))
              $ LamE [VarP oid]
              $ AppE (AppE (VarE '(<$>)) (ConE 'SomeObject))
              $ SigE
                (AppE (VarE 'global) (AppE (ConE 'TObjectID) (VarE oid)))
                (AppT (ConT ''IO) (ConT . mkName $ formatter x))
          ]
    proxy x = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (ConT . mkName $ formatter x))

-- Haddock {{{

-- | Haddock text for an XML element. its @\<description\>@ and the
-- @summary@ attribute as the first paragraph and the body as the rest. Fallback
-- to just @summary@ attribute.
elemDoc :: Element -> Maybe String
elemDoc el = escapeHaddock <$> (descDoc <|> summaryAttr el)
  where
    descDoc = do
      d <- findChild (qname "description") el
      case catMaybes [summaryAttr d, nonBlank . dedent $ strContent d] of
        [] -> Nothing
        ps -> Just $ intercalate "\n\n" ps
    summaryAttr e = L.unwords . L.words <$> findAttr (qname "summary") e
    nonBlank s = if all isSpace s then Nothing else Just s

-- | Strip the common indentation the description body inherits from the XML file.
dedent :: String -> String
dedent s = intercalate "\n" $ strip <$> body
  where
    body = L.dropWhileEnd blank . dropWhile blank $ L.lines s
    blank :: [Char] -> Bool = all isSpace
    indent = case filter (not . blank) body of
      [] -> 0
      ls -> L.minimum $ length . takeWhile (== ' ') <$> ls
    strip l = if blank l then "" else drop indent l

-- | Escape Haddock markup. Wayland descriptions are full of bs.
escapeHaddock :: String -> String
escapeHaddock = concatMap $ \c -> if c `elem` ("\\/'\"@<>[]#" :: String) then ['\\', c] else [c]

-- | Attach a Haddock comment to a name defined by the current splice.
-- No-op under `isIO`, where there is no splice to finalize.
docDecl :: Bool -> Name -> Maybe String -> Q ()
docDecl isIO n = unless isIO . mapM_ (addModFinalizer . putDoc (DeclDoc n))

-- | Attach a Haddock comment to the @i@th argument of a function or constructor.
docArg :: Bool -> Name -> Int -> Maybe String -> Q ()
docArg isIO n i = unless isIO . mapM_ (addModFinalizer . putDoc (ArgDoc n i))

-- }}}

-- | @instance Global T@, for globals whose only field is @wlid@ the default
-- method coerces the id. Reports warns for globals that need to be handwritten.
deriveGlobal :: Name -> Q [Dec]
deriveGlobal ty = do
  (_cn, fields) <- soleRecordCon ty
  case fields of
    [("wlid", _)] -> pure [InstanceD Nothing [] (AppT (ConT ''Global) (ConT ty)) []]
    fs
      | "wlid" `notElem` fmap fst fs ->
          fail $ "sayland: " <> nameBase ty <> " has no `wlid` field"
      | otherwise -> do
          declared <- isInstance ''Global [ConT ty]
          unless declared
            . reportWarning
            . intercalate "\n"
            $ [ "sayland: " <> nameBase ty <> " is a global with fields beyond `wlid`, so its"
              , "  `Global` instance cannot be derived. Write one above the `generateTables`"
              , "  splice, e.g."
              , ""
              , "    instance Global " <> nameBase ty <> " where"
              , "      global i = " <> nameBase ty <> " i <$> newIORef ..."
              , ""
              , "  fields to initialise: " <> intercalate ", " [f | (f, _) <- fs, f /= "wlid"]
              ]
          pure []
  where
    -- Strip the @$sel:wlid:Wl_seat@ mangling DuplicateRecordFields can introduce.
    fieldBase :: Name -> String
    fieldBase n = case nameBase n of
      '$' : 's' : 'e' : 'l' : ':' : rest -> takeWhile (/= ':') rest
      b -> b

    -- Constructor of a record with its fields.
    soleRecordCon :: Name -> Q (Name, [(String, Type)])
    soleRecordCon typ = do
      con <-
        reify typ >>= \case
          TyConI (NewtypeD _ _ _ _ c _) -> pure c
          TyConI (DataD _ _ _ _ [c] _) -> pure c
          TyConI (DataD _ _ _ _ cs _) ->
            fail $ "sayland: " <> nameBase typ <> " has " <> show (length cs) <> " constructors, expected 1"
          _ -> fail $ "sayland: " <> nameBase typ <> " is not a data or newtype declaration"
      case con of
        RecC cn fields -> pure (cn, [(fieldBase f, t) | (f, _, t) <- fields])
        _ -> fail $ "sayland: " <> nameBase typ <> " is not a record"

-- | Defines an enum-like along with a function to look up the value of each element.
-- example output:
-- data Enum_[interface]_[name] = A | B | C | D ... deriving (Eq, Ord)
-- enumName' A = 1 ...
--
-- if the enum is a bitfield, instead generates the following:
-- data Enum_[interface]_[name] = Enum_[interface]_[name] {[name]_[entry] :: Bool, [name]_[entry2] :: Bool, ...} deriving (Eq, Ord, Generic)
mkEnum :: Bool -> String -> Element -> Q [Dec]
mkEnum isIO interfaceName enumEl = do
  bool
    ( do
        forM_ entries $ \e -> docDecl isIO (mkName $ enumName'' <> entryName e) (elemDoc e)
        docDecl isIO (mkName enumName') (elemDoc enumEl)
        docCodec
        codec <- enumCodec
        pure
          $ [DataD [] (mkName enumName') [] Nothing constructors [DerivClause (Just StockStrategy) [ConT ''Eq, ConT ''Ord]]]
          <> codec
          <> [ InstanceD
                 Nothing
                 []
                 (AppT (ConT ''Show) $ ConT $ mkName enumName')
                 [FunD 'Text.Show.showsPrec show_clauses]
             ]
          <> errorInstance
    )
    ( do
        docDecl isIO (mkName enumName') (elemDoc enumEl)
        docCodec
        codec <- bitfieldCodec
        pure
          $ [DataD [] (mkName enumName') [] Nothing bitfieldConstructor [DerivClause (Just StockStrategy) [ConT ''Eq, ConT ''Ord, ConT ''Generic]]]
          <> codec
          <> [ InstanceD
                 Nothing
                 []
                 (AppT (ConT ''Show) $ ConT $ mkName enumName')
                 [FunD 'Text.Show.showsPrec bitfield_show_clauses]
             ]
    )
    isBitfield
  where
    enumName = fromJust $ findAttr (qname "name") enumEl
    entries = findChildren (qname "entry") enumEl
    entryName e = fromJust $ findAttr (qname "name") e
    enumKV = [(entryName e, read . fromJust $ findAttr (qname "value") e) | e <- entries]
    enumKeys = fmap fst enumKV

    enumName' = "Enum_" <> interfaceName <> "_" <> enumName
    enumName'' = enumName' <> "_"
    constructors = (`NormalC` []) . mkName . (enumName'' <>) <$> fmap fst enumKV

    putName = mkName $ "put" <> enumName'
    getName = mkName $ "get" <> enumName'
    enumT = conT $ mkName enumName'
    docCodec = do
      docDecl isIO putName . Just $ "Put a t'" <> enumName' <> "'."
      docDecl isIO getName . Just $ "Get a t'" <> enumName' <> "'."
    codecSigs =
      [ sigD putName [t|$enumT -> WirePut ()|]
      , sigD getName [t|WireGet (Either MessageError $enumT)|]
      ]

    -- putEnum_[interface]_[name] A = putWlUInt 1
    -- getEnum_[interface]_[name] = (\case Left short -> ...; Right 1 -> Right A; Right unknown -> ...) <$> getWlUInt
    enumCodec = do
      short <- newName "short"
      unknown <- newName "unknown"
      let entryCon k = mkName $ enumName'' <> k
          matches =
            [match (conP 'Left [varP short]) (normalB [|Left (BodyTooShort $(varE short))|]) []]
              <> [match (conP 'Right [litP $ integerL v]) (normalB [|Right $(conE $ entryCon k)|]) [] | (k, v) <- enumKV]
              <> [match (conP 'Right [varP unknown]) (normalB [|Left (UnknownEnumValue $(varE unknown))|]) []]
      sequence
        $ codecSigs
        <> [ funD putName [clause [conP (entryCon k) []] (normalB [|putWlUInt $(litE $ integerL v)|]) [] | (k, v) <- enumKV]
           , valD (varP getName) (normalB [|$(lamCaseE matches) <$> getWlUInt|]) []
           ]

    show_clauses =
      [ Clause
          [WildP, ConP (mkName $ enumName'' <> k) [] []]
          (NormalB $ AppE (VarE 'Text.Show.showString) (LitE (StringL k)))
          []
      | (k, _) <- enumKV
      ]

    errorInstance =
      [ InstanceD
          Nothing
          []
          (AppT (ConT ''ErrorCode) (ConT $ mkName enumName'))
          [ FunD
              'errorCode
              [ Clause [ConP (mkName $ enumName'' <> k) [] []] (NormalB $ AppE (ConE 'WlUInt) (LitE $ IntegerL v)) []
              | (k, v) <- enumKV
              ]
          ]
      | enumName == "error"
      ]

    isBitfield = case findAttr (qname "bitfield") enumEl of
      Just x -> x == "true"
      Nothing -> False
    bitfieldConstructor = [RecC (mkName enumName') [(mkName $ enumName <> "_" <> name, Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Bool) | name <- enumKeys]]

    -- putEnum_[interface]_[name] (Enum_[interface]_[name] a b) = putWlUInt (sum [bool 0 1 a, bool 0 2 b])
    -- getEnum_[interface]_[name] = bimap BodyTooShort (\(WlUInt byte) -> Enum_[interface]_[name] (testBit byte 0) (testBit byte 1)) <$> getWlUInt
    bitfieldCodec = do
      fields <- traverse (newName . ("field_" <>)) enumKeys
      byte <- newName "byte"
      let flags = listE [[|bool 0 $(litE $ integerL v) $(varE f)|] | (f, (_, v)) <- zip fields enumKV]
          bits = [if v == 0 then [|True|] else [|testBit $(varE byte) $(litE . integerL . round $ logBase (2 :: Float) (fromIntegral v))|] | (_, v) <- enumKV]
      sequence
        $ codecSigs
        <> [ funD putName [clause [conP (mkName enumName') (varP <$> fields)] (normalB [|putWlUInt (sum $flags)|]) []]
           , valD (varP getName) (normalB [|bimap BodyTooShort $(lamE [conP 'WlUInt [varP byte]] $ foldl appE (conE $ mkName enumName') bits) <$> getWlUInt|]) []
           ]
    bitfield_show_clauses =
      [ Clause
          [WildP, RecP (mkName enumName') [(mkName $ enumName <> "_" <> field, VarP (mkName $ "field_" <> field)) | field <- enumKeys]]
          ( NormalB
              $ AppE (VarE 'Text.Show.showString)
              $ AppE (AppE (VarE 'intercalate) $ LitE $ StringL " .|. ")
              $ AppE (AppE (VarE 'fmap) $ VarE 'fst)
              $ AppE (AppE (VarE 'filter) $ VarE 'snd)
              $ ListE [TupE [Just $ LitE $ StringL field, Just $ VarE $ mkName $ "field_" <> field] | field <- enumKeys]
          )
          []
      ]

wlFormatter :: String -> String
wlFormatter [] = []
wlFormatter (x : xs) = toUpper x : xs

-- | Loads all .xml files in `path` as protocols.
-- Set `isIO` to True only when running the function within an IO monad. This should be used *only* for debugging purposes.
-- `monad` defines the monad in which all events and requests operate in.
loadProtocols :: (String -> String) -> Bool -> FilePath -> Q [Dec]
loadProtocols formatter isIO path = do
  protocol_files <- filter ((== ".xml") . takeExtension) <$> runIO (listDirectory path)
  concat <$> mapM (loadProtocolFile formatter isIO . (path </>)) protocol_files

findInterfaces :: Element -> [Element]
findInterfaces = findChildren (qname "interface")

-- | Load a protocol from the specified `path`. Arguments have the same meaning as in `loadProtocols`.
-- | The @\<protocol\>@ elements of a protocol file, recompiling when it changes.
readProtocols :: Bool -> FilePath -> Q [Element]
readProtocols isIO path = do
  unless isIO $ addDependentFile path
  filter ((== qname "protocol") . elName) . onlyElems . parseXML <$> runIO (BS.readFile path)

loadProtocolFile :: (String -> String) -> Bool -> FilePath -> Q [Dec]
loadProtocolFile formatter isIO path = do
  protocols <- readProtocols isIO path
  concat
    <$> mapM
      ((<&> concat) . mapM (loadInterface formatter isIO) . findInterfaces)
      protocols

loadProtocolFileEnums :: Bool -> FilePath -> Q [Dec]
loadProtocolFileEnums isIO path = do
  protocols <- readProtocols isIO path
  concat . concat <$> mapM (mapM (loadInterfaceEnums isIO) . findInterfaces) protocols

generateTables :: Bool -> (String -> String) -> FilePath -> Q [Dec]
generateTables isIO formatter path = do
  protocols <- readProtocols isIO path
  concat <$> traverse (\p -> generateProtocolTable isIO p formatter) protocols

-- | Create message types (requests and events).
mkMessages :: Bool -> (String -> String) -> String -> String -> [Element] -> Q [Dec]
mkMessages isIO formatter interfaceName prefix events = do
  docDecl isIO (mkName prefix') . Just $ prefix <> "s of the t'" <> formatter interfaceName <> "' interface."
  forM_ events $ \e -> do
    docDecl isIO (conName e) (elemDoc e)
    forM_ (zip [0 ..] $ findChildren (qname "arg") e) $ \(i, a) ->
      docArg isIO (conName e) i (elemDoc a)
  pure [DataD [] (mkName prefix') [] Nothing constructors []]
  where
    prefix' = prefix <> "_" <> interfaceName
    conName x = mkName $ prefix' <> "_" <> fromJust (findAttr (qname "name") x)
    buildBang x = (Bang NoSourceUnpackedness NoSourceStrictness, (argCodec formatter interfaceName x).argTy)
    buildRecord x = NormalC (conName x) $ buildBang <$> findChildren (qname "arg") x
    constructors = fmap buildRecord events

mkShow :: String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkShow interfaceName prefix prefix2 events =
  mapM (mkShowC . snd) events <&> \m ->
    [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''ObjectID) $ AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''String)
    , FunD (mkName prefix) $ bool m [Clause [WildP, WildP] (NormalB $ LitE $ StringL $ interfaceName <> "@?") []] (null m)
    ]
  where
    -- showMessage oid (Con a b) = "<interface>@" <> show oid <> ".<message>: " <> " a: " <> show a <> " b: " <> show b
    mkShowC :: Element -> Q Clause
    mkShowC e = do
      oid <- newName "oid"
      vars <- traverse newName argNames
      let header = [|$(stringE $ interfaceName <> "@") <> show $(varE oid) <> $(stringE $ "." <> eventName <> bool ": " "" (null argNames))|]
          single acc (n, v) = [|$acc <> $(stringE $ " " <> n <> ": ") <> show $(varE v)|]
      clause [varP oid, conP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) (varP <$> vars)] (normalB . foldl' single header $ zip argNames vars) []
      where
        argNames = fromJust . findAttr (qname "name") <$> findChildren (qname "arg") e
        eventName = fromJust $ findAttr (qname "name") e

mkOpcodeGetter :: String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkOpcodeGetter interfaceName prefix prefix2 events =
  mapM mkClause events <&> \m ->
    [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''Word16)
    , FunD (mkName prefix) $ bool m [Clause [] (NormalB $ AppE (VarE (mkName "error")) $ LitE $ StringL "no events (empty mkEvents output)") []] (null m)
    ]
  where
    mkClause :: (Word16, Element) -> Q Clause
    mkClause (opcode, element) = pure $ Clause [ConP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) [] [WildP | _ <- args]] (NormalB $ LitE $ IntegerL $ fromIntegral opcode) []
      where
        eventName = fromJust $ findAttr (qname "name") element
        args = findChildren (qname "arg") element

mkPut :: (String -> String) -> String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkPut formatter interfaceName prefix prefix2 events =
  ( \m ->
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ AppT (ConT ''WirePut) (TupleT 0))
      , FunD (mkName prefix) $ bool m [Clause [] (NormalB $ AppE (VarE (mkName "error")) $ LitE $ StringL "no events (empty mkEvents output)") []] (null m)
      ]
  )
    <$> traverse mkClause events
  where
    mkClause :: (Word16, Element) -> Q Clause
    mkClause (_opcode, element) = do
      putters <- traverse (\a -> (argCodec formatter interfaceName a).putter) args
      pure
        $ Clause
          [ConP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) [] $ fmap (VarP . mkName . ("arg_" <>)) argNames]
          (NormalB . foldr (\x acc -> InfixE (Just x) (VarE '(>>)) (Just acc)) (AppE (VarE 'pure) (ConE '())) $ zipWith (\p n -> AppE p (VarE $ mkName $ "arg_" <> n)) putters argNames)
          []
      where
        args = findChildren (qname "arg") element
        argNames = fromJust . findAttr (qname "name") <$> args
        eventName = fromJust $ findAttr (qname "name") element

mkParser :: (String -> String) -> String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkParser formatter interfaceName prefix prefix2 events = do
  opcode <- newName "opcode"
  sequence
    [ sigD name [t|Word16 -> WireGet (Either MessageError $(conT $ mkName $ prefix2 <> interfaceName))|]
    , funD name $ fmap mkClause events <> [clause [varP opcode] (normalB [|pure (Left (UnknownOpcode $(varE opcode)))|]) []]
    ]
  where
    name = mkName prefix
    -- Every arg is got, then the first error (if any) is the result:
    -- getMessage <opcode> = do
    --   arg1 <- <get arg 1>
    --   arg2 <- <get arg 2>
    --   pure (pure <Constructor> <*> arg1 <*> arg2) -- inner pure and <*> in Either
    mkClause :: (Word16, Element) -> Q Clause
    mkClause (opcode, element) = do
      vars <- traverse (const $ newName "arg") args
      let binds = zipWith (\v a -> bindS (varP v) (argCodec formatter interfaceName a).getter) vars args
          built = foldl' (\acc v -> [|$acc <*> $(varE v)|]) [|pure $(conE con)|] vars
      clause [litP . integerL $ fromIntegral opcode] (normalB . doE $ binds <> [noBindS [|pure $built|]]) []
      where
        args = findChildren (qname "arg") element
        eventName = fromJust $ findAttr (qname "name") element
        con = mkName $ prefix2 <> interfaceName <> "_" <> eventName

-- | Create message instances for messages of an interface.
mkMessageInstances :: (String -> String) -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkMessageInstances formatter interfaceName prefix2 events = do
  put' <- mkPut formatter interfaceName "putMessage" prefix2 events
  get' <- mkParser formatter interfaceName "getMessage" prefix2 events
  let sender' = [FunD 'sender [Clause [WildP] (NormalB . ConE $ if prefix2 == "Request_" then 'Client else 'Server) []]]
  opc' <- mkOpcodeGetter interfaceName "getOpcode" prefix2 events
  show' <- mkShow interfaceName "showMessage" prefix2 events
  pure [InstanceD Nothing [] (AppT (ConT ''Message) $ ConT . mkName $ prefix2 <> interfaceName) $ put' <> get' <> opc' <> show' <> sender']

-- | Create all definitions for a single interface - the class, parsers, builders, enums, opcodes etc.
loadInterface :: (String -> String) -> Bool -> Element -> Q [Dec]
loadInterface formatter isIO int = do
  let events = findChildren (qname "event") int
  let requests = findChildren (qname "request") int

  ifaceName <-
    if isIO
      then pure . mkName $ formatter name'
      else
        lookupTypeName (formatter name') >>= \case
          Just n -> pure n
          Nothing -> fail $ "sayland: protocol declares interface `" <> name' <> "` but no type `" <> formatter name' <> "` is in scope."

  docDecl isIO ifaceName (elemDoc int)
  -- Checked while compiling to not contain NUL.
  wlName <- quoteExp wl name'

  concat
    <$> sequence
      [ -- WaylandEvent
        mkMessages isIO formatter name' "Request" requests
      , mkMessages isIO formatter name' "Event" events
      , mkMessageInstances formatter name' "Event_" $ zip [0 ..] events
      , mkMessageInstances formatter name' "Request_" $ zip [0 ..] requests
      , -- Interface instance
        pure
          [ InstanceD
              Nothing
              []
              (AppT (ConT ''Interface) ifaceT)
              [ TySynInstD $ TySynEqn Nothing (AppT (ConT ''Event) ifaceT) (ConT $ mkName $ "Event_" <> name')
              , TySynInstD $ TySynEqn Nothing (AppT (ConT ''Request) ifaceT) (ConT $ mkName $ "Request_" <> name')
              , FunD
                  'getInterfaceVersion
                  [Clause [WildP] (NormalB $ AppE (ConE 'WlUInt) (LitE $ IntegerL version')) []]
              , FunD
                  'getInterfaceName
                  [Clause [WildP] (NormalB wlName) []]
              ]
          ]
      ]
  where
    name' = fromJust $ findAttr (qname "name") int
    ifaceT = ConT . mkName $ formatter name'
    version' = read . fromJust $ findAttr (qname "version") int

loadInterfaceEnums :: Bool -> Element -> Q [Dec]
loadInterfaceEnums isIO int =
  concat <$> mapM (mkEnum isIO name) enums
  where
    name = fromJust $ findAttr (qname "name") int
    enums = findChildren (qname "enum") int

-- | Whether an @\<arg\>@ may be null, from its @allow-null@ attribute. Only strings and objects can be.
isNullable :: Element -> Bool
isNullable element = case findAttr (qname "allow-null") element of
  Just "true"
    | findAttr (qname "type") element `elem` [Just "string", Just "object"] -> True
    | otherwise -> error $ "allow-null on an arg that is not a string or object: " <> show element
  _ -> False

-- | Name of the type of an enum argument. @enum="iface.name"@ refers to another interface's enum.
enumTypeName :: String -> String -> String
enumTypeName intName enumName =
  "Enum_" <> case span (/= '.') enumName of
    (a, "") -> intName <> "_" <> a
    (a, _ : b) -> a <> "_" <> b

-- | How an @\<arg\>@ is in Haskell, from the XML: its type, a putter @type -> WirePut ()@,
-- and a getter @WireGet (Either MessageError type)@ that fails on anything the type rules out.
data ArgCodec = ArgCodec {argTy :: Type, putter :: Q Exp, getter :: Q Exp}

-- | The `ArgCodec` of an @\<arg\>@ of the interface with the given name.
argCodec :: (String -> String) -> String -> Element -> ArgCodec
argCodec formatter intName element = case findAttr (qname "enum") element of
  Just enumName -> let n = enumTypeName intName enumName in ArgCodec (ConT $ mkName n) (varE . mkName $ "put" <> n) (varE . mkName $ "get" <> n)
  Nothing -> case (findAttr (qname "type") element, findAttr (qname "interface") element, isNullable element) of
    (Just "new_id", Just x, _) -> ArgCodec (tObject x) [|putObjectID . toObjectID|] [|bimap InvalidObjectID TObjectID <$> getObjectID|]
    (Just "new_id", Nothing, _) -> ArgCodec (ConT ''WlNewId) [|putWlNewId|] [|first InvalidNewId <$> getWlNewId|]
    (Just "int", _, _) -> mayRunOut ''WlInt [|putWlInt|] [|getWlInt|]
    (Just "uint", _, _) -> mayRunOut ''WlUInt [|putWlUInt|] [|getWlUInt|]
    (Just "fixed", _, _) -> mayRunOut ''WlFixed [|putWlFixed|] [|getWlFixed|]
    (Just "array", _, _) -> mayRunOut ''WlArray [|putWlArray|] [|getWlArray|]
    (Just "fd", _, _) -> ArgCodec (ConT ''WlFd) [|putWlFd|] [|maybe (Left MissingFd) Right <$> getWlFd|]
    (Just "string", _, True) -> ArgCodec (ConT ''WlString) [|putWlString|] [|first InvalidString <$> getWlString|]
    (Just "string", _, False) -> ArgCodec (ConT ''WlText) [|putWlString . Just|] [|(>>= maybe (Left NullString) Right) . first InvalidString <$> getWlString|]
    (Just "object", Just x, True) -> ArgCodec (AppT (ConT ''Maybe) $ tObject x) [|putWlObjectID . fmap toObjectID|] [|bimap BodyTooShort (fmap TObjectID) <$> getWlObjectID|]
    (Just "object", Just x, False) -> ArgCodec (tObject x) [|putObjectID . toObjectID|] [|bimap InvalidObjectID TObjectID <$> getObjectID|]
    (Just "object", Nothing, True) -> mayRunOut ''WlObjectID [|putWlObjectID|] [|getWlObjectID|]
    (Just "object", Nothing, False) -> ArgCodec (ConT ''ObjectID) [|putObjectID|] [|first InvalidObjectID <$> getObjectID|]
    _ -> error $ "sayland: unsupported arg " <> show element
  where
    tObject = AppT (ConT ''TObjectID) . ConT . mkName . formatter
    -- A getter that can only run out of bytes.
    mayRunOut ty p g = ArgCodec (ConT ty) p [|first BodyTooShort <$> $g|]

-- vim: foldmethod=marker
