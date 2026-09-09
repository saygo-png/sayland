{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Description : Defines all requests and events that exist and should be implemented. Implementations under `Protocols`
module Protocol (module Protocol) where

import Data.Binary
import Data.Maybe (fromJust)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Relude hiding (Type, get, put)
import Relude.Unsafe qualified as Unsafe
import Sayland.Internal.Utils
import Sayland.Types
import Sayland.Wire.Types
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Show qualified
import Text.XML.Light

type VersionTable = [(String, Word32)]

-- | Generates a VersionTable for the given protocol.
generateVersionTable :: Element -> [Dec]
generateVersionTable e =
  [ SigD name $ ConT ''VersionTable
  , ValD (VarP name) (NormalB $ ListE defs) []
  ]
  where
    protocol = fromJust $ findAttr (qname "name") e
    name = mkName $ protocol <> "VersionTable"
    tuple x = TupE [Just $ VarE $ mkName $ x <> "Name", Just $ VarE $ mkName $ x <> "Version"]
    defs = tuple . fromJust . findAttr (qname "name") <$> findChildren (qname "interface") e

type InterfaceClientTable = [(String, ObjectID -> IO (Interface Client))]

type InterfaceServerTable = [(String, ObjectID -> IO (Interface Server))]

-- | Generates an InterfaceTable, using formatter to format classes names - as they are to be defined by the user.
generateInterfaceTable :: Element -> (String -> String) -> [Dec]
generateInterfaceTable e formatter =
  [ SigD cname $ ConT ''InterfaceClientTable
  , ValD (VarP cname) (NormalB $ ListE defs) []
  , SigD sname $ ConT ''InterfaceServerTable
  , ValD (VarP sname) (NormalB $ ListE defs) []
  ]
  where
    protocol = fromJust $ findAttr (qname "name") e
    cname = mkName $ protocol <> "InterfaceClientTable"
    sname = mkName $ protocol <> "InterfaceServerTable"
    tuple x =
      TupE
        [ Just $ VarE $ mkName $ x <> "Name"
        , Just
            $ LamE [VarP oid]
            $ AppE (AppE (VarE '(<$>)) (ConE 'Interface))
            $ SigE
              (AppE (VarE 'newInterface) (AppE (ConE 'TObjectID) (VarE oid)))
              (AppT (ConT ''IO) (ConT . mkName $ formatter x))
        ]
      where
        oid = mkName "objectId"
    defs = tuple . fromJust . findAttr (qname "name") <$> findChildren (qname "interface") e

{- | @instance NewInterface T where newInterface = pure . T@, for interfaces whose
only field is @wlid@. Yields no declarations when there are further fields: their
initial values are not derivable, so those instances stay hand-written.
-}
deriveNewInterface :: Name -> Q [Dec]
deriveNewInterface ty = do
  (cn, fields) <- soleRecordCon ty
  case fields of
    [("wlid", _)] ->
      pure
        [ InstanceD
            Nothing
            []
            (AppT (ConT ''NewInterface) (ConT ty))
            [ FunD
                'newInterface
                [Clause [] (NormalB $ InfixE (Just $ VarE 'pure) (VarE '(.)) (Just $ ConE cn)) []]
            ]
        ]
    fs
      | "wlid" `notElem` fmap fst fs -> fail $ "sayland: " <> nameBase ty <> " has no `wlid` field"
      | otherwise -> pure []
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

-- TemplateHaskell Utils {{{

-- | Returns a declaration of the `Function`s opcode as an integer variable.
mkOpcode :: String -> String -> Word16 -> [Dec]
mkOpcode interfaceName fname opcode =
  [ SigD (mkName $ interfaceName <> "_" <> fname <> "Opcode") (ConT ''Word16)
  , FunD (mkName $ interfaceName <> "_" <> fname <> "Opcode") [Clause [] (NormalB $ LitE $ IntegerL $ fromIntegral opcode) []]
  ]

{- | Defines an enum-like along with a function to look up the value of each element.
example output:
data EnumName = A | B | C | D ... deriving Eq
enumName' A = 1 ...
-}
mkEnum :: String -> String -> [(String, Int)] -> [Dec]
mkEnum interfaceName enumName enumKV =
  [ DataD [] (mkName enumName') [] Nothing constructors [DerivClause (Just StockStrategy) [ConT ''Eq, ConT ''Ord]]
  , InstanceD
      Nothing
      []
      (AppT (ConT ''WireFormat) $ ConT $ mkName enumName')
      [ FunD 'wirePut clauses
      , FunD 'wireGet clauses'
      ]
  , InstanceD
      Nothing
      []
      (AppT (ConT ''Show) $ ConT $ mkName enumName')
      [FunD 'Text.Show.showsPrec show_clauses]
  ]
  where
    enumName' = "Enum_" <> interfaceName <> "_" <> enumName
    enumName'' = enumName' <> "_"
    constructors = (`NormalC` []) . mkName . (enumName'' <>) <$> fmap fst enumKV
    clauses = [Clause [ConP (mkName $ enumName'' <> k) [] []] (NormalB (AppE (VarE 'wirePut) $ AppE (ConE 'WlUInt) $ LitE (IntegerL (fromIntegral v)))) [] | (k, v) <- enumKV]

    clauses' =
      [Clause [] (NormalB . DoE Nothing $ [BindS (VarP $ mkName "variant") getUInt, NoBindS $ CaseE (VarE $ mkName "variant") matches]) []]
    getUInt = SigE (VarE 'wireGet) (AppT (ConT ''WireGet) (ConT ''WlUInt))
    matches = [Match (LitP (IntegerL (fromIntegral v))) (NormalB (AppE (VarE 'pure) (ConE (mkName $ enumName'' <> k)))) [] | (k, v) <- enumKV]

    show_clauses =
      [ Clause
          [WildP, ConP (mkName $ enumName'' <> k) [] []]
          (NormalB $ AppE (VarE 'Text.Show.showString) (LitE (StringL k)))
          []
      | (k, _) <- enumKV
      ]

-- }}}

{- | Loads all .xml files in `path` as protocols.
Set `isIO` to True only when running the function within an IO monad. This should be used *only* for debugging purposes.
`monad` defines the monad in which all events and requests operate in.
-}
loadProtocols :: (String -> String) -> Bool -> FilePath -> Q [Dec]
loadProtocols formatter isIO path = do
  protocol_files <- filter ((== ".xml") . takeExtension) <$> runIO (listDirectory path)
  concat <$> mapM (loadProtocolFile formatter isIO . (path </>)) protocol_files

findInterfaces :: Element -> [Element]
findInterfaces = findChildren (qname "interface")

-- | Load a protocol from the specified `path`. Arguments have the same meaning as in `loadProtocols`.
loadProtocolFile :: (String -> String) -> Bool -> FilePath -> Q [Dec]
loadProtocolFile formatter isIO path = do
  unless isIO $ addDependentFile path
  protocols <- filter ((== qname "protocol") . elName) . onlyElems . parseXML <$> runIO (readFileBS path)
  concat
    <$> mapM
      ((<&> concat) . mapM (loadInterface formatter isIO) . findInterfaces)
      protocols

loadProtocolFileEnums :: Bool -> FilePath -> Q [Dec]
loadProtocolFileEnums isIO path = do
  unless isIO $ addDependentFile path
  protocols <- filter ((== qname "protocol") . elName) . onlyElems . parseXML <$> runIO (readFileBS path)
  pure $ concat $ concatMap (fmap loadInterfaceEnums . findInterfaces) protocols

generateTables :: Bool -> (String -> String) -> FilePath -> Q [Dec]
generateTables isIO formatter path = do
  unless isIO $ addDependentFile path
  protocols <- filter ((== qname "protocol") . elName) . onlyElems . parseXML <$> runIO (readFileBS path)
  pure
    $ concatMap (`generateInterfaceTable` formatter) protocols
    <> concatMap generateVersionTable protocols

mkEvents :: (String -> String) -> String -> String -> [Element] -> [Dec]
mkEvents formatter interfaceName prefix events = [DataD [] (mkName prefix') [] Nothing constructors []]
  where
    prefix' = prefix <> "_" <> interfaceName
    buildBang x = (Bang NoSourceUnpackedness NoSourceStrictness, argType formatter interfaceName x)
    buildRecord x = NormalC (mkName $ prefix' <> "_" <> fromJust (findAttr (qname "name") x)) $ buildBang <$> findChildren (qname "arg") x
    constructors = fmap buildRecord events

mkShow :: String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkShow interfaceName prefix prefix2 events =
  mapM (pure . mkShowC) (fmap snd events) <&> \m ->
    bool
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''ObjectID) $ AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''String)
      , FunD (mkName prefix) m
      ]
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''ObjectID) $ AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''String)
      , FunD (mkName prefix) [Clause [] (NormalB $ AppE (VarE (mkName "error")) $ LitE $ StringL "no events (empty mkEvents output)") []]
      ]
      (null m)
  where
    arrow = case prefix2 of
      "Request_" -> ""
      "Event_" -> ""
      _ -> "??? "
    mkShowC :: Element -> Clause
    mkShowC e = Clause [VarP $ mkName "oid", ConP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) [] $ fmap (VarP . addBoundPrefix) args] (NormalB $ chainShow (reverse args)) []
      where
        single x = AppE (AppE (VarE '(<>)) $ LitE $ StringL $ " " <> nameBase x <> ": ") $ AppE (VarE 'show) $ VarE $ addBoundPrefix x
        chainShow [] =
          AppE (AppE (VarE '(<>)) $ LitE $ StringL $ mconcat [arrow, interfaceName, "@"])
            $ AppE (AppE (VarE '(<>)) (AppE (VarE 'show) $ VarE (mkName "oid"))) (LitE $ StringL $ mconcat [".", eventName])
        chainShow [x] =
          AppE
            ( AppE (VarE '(<>))
                $ AppE (AppE (VarE '(<>)) $ LitE $ StringL $ mconcat [arrow, interfaceName, "@"])
                $ AppE (AppE (VarE '(<>)) (AppE (VarE 'show) $ VarE (mkName "oid"))) (LitE $ StringL $ mconcat [".", eventName, ": "])
            )
            $ single x
        chainShow (x : xs) = InfixE (Just $ chainShow xs) (VarE '(<>)) (Just $ single x)
        args = mkName . fromJust . findAttr (qname "name") <$> findChildren (qname "arg") e
        eventName = fromJust $ findAttr (qname "name") e
        addBoundPrefix x = mkName $ "bound_" <> nameBase x

mkOpcodeGetter :: String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkOpcodeGetter interfaceName prefix prefix2 events =
  mapM mkClause events <&> \m ->
    bool
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''Word16)
      , FunD (mkName prefix) m
      ]
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''Word16)
      , FunD (mkName prefix) [Clause [] (NormalB $ AppE (VarE (mkName "error")) $ LitE $ StringL "no events (empty mkEvents output)") []]
      ]
      (null m)
  where
    mkClause :: (Word16, Element) -> Q Clause
    mkClause (opcode, element) = pure $ Clause [ConP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) [] [WildP | _ <- args]] (NormalB $ LitE $ IntegerL $ fromIntegral opcode) []
      where
        eventName = fromJust $ findAttr (qname "name") element
        args = findChildren (qname "arg") element

mkPut :: String -> String -> String -> [(Word16, Element)] -> [Dec]
mkPut interfaceName prefix prefix2 events =
  ( \m ->
      bool
        [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ AppT (ConT ''WirePut) (TupleT 0))
        , FunD (mkName prefix) m
        ]
        [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ AppT (ConT ''WirePut) (TupleT 0))
        , FunD (mkName prefix) [Clause [] (NormalB $ AppE (VarE (mkName "error")) $ LitE $ StringL "no events (empty mkEvents output)") []]
        ]
        (null m)
  )
    $ mkClause
    <$> events
  where
    nestPutters [] = AppE (VarE 'pure) $ ConE '()
    nestPutters [x] = x
    nestPutters (x : xs) = InfixE (Just $ nestPutters xs) (VarE '(>>)) (Just x)
    mkClause :: (Word16, Element) -> Clause
    mkClause (_opcode, element) =
      Clause
        [ConP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) [] $ fmap (VarP . mkName . ("arg_" <>)) argNames]
        (NormalB $ nestPutters $ reverse $ (\n -> AppE (VarE 'wirePut) (VarE $ mkName $ "arg_" <> n)) <$> argNames)
        []
      where
        args = findChildren (qname "arg") element
        argNames = fromJust . findAttr (qname "name") <$> args
        eventName = fromJust $ findAttr (qname "name") element

mkParser :: String -> String -> String -> [(Word16, Element)] -> [Dec]
mkParser interfaceName prefix prefix2 events =
  ( \m ->
      bool
        [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''Word16) $ AppT (ConT ''WireGet) $ ConT $ mkName $ prefix2 <> interfaceName)
        , FunD (mkName prefix) m
        ]
        [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''Word16) $ AppT (ConT ''WireGet) $ ConT $ mkName $ prefix2 <> interfaceName)
        , FunD (mkName prefix) [Clause [] (NormalB $ AppE (VarE (mkName "error")) $ LitE $ StringL "no events (empty mkEvents output)") []]
        ]
        (null m)
  )
    $ mkClause
    <$> events
  where
    mkClause :: (Word16, Element) -> Clause
    mkClause (opcode, element) =
      Clause
        [LitP $ IntegerL $ fromIntegral opcode]
        (NormalB $ nestGetters $ reverse $ ConE (mkName $ prefix2 <> interfaceName <> "_" <> eventName) : (VarE 'wireGet <$ args))
        []
      where
        args = findChildren (qname "arg") element
        eventName = fromJust $ findAttr (qname "name") element

    nestGetters [] = undefined
    nestGetters [x] = AppE (VarE 'pure) x
    nestGetters [x, y] = InfixE (Just y) (VarE '(<$>)) (Just x)
    nestGetters (x : xs) = InfixE (Just $ nestGetters xs) (VarE '(<*>)) (Just x)

mkWlEvent :: String -> String -> [(Word16, Element)] -> Q [Dec]
mkWlEvent interfaceName prefix2 events = do
  let put' = mkPut interfaceName "putEvent" prefix2 events
      get' = mkParser interfaceName "getEvent" prefix2 events
  opc' <- mkOpcodeGetter interfaceName "getOpcode" prefix2 events
  show' <- mkShow interfaceName "showEvent" prefix2 events
  pure [InstanceD Nothing [] (AppT (ConT ''WaylandEvent) $ ConT . mkName $ prefix2 <> interfaceName) $ put' <> get' <> opc' <> show']

-- | Create all definitions for a single interface - version, the class, parsers, builders, enums, opcodes,
loadInterface :: (String -> String) -> Bool -> Element -> Q [Dec]
loadInterface formatter isIO int = do
  let events = findChildren (qname "event") int
  let requests = findChildren (qname "request") int
  let opcodes = concatMap (\(x, y) -> mkOpcode name' (fromJust $ findAttr (qname "name") y) x) $ zip [1 ..] $ findChildren (qname "event") int

  ifaceName <-
    if isIO
      then pure . mkName $ formatter name'
      else
        lookupTypeName (formatter name') >>= \case
          Just n -> pure n
          Nothing -> fail $ "sayland: protocol declares interface `" <> name' <> "` but no type `" <> formatter name' <> "` is in scope."
  newInterfaceInstance <- if isIO then pure [] else deriveNewInterface ifaceName

  concat
    <$> sequence
      [ -- WaylandEvent
        pure $ mkEvents formatter name' "Request" requests
      , pure $ mkEvents formatter name' "Event" events
      , mkWlEvent name' "Event_" $ zip [0 ..] events
      , mkWlEvent name' "Request_" $ zip [0 ..] requests
      , pure
          [ -- Version
            SigD (mkName $ name' <> "Version") $ ConT ''Word32
          , ValD (VarP verName) (NormalB . LitE . IntegerL $ version') []
          , -- Name
            SigD (mkName $ name' <> "Name") $ ConT ''String
          , ValD (VarP nameName) (NormalB . LitE . StringL $ name') []
          ]
      , -- IsInterface instance
        pure
          [ InstanceD
              Nothing
              []
              (AppT (ConT ''IsInterface) ifaceT)
              [ TySynInstD $ TySynEqn Nothing (AppT (ConT ''Event) ifaceT) (ConT $ mkName $ "Event_" <> name')
              , TySynInstD $ TySynEqn Nothing (AppT (ConT ''Request) ifaceT) (ConT $ mkName $ "Request_" <> name')
              ]
          ]
      , pure newInterfaceInstance
      , -- Opcodes
        pure opcodes
      ]
  where
    name' = fromJust $ findAttr (qname "name") int
    ifaceT = ConT . mkName $ formatter name'
    verName = mkName $ name' <> "Version"
    nameName = mkName $ name' <> "Name"
    version' = Unsafe.read . fromJust $ findAttr (qname "version") int

loadInterfaceEnums :: Element -> [Dec]
loadInterfaceEnums int = concatMap (uncurry $ mkEnum name') enums'
  where
    name' = fromJust $ findAttr (qname "name") int
    enums' = loadEnum <$> findChildren (qname "enum") int

-- | Load enum data from XML spec.
loadEnum :: Element -> (String, [(String, Int)])
loadEnum e' = (fromJust $ findAttr (qname "name") e', f <$> findChildren (qname "entry") e')
  where
    f e = (fromJust $ findAttr (qname "name") e, Unsafe.read $ fromJust $ findAttr (qname "value") e)

argType :: (String -> String) -> String -> Element -> Type
argType formatter intName element = case findAttr (qname "enum") element of
  Just enumName ->
    ConT
      $ mkName
      $ "Enum_"
      <> case span (/= '.') enumName of
        (a, "") -> intName <> "_" <> a
        (a, _ : b) -> a <> "_" <> b
  Nothing -> case findAttr (qname "type") element of
    Nothing -> error $ "arg without a type discovered" <> show element
    Just "new_id" -> case findAttr (qname "interface") element of
      Just x -> AppT (ConT ''TObjectID) . ConT . mkName $ formatter x
      Nothing -> ConT ''WlNewId
    Just "int" -> ConT ''WlInt
    Just "uint" -> ConT ''WlUInt
    Just "fixed" -> ConT ''WlFixed
    Just "string" -> ConT ''WlString
    Just "array" -> ConT ''WlArray
    Just "fd" -> ConT ''WlFd
    Just "object" -> case findAttr (qname "interface") element of
      Just x -> AppT (ConT ''TObjectID) . ConT . mkName $ formatter x
      Nothing -> ConT ''ObjectID
    Just y -> error $ "unknown type: " <> fromString y

-- }}}

-- vim: foldmethod=marker
