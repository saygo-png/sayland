{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Protocol (module Protocol) where

-- This module's purpose is to define all requests and events that exist and should be implemented. Implementing them is handled in `Protocols/`

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put (putByteString, putInt32le, putWord32le)
import Data.ByteString qualified as BS
import Data.Maybe (fromJust)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Relude hiding (Type, get, put)
import Relude.Unsafe qualified as Unsafe
import Saywayland.Types
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import System.Posix (Fd)
import Text.Show qualified
import Text.XML.Light
import Control.Concurrent.STM (readTQueue)
import Data.Traversable (for)
import Relude.Extra (Elem)

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

type InterfaceClientTable = [(String, IO (Interface Client))]

type InterfaceServerTable = [(String, IO (Interface Server))]

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
    tuple x = TupE [Just $ VarE $ mkName $ x <> "Name", Just $ AppE (AppE (VarE $ mkName "<$>") $ ConE 'Interface) $ SigE (VarE $ mkName "defM") (AppT (ConT ''IO) $ ConT $ mkName $ formatter x)]
    defs = tuple . fromJust . findAttr (qname "name") <$> findChildren (qname "interface") e

-- Getters/Putters {{{

-- | Get a Wire-encoded String.
getString :: Get BS.ByteString
getString = do
  len <- getWord32le
  str <- getByteString $ fromIntegral len
  let padding = (4 - (len `mod` 4)) `mod` 4
  _ <- getByteString $ fromIntegral padding
  pure str

-- | Put a Wire-encoded String
putString :: BS.ByteString -> Put
putString s' =
  putWord32le (fromIntegral $ BS.length s)
    >> putByteString s
    >> putByteString (BS.replicate padding 0)
  where
    s = s' <> BS.pack [0]
    padding = (4 - BS.length s `mod` 4) `mod` 4

-- | Get a Wire-encoded Fixed
getFixed24_8 :: Get Double
getFixed24_8 = getInt32le <&> (/ 256.0) . fromIntegral

-- | Put a Wire-encoded Fixed
putFixed24_8 :: Double -> Put
putFixed24_8 d = putInt32le $ fromIntegral @Integer $ round $ d * 256

-- | Get an Fd from previously obtained ancillary data
getFd :: AdditionalParserData -> IO (Get Fd)
getFd dat = pure <$> atomically (readTQueue dat.fdqueue)


-- todo: the following 2 can and should be replaced by Binary instances (?)

-- | Return a TH getter expression for a given Type.
getForType :: Type -> Q Exp
getForType t = case t of
  ConT name
    | name == ''Int -> [|const (pure $ fromIntegral <$> getWord32le)|]
    | name == ''Word32 -> [|const (pure getWord32le)|]
    | name == ''BS.ByteString -> [|const (pure getString)|]
    | name == ''Double -> [|const (pure getFixed24_8)|]
    | name == ''ObjectID -> [|const (pure getWord32le)|]
    | name == ''NewID ->
        [|
          ( const
              $ pure
                ( do
                    strname <- getString
                    name' <- getWord32le
                    id' <- getWord32le
                    pure (strname, name', id')
                )
          )
          |]
    | name == ''Fd -> [|getFd|]
    | otherwise -> [|const (pure get)|]
  AppT (ConT name) _
    | name == ''TObjectID -> [|const (pure $ TObjectID <$> getWord32le)|]
    | otherwise -> [|(const put)|]
  _ -> error $ "[getForType] unsupported type: " <> show t

-- | Return a TH putter expression for a given Type.
putForType :: Type -> Q Exp
putForType t = case t of
  ConT name
    | name == ''Int -> [|(const $ putWord32le . fromIntegral)|]
    | name == ''Word32 -> [|(const putWord32le)|]
    | name == ''BS.ByteString -> [|(const putString)|]
    | name == ''Double -> [|(const putFixed24_8)|]
    | name == ''ObjectID -> [|(const putWord32le)|]
    | name == ''NewID -> [|(const (\(x, y, z) -> putString x >> putWord32le y >> putWord32le z))|]
    | name == ''Fd -> [|const (const pass)|]
    | otherwise -> [|(const put)|]
  AppT (ConT name) _
    | name == ''TObjectID -> [|const $ \(TObjectID x) -> putWord32le x|]
    | otherwise -> [|(const put)|]
  _ -> error $ "[putForType] unsupported type: " <> show t

-- }}}

-- Utils {{{
qname :: String -> QName
qname x = QName x Nothing Nothing

-- }}}

-- TemplateHaskell Utils {{{
adata :: Name
adata = mkName "_additionalData"

-- | Defines an integer variable with name `name` and value `x`.
mkIntVariable :: String -> Integer -> [Dec]
mkIntVariable name x =
  [ SigD (mkName name) (ConT ''Int)
  , ValD (VarP (mkName name)) (NormalB (LitE $ IntegerL x)) []
  ]

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
      (AppT (ConT ''Binary) $ ConT $ mkName enumName')
      [ FunD 'put clauses
      , FunD 'get clauses'
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
    clauses = [Clause [ConP (mkName $ enumName'' <> k) [] []] (NormalB (AppE (VarE 'putWord32le) $ LitE (IntegerL (fromIntegral v)))) [] | (k, v) <- enumKV]

    clauses' =
      [Clause [] (NormalB . DoE Nothing $ [BindS (VarP $ mkName "variant") $ VarE 'getWord32le, NoBindS $ CaseE (VarE $ mkName "variant") matches]) []]
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
      ((<&> concat) . mapM (loadInterface formatter) . findInterfaces)
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
mkEvents formatter interfaceName prefix events = DataD [] (mkName prefix') [] Nothing constructors []:datatypes
  where
    prefix' = prefix <> "_" <> interfaceName
    buildBang x = (mkName . fromJust $ findAttr (qname "name") x, Bang NoSourceUnpackedness NoSourceStrictness, argType formatter interfaceName x)
    buildRecord x = RecC (mkName $ prefix' <> "_" <> fromJust (findAttr (qname "name") x)) $ buildBang <$> findChildren (qname "arg") x
    buildDT x = DataD [] (mkName name) [] Nothing [buildRecord x] []
      where
        name = prefix' <> "_" <> fromJust (findAttr (qname "name") x)
    datatypes = fmap buildDT events
    buildCon x = NormalC (mkName $ name <> "'") [(Bang NoSourceUnpackedness NoSourceStrictness, ConT $ mkName name)]
      where
        name = prefix' <> "_" <> fromJust (findAttr (qname "name") x)
    constructors = fmap buildCon events

mkShow :: String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkShow interfaceName prefix prefix2 events =
  mapM (pure . mkShowC) (fmap snd events) <&> \m ->
    bool
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''ObjectID) $ AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''String)
      , FunD (mkName prefix) m
      ]
      []
      (null m)
  where
    arrow = case prefix2 of
      "Request_" -> "        -> "
      "Event_" -> "        <- "
      _ -> "        ?? "
    mkShowC :: Element -> Clause
    mkShowC e = Clause [VarP $ mkName "oid", ConP (mkName $ prefix2 <> interfaceName <> "_" <> eventName <> "'") [] [RecP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) $ fmap (\x -> (x, VarP $ addBoundPrefix x)) args]] (NormalB $ chainShow (reverse args)) []
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
      []
      (null m)
  where
    mkClause :: (Word16, Element) -> Q Clause
    mkClause (opcode, element) = pure $ Clause [ConP (mkName $  prefix2 <> interfaceName <> "_" <> eventName <> "'") [] [RecP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) []]] (NormalB $ LitE $ IntegerL $ fromIntegral opcode) []
      where
        eventName = fromJust $ findAttr (qname "name") element

mkPut :: (String -> String) -> String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkPut formatter interfaceName prefix prefix2 events =
  mapM mkClause events <&> \m ->
    bool
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''AdditionalParserData) $ AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''Put)
      , FunD (mkName prefix) m
      ]
      []
      (null m)
  where
    nestPutters [] = AppE (VarE 'pure) $ ConE '()
    nestPutters [x] = x
    nestPutters (x : xs) = InfixE (Just $ nestPutters xs) (VarE '(>>)) (Just x)
    mkClause :: (Word16, Element) -> Q Clause
    mkClause (_opcode, element) =
      mapM (\(a, b) -> putForType b <&> (`AppE` (GetFieldE (VarE $ mkName "_event") $ fromJust $ findAttr (qname "name") a)) . (`AppE` VarE adata)) (zip args argTypes)
        <&> \x ->
          ( Clause
              [ VarP adata
              , ConP (mkName $ prefix2 <> interfaceName <> "_" <> eventName <> "'") [] [AsP (mkName "_event") $ RecP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) []]
              ]
              $ NormalB
              $ nestPutters (reverse x)
          )
            []
      where
        args = findChildren (qname "arg") element
        argTypes = fmap (argType formatter interfaceName) args
        eventName = fromJust $ findAttr (qname "name") element

mkParser :: (String -> String) -> String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkParser formatter interfaceName prefix prefix2 events =
  mapM mkClause events <&> \m ->
    bool
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''Word16) $ AppT (AppT ArrowT $ ConT ''AdditionalParserData) (AppT (ConT ''IO) $ AppT (ConT ''Get) $ ConT $ mkName $ prefix2 <> interfaceName))
      , FunD (mkName prefix) m
      ]
      []
      (null m)
  where
    mkClause :: (Word16, Element) -> Q Clause
    mkClause (opcode, element) =
      mapM getForType argTypes <&> \getters ->
        Clause
          [LitP $ IntegerL $ fromIntegral opcode, VarP adata]
          ( NormalB
              $ DoE Nothing
              $ [BindS (VarP $ mkBinding a) (AppE getter $ VarE adata) | (a, getter) <- zip args getters]
              <> [NoBindS $ AppE (VarE 'pure) $ AppE (AppE (VarE '(<$>)) $ ConE $ mkName $ prefix2 <> interfaceName <> "_" <> eventName <> "'") $ nestGetters $ reverse $ ConE (mkName $ prefix2 <> interfaceName <> "_" <> eventName) : fmap mkexpr args]
          )
          []
      where
        args = findChildren (qname "arg") element
        argTypes = fmap (argType formatter interfaceName) args
        eventName = fromJust $ findAttr (qname "name") element

        -- Adding "bound_" prefix to avoid shadowing with names such as "id".
        mkBinding x = mkName $ "bound_" <> fromJust (findAttr (qname "name") x)
        mkexpr = VarE . mkBinding

    nestGetters [] = undefined
    nestGetters [x] = AppE (VarE 'pure) x
    nestGetters [x, y] = InfixE (Just y) (VarE '(<$>)) (Just x)
    nestGetters (x : xs) = InfixE (Just $ nestGetters xs) (VarE '(<*>)) (Just x)

mkWLEvent :: (String -> String) -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkWLEvent formatter interfaceName prefix2 events = do
  put' <- mkPut formatter interfaceName "putEvent" prefix2 events
  get' <- mkParser formatter interfaceName "getEvent" prefix2 events
  opc' <- mkOpcodeGetter interfaceName "getOpcode" prefix2 events
  show' <- mkShow interfaceName "showEvent" prefix2 events
  pure [InstanceD Nothing [] (AppT (ConT ''WaylandEvent) $ ConT . mkName $ prefix2 <> interfaceName) $ put' <> get' <> opc' <> show']

-- | Create all definitions for a single interface - version, the class, parsers, builders, enums, opcodes,
loadInterface :: (String -> String) -> Element -> Q [Dec]
loadInterface formatter int = do
  let events = findChildren (qname "event") int
  let requests = findChildren (qname "request") int
  let opcodes = concatMap (\(x, y) -> mkOpcode name' (fromJust $ findAttr (qname "name") y) x) $ zip [1 ..] $ findChildren (qname "event") int

  concat
    <$> sequence
      [ -- WaylandEvent
        pure $ mkEvents formatter name' "Request" requests
      , pure $ mkEvents formatter name' "Event" events
      , mkWLEvent formatter name' "Event_" $ zip [0 ..] events
      , mkWLEvent formatter name' "Request_" $ zip [0 ..] requests
      , pure
          [ -- Version
            SigD (mkName $ name' <> "Version") $ ConT ''Word32
          , ValD (VarP verName) (NormalB . LitE . IntegerL $ version') []
          , -- Name
            SigD (mkName $ name' <> "Name") $ ConT ''String
          , ValD (VarP nameName) (NormalB . LitE . StringL $ name') []
          -- Class definition
          ]
      , -- Opcodes
        pure opcodes
      ]
  where
    name' = fromJust $ findAttr (qname "name") int
    verName = mkName $ name' <> "Version"
    nameName = mkName $ name' <> "Name"
    version' = Unsafe.read . fromJust $ findAttr (qname "version") int

loadInterfaceEnums :: Element -> [Dec]
loadInterfaceEnums int = concatMap (uncurry $ mkEnum name') enums'
  where
    name' =  fromJust $ findAttr (qname "name") int
    enums' = loadEnum <$> findChildren (qname "enum") int

-- | Load enum data from XML spec.
loadEnum :: Element -> (String, [(String, Int)])
loadEnum e' = (fromJust $ findAttr (qname "name") e', f <$> findChildren (qname "entry") e')
  where
    f e = (fromJust $ findAttr (qname "name") e, Unsafe.read $ fromJust $ findAttr (qname "value") e)

argType :: (String -> String) -> String -> Element -> Type
argType formatter intName x = case findAttr (qname "enum") x of
  Just x' ->
    ConT
      $ mkName
      $ "Enum_"
      <> case span (/= '.') x' of
        (a, "") -> intName <> "_" <> a
        (a, _ : b) -> a <> "_" <> b
  Nothing -> case findAttr (qname "type") x of
    Nothing -> error $ "arg without a type discovered" <> show x
    Just "new_id" -> case findAttr (qname "interface") x of
      Just _ -> ConT ''ObjectID
      Nothing -> ConT ''NewID
    Just "int" -> ConT ''Int
    Just "uint" -> ConT ''Word32
    Just "fixed" -> ConT ''Double
    Just "string" -> ConT ''BS.ByteString
    Just "object" -> case findAttr (qname "interface") x of
      Just x -> AppT (ConT ''TObjectID) . ConT . mkName $ formatter x
      Nothing-> ConT ''ObjectID
    Just "array" -> ConT ''BS.ByteString
    Just "fd" -> ConT ''Fd
    Just y -> error $ "unknown type: " <> fromString y

-- }}}

-- vim: foldmethod=marker
