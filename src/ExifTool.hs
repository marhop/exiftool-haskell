{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module     : ExifTool
-- Copyright  : (c) Martin Hoppenheit 2020-2021
-- License    : MIT
-- Maintainer : martin@hoppenheit.info
--
-- This module contains bindings to the [ExifTool](https://exiftool.org)
-- command-line application that enable reading, writing and deleting metadata
-- in various file formats. Here's a short code example, the details are
-- explained below.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Data.Text (Text)
-- > import ExifTool
-- > 
-- > data Foo = Foo
-- >   { description :: Text,
-- >     resolution :: Int
-- >   }
-- >   deriving (Show)
-- > 
-- > main :: IO ()
-- > main = withExifTool $ \et -> do
-- >   m <- readMeta et [] "a.jpg"
-- >   print $ Foo <$> get (Tag "Description") m <*> get (Tag "XResolution") m
-- >   let m' = del (Tag "Description") . set (Tag "XResolution") (42 :: Int) $ m
-- >   writeMeta et m' "a.jpg"
--
-- Note that this module expects the @exiftool@ binary to be in your PATH.

module ExifTool
  ( -- * Running an ExifTool instance
    --
    -- | Most functions in this module interact with an ExifTool instance
    -- i.e., a running ExifTool process represented by the 'ExifTool' data
    -- type. The easiest way to obtain an instance is the 'withExifTool'
    -- function that takes care of starting and stopping the process.
    ExifTool,
    startExifTool,
    stopExifTool,
    withExifTool,
    -- * Reading and writing metadata
    --
    -- | The ExifTool instance can then be used to read or write metadata in a
    -- file with the respective functions.
    readMeta,
    readMetaEither,
    writeMeta,
    writeMetaEither,
    -- | Metadata is represented by a 'Data.HashMap.Strict.HashMap' of
    -- 'Tag'/'Value' pairs (with alias 'Metadata').
    Metadata,
    Tag (..),
    Value (..),
    FromValue (..),
    ToValue (..),
    -- | In general, the usual HashMap functions like
    -- 'Data.HashMap.Strict.lookup' can be used on Metadata. However, the
    -- ExifTool module defines additional utility functions that make working
    -- with Metadata easier.
    get,
    set,
    del,
  )
where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Bifunctor (bimap)
import System.IO (Handle, hFlush, hReady)

import Data.Aeson
  ( FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
    ToJSONKey (..),
    eitherDecode,
    encode,
  )
import qualified Data.Aeson as JSON
import Data.Aeson.Encoding.Internal (bool, list, scientific, text)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.ByteString.Lazy (hPut)
import Data.HashMap.Strict (HashMap, delete, insert, (!?))
import Data.Hashable (Hashable)
import Data.Scientific
  ( FPFormat (Fixed),
    Scientific,
    formatScientific,
    fromFloatDigits,
    isInteger,
    toBoundedInteger,
    toRealFloat,
  )
import Data.String.Conversions (cs)
import Data.Text (Text, intercalate, isPrefixOf, stripPrefix)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO (hGetLine, hPutStrLn)
import qualified Data.Vector as Vector
import System.IO.Temp (withSystemTempFile)
import System.Process
  ( ProcessHandle,
    StdStream (CreatePipe),
    cleanupProcess,
    createProcess,
    proc,
    std_err,
    std_in,
    std_out,
  )

-- | An ExifTool instance, initialized with 'startExifTool' and terminated with
-- 'stopExifTool'.
data ExifTool
  = ET
      -- STDIN of this ExifTool process
      !Handle
      -- STDOUT of this ExifTool process
      !Handle
      -- STDERR of this ExifTool process
      !Handle
      -- process handle of this ExifTool process
      !ProcessHandle

-- | A set of ExifTool tag/value pairs.
type Metadata = HashMap Tag Value

-- | An ExifTool tag name like @Tag "Description"@,
-- @Tag "EXIF:IFD0:XResolution"@ or @Tag "XMP:all"@.
newtype Tag = Tag {tagName :: Text}
  deriving (Show, Eq)
  deriving (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey) via Text

-- | An ExifTool tag value, enclosed in a type wrapper.
data Value
  = String !Text
  | Binary !ByteString
  | Number !Scientific
  | Bool !Bool
  | List ![Value]
  -- Struct (Map Text Value)
  deriving (Show, Eq)

instance FromJSON Value where
  parseJSON (JSON.String x)
    | Just b <- stripPrefix "base64:" x =
      either (fail . cs) (pure . Binary) (decodeBase64 $ cs b)
    | otherwise = pure $ String x
  parseJSON (JSON.Number x) = pure $ Number x
  parseJSON (JSON.Bool x) = pure $ Bool x
  parseJSON (JSON.Array xs) = List . Vector.toList <$> traverse parseJSON xs
  parseJSON JSON.Null = pure $ String ""
  -- parseJSON (JSON.Object x) = Struct <$> sequence (fmap parseJSON x)
  parseJSON x = fail $ "error parsing ExifTool JSON output: " <> show x

instance ToJSON Value where
  toJSON (String x) = JSON.String x
  toJSON (Binary x) = JSON.String $ "base64:" <> encodeBase64 x
  toJSON (Number x) = JSON.Number x
  toJSON (Bool x) = JSON.Bool x
  toJSON (List xs) = JSON.Array . Vector.fromList $ map toJSON xs
  toEncoding (String x) = text x
  toEncoding (Binary x) = text $ "base64:" <> encodeBase64 x
  toEncoding (Number x) = scientific x
  toEncoding (Bool x) = bool x
  toEncoding (List xs) = list toEncoding xs

-- | Data types that a 'Value' can be turned into.
--
-- @since 0.2.0.0
class FromValue a where
  fromValue :: Value -> Maybe a

-- | Data types that can be turned into a 'Value'.
--
-- @since 0.2.0.0
class ToValue a where
  toValue :: a -> Value

instance FromValue Value where
  fromValue = Just

instance ToValue Value where
  toValue = id

instance FromValue Text where
  fromValue (String x) = Just x
  fromValue (Binary x) = either (const Nothing) Just $ decodeUtf8' x
  fromValue (Number x) =
    Just . cs . formatScientific Fixed (Just $ if isInteger x then 0 else 2) $ x
  fromValue (Bool x) = Just . cs . show $ x
  fromValue (List xs) = intercalate ", " <$> traverse fromValue xs

instance ToValue Text where
  toValue = String

instance FromValue ByteString where
  fromValue (Binary x) = Just x
  fromValue _ = Nothing

instance ToValue ByteString where
  toValue = Binary

instance FromValue Int where
  fromValue (Number x) = toBoundedInteger x
  fromValue _ = Nothing

instance ToValue Int where
  toValue = Number . fromIntegral

instance FromValue Integer where
  fromValue x = fromIntegral <$> (fromValue x :: Maybe Int)

instance ToValue Integer where
  toValue = Number . fromIntegral

instance FromValue Float where
  fromValue (Number x) = Just $ toRealFloat x
  fromValue _ = Nothing

instance ToValue Float where
  toValue = Number . fromFloatDigits

instance FromValue Double where
  fromValue (Number x) = Just $ toRealFloat x
  fromValue _ = Nothing

instance ToValue Double where
  toValue = Number . fromFloatDigits

instance FromValue Bool where
  fromValue (Bool x) = Just x
  fromValue _ = Nothing

instance ToValue Bool where
  toValue = Bool

instance FromValue a => FromValue [a] where
  fromValue (List xs) = traverse fromValue xs
  fromValue _ = Nothing

instance ToValue a => ToValue [a] where
  toValue = List . fmap toValue

-- | Start an ExifTool instance. Use 'stopExifTool' when done, or 'withExifTool'
-- to combine both steps.
startExifTool :: IO ExifTool
startExifTool = do
  (Just i, Just o, Just e, p) <- createProcess conf
  pure $ ET i o e p
  where
    conf =
      (proc "exiftool" options)
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe
        }
    options = ["-stay_open", "True", "-@", "-"]

-- | Stop a running ExifTool instance.
stopExifTool :: ExifTool -> IO ()
stopExifTool (ET i o e p) = do
  hPutStrLn i "-stay_open"
  hPutStrLn i "False"
  hFlush i
  cleanupProcess (Just i, Just o, Just e, p)

-- | Start an ExifTool instance, do something with it, then stop it.
withExifTool :: (ExifTool -> IO a) -> IO a
withExifTool = bracket startExifTool stopExifTool

-- | Send a sequence of command-line arguments to a running ExifTool instance
-- and return the corresponding output/errors.
--
-- The final @-execute@ argument is added automatically.
sendCommand :: ExifTool -> [Text] -> IO (Either Text Text)
sendCommand (ET i o e _) cmds = do
  mapM_ (hPutStrLn i) cmds
  hPutStrLn i "-execute"
  hFlush i
  -- Do not switch the order of readOut/readErr lest we miss errors!
  out <- readOut o ""
  err <- readErr e ""
  pure $
    if isError err
      then Left err
      else Right out
  where
    readOut :: Handle -> Text -> IO Text
    readOut h acc = do
      l <- hGetLine h
      if "{ready}" `isPrefixOf` l
        then pure acc
        else readOut h (acc <> l)
    readErr :: Handle -> Text -> IO Text
    readErr h acc = do
      hasMore <- hReady h
      if not hasMore
        then pure acc
        else do
          l <- hGetLine h
          readErr h (acc <> l)
    isError :: Text -> Bool
    isError t = t `notElem` ["", "    1 image files updated"]

-- | Read the given tags from a file, with ExifTool errors leading to runtime
-- errors. (Use 'readMetaEither' instead if you would rather intercept them.)
-- Use an empty tag list to return all metadata.
--
-- @since 0.2.0.0
readMeta :: ExifTool -> [Tag] -> FilePath -> IO Metadata
readMeta et ts fp = eitherError <$> readMetaEither et ts fp

-- | Read the given tags from a file, with ExifTool errors returned as Left
-- values. Use an empty tag list to return all metadata.
--
-- @since 0.2.0.0
readMetaEither :: ExifTool -> [Tag] -> FilePath -> IO (Either Text Metadata)
readMetaEither et ts fp = do
  result <- sendCommand et (cs fp : options <> tags)
  pure $ result >>= parseOutput
  where
    options = ["-json", "-a", "-U", "-s", "-binary"]
    tags = fmap (("-" <>) . tagName) ts
    parseOutput = bimap cs head . eitherDecode . cs

-- | Write metadata to a file, with ExifTool errors leading to runtime errors.
-- (Use 'setMetaEither' instead if you would rather intercept them.) The file is
-- modified in place. Make sure you have the necessary backups!
--
-- @since 0.2.0.0
writeMeta :: ExifTool -> Metadata -> FilePath -> IO ()
writeMeta et m fp = eitherError <$> writeMetaEither et m fp

-- | Write metadata to a file, with ExifTool errors returned as Left values. The
-- file is modified in place. Make sure you have the necessary backups!
--
-- @since 0.2.0.0
writeMetaEither :: ExifTool -> Metadata -> FilePath -> IO (Either Text ())
writeMetaEither et m fp =
  withSystemTempFile "exiftool.json" $ \metafile h -> do
    hPut h $ encode [delete (Tag "SourceFile") m]
    hFlush h
    void <$> sendCommand et (cs fp : "-json=" <> cs metafile : options)
  where
    options = ["-overwrite_original", "-f"]

-- | Retrieve the value of a tag.
--
-- @since 0.2.0.0
get :: FromValue a => Tag -> Metadata -> Maybe a
get t m = (m !? t) >>= fromValue

-- | Set a tag to a (new) value.
--
-- @since 0.2.0.0
set :: ToValue a => Tag -> a -> Metadata -> Metadata
set t v = insert t (toValue v)

-- | Delete a tag (i.e., set its value to a marker that will make ExifTool
-- delete it when 'writeMeta' is called.
--
-- @since 0.2.0.0
del :: Tag -> Metadata -> Metadata
del t = set t (String "-")

-- | Extract content from Right or throw error.
eitherError :: Either Text a -> a
eitherError = either (error . cs) id
