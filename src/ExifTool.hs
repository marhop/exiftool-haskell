{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module     : ExifTool
-- Copyright  : (c) Martin Hoppenheit 2021
-- License    : MIT
-- Maintainer : martin@hoppenheit.info
--
-- This module contains bindings to the [ExifTool](https://exiftool.org)
-- command-line application that enable reading, writing and deleting metadata
-- in various file formats. Here's a short code example, the details are
-- explained below.
--
-- > {-# LANGUAGE OverloadedLists #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Data.HashMap.Strict ((!?))
-- > import ExifTool
-- > 
-- > example :: IO ()
-- > example =
-- >   withExifTool $ \et -> do
-- >     -- Read metadata, with exact (!?) and fuzzy (~~) tag lookup.
-- >     m <- getMeta et "a.jpg"
-- >     print $ m !? Tag "EXIF" "ExifIFD" "DateTimeOriginal"
-- >     print $ m ~~ Tag "EXIF" "" "XResolution"
-- >     print $ m ~~ Tag "XMP" "" ""
-- >     -- Write and delete metadata.
-- >     setMeta et [(Tag "XMP" "XMP-dc" "Description", String "...")] "a.jpg"
-- >     deleteMeta et [Tag "XMP" "XMP-dc" "Description"] "a.jpg"
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
    -- | The ExifTool instance can then be used to read, write or delete
    -- metadata in a file with the respective functions. These come in two
    -- variants, one that throws runtime errors when the ExifTool process
    -- returns error messages and one that instead produces Either values.
    -- Choose those that best fit your use case.
    getMeta,
    setMeta,
    deleteMeta,
    getMetaEither,
    setMetaEither,
    deleteMetaEither,
    -- * Data types and utility functions
    --
    -- | Metadata is represented by a 'Data.HashMap.Strict.HashMap' of
    -- 'Tag'/'Value' pairs (with alias 'Metadata'), so it is advisable to
    -- import some functions like 'Data.HashMap.Strict.lookup' or
    -- 'Data.HashMap.Strict.!?' from the "Data.HashMap.Strict" module. The
    -- ExifTool module defines additional utility functions that make working
    -- with Metadata easier.
    Metadata,
    Tag (..),
    Value (..),
    filterByTag,
    (~~),
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
import Data.HashMap.Strict (HashMap, delete, filterWithKey, fromList)
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import Data.String.Conversions (cs)
import Data.Text (Text, isPrefixOf, stripPrefix, toCaseFold)
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
newtype Tag = Tag Text
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
      either (fail . cs) (return . Binary) (decodeBase64 $ cs b)
    | otherwise = return $ String x
  parseJSON (JSON.Number x) = return $ Number x
  parseJSON (JSON.Bool x) = return $ Bool x
  parseJSON (JSON.Array xs) =
    List <$> sequence (Vector.toList $ fmap parseJSON xs)
  parseJSON JSON.Null = return $ String ""
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

-- | Start an ExifTool instance. Use 'stopExifTool' when done, or 'withExifTool'
-- to combine both steps.
startExifTool :: IO ExifTool
startExifTool = do
  (Just i, Just o, Just e, p) <- createProcess conf
  return $ ET i o e p
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
  return $
    if isError err
      then Left err
      else Right out
  where
    readOut :: Handle -> Text -> IO Text
    readOut h acc = do
      l <- hGetLine h
      if "{ready}" `isPrefixOf` l
        then return acc
        else readOut h (acc <> l)
    readErr :: Handle -> Text -> IO Text
    readErr h acc = do
      hasMore <- hReady h
      if not hasMore
        then return acc
        else do
          l <- hGetLine h
          readErr h (acc <> l)
    isError :: Text -> Bool
    isError t = t `notElem` ["", "    1 image files updated"]

-- | Read all metadata from a file, with ExifTool errors leading to runtime
-- errors. (Use 'getMetaEither' instead if you would rather intercept them.)
getMeta ::
  -- | ExifTool instance
  ExifTool ->
  -- | file name
  Text ->
  -- | tag/value Map
  IO Metadata
getMeta et file = eitherError <$> getMetaEither et file

-- | Read all metadata from a file, with ExifTool errors returned as Left
-- values.
getMetaEither ::
  -- | ExifTool instance
  ExifTool ->
  -- | file name
  Text ->
  -- | tag/value Map
  IO (Either Text Metadata)
getMetaEither et file = do
  result <- sendCommand et (file : options)
  return $ result >>= parseOutput
  where
    parseOutput :: Text -> Either Text Metadata
    parseOutput = bimap cs head . eitherDecode . cs
    options = ["-json", "-a", "-U", "-G:0:1", "-s", "-binary"]

-- | Write metadata to a file, with ExifTool errors leading to runtime errors.
-- (Use 'setMetaEither' instead if you would rather intercept them.) The file is
-- modified in place. Make sure you have the necessary backups!
setMeta ::
  -- | ExifTool instance
  ExifTool ->
  -- | tag/value Map
  Metadata ->
  -- | file name
  Text ->
  IO ()
setMeta et m file = eitherError <$> setMetaEither et m file

-- | Write metadata to a file, with ExifTool errors returned as Left values. The
-- file is modified in place. Make sure you have the necessary backups!
setMetaEither ::
  -- | ExifTool instance
  ExifTool ->
  -- | tag/value Map
  Metadata ->
  -- | file name
  Text ->
  IO (Either Text ())
setMetaEither et m file =
  withSystemTempFile "exiftool.json" $ \metafile h -> do
    hPut h $ encode [delete (Tag "SourceFile") m]
    hFlush h
    void <$> sendCommand et (file : "-json=" <> cs metafile : options)
  where
    options = ["-overwrite_original", "-f"]

-- | Delete metadata from a file, with ExifTool errors leading to runtime
-- errors. (Use 'deleteMetaEither' instead if you would rather intercept them.)
-- The file is modified in place. Make sure you have the necessary backups!
deleteMeta ::
  -- | ExifTool instance
  ExifTool ->
  -- | tags to be deleted
  [Tag] ->
  -- | file name
  Text ->
  IO ()
deleteMeta et ts file = eitherError <$> deleteMetaEither et ts file

-- | Delete metadata from a file, with ExifTool errors returned as Left values.
-- The file is modified in place. Make sure you have the necessary backups!
deleteMetaEither ::
  -- | ExifTool instance
  ExifTool ->
  -- | tags to be deleted
  [Tag] ->
  -- | file name
  Text ->
  IO (Either Text ())
deleteMetaEither et ts = setMetaEither et (fromList $ fmap (,String "-") ts)

-- | Filter metadata by tag name.
filterByTag :: (Tag -> Bool) -> Metadata -> Metadata
filterByTag p = filterWithKey (\t _ -> p t)

-- | Filter metadata by tag name ignoring case.
--
-- Note that @~~@ has higher precedence than '<>', so @m ~~ t <> m ~~ t' == (m
-- ~~ t) <> (m ~~ t')@ which makes combining filters easy.
(~~) :: Metadata -> Tag -> Metadata
infixl 8 ~~
m ~~ (Tag t) = filterByTag (\(Tag t') -> toCaseFold t == toCaseFold t') m

-- | Extract content from Right or throw error.
eitherError :: Either Text a -> a
eitherError = either (error . cs) id
