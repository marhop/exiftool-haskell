{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module     : ExifTool
-- Copyright  : (c) Martin Hoppenheit 2020
-- License    : MIT
-- Maintainer : martin@hoppenheit.info
--
-- TODO Module documentation, including complete usage example.

module ExifTool
    ( ExifTool
    , Tag(..)
    , Value(..)
    , startExifTool
    , stopExifTool
    , withExifTool
    , getMetadata
    ) where

import Control.Exception (bracket)
import qualified Data.Aeson as JSON
import Data.Aeson
    ( FromJSON(..)
    , FromJSONKey(..)
    , FromJSONKeyFunction(..)
    , eitherDecode
    )
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import Data.String.Conversions (cs)
import Data.Text (Text, isPrefixOf, splitOn, stripPrefix)
import Data.Text.IO (hGetLine, hPutStrLn)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import System.IO (Handle, hFlush)
import System.Process
    ( ProcessHandle
    , StdStream(CreatePipe)
    , cleanupProcess
    , createProcess
    , proc
    , std_in
    , std_out
    )

-- | An ExifTool instance, initialized with 'startExifTool' and terminated with
-- 'stopExifTool'.
data ExifTool = ET
    { etIn   :: !Handle        -- ^ STDIN of this ExifTool process
    , etOut  :: !Handle        -- ^ STDOUT of this ExifTool process
    , etProc :: !ProcessHandle -- ^ process handle of this ExifTool process
    }

-- | An ExifTool tag name, consisting of three components:
--
-- 1. The family 0 tag group (information type) e.g., @EXIF@ or @XMP@.
-- 2. The family 1 tag group (specific location) e.g., @IFD0@ or @XMP-dc@.
-- 3. The actual tag name e.g., @XResolution@ or @Description@.
--
-- Example: @Tag "EXIF" "IFD0" "XResolution"@ corresponds to the ExifTool tag
-- name @EXIF:IFD0:XResolution@.
--
-- See <https://exiftool.org/#groups> for a list of tag groups.
data Tag = Tag
    { tagFamily0 :: !Text -- ^ family 0 tag group
    , tagFamily1 :: !Text -- ^ family 1 tag group
    , tagName    :: !Text -- ^ actual tag name
    } deriving (Show, Eq, Generic, Hashable)

instance FromJSON Tag where
    parseJSON (JSON.String x)
        | Just t <- parseTag x = return t
    parseJSON x = fail $ "unexpected formatting of ExifTool tag: " <> show x

-- | Parse an ExifTool tag name of the form @family0:family1:name@ or the
-- special case @SourceFile@.
parseTag :: Text -> Maybe Tag
parseTag t =
    case splitOn ":" t of
        [f0, f1, n] -> Just $ Tag f0 f1 n
        ["SourceFile"] -> Just $ Tag "" "" "SourceFile"
        _ -> Nothing

instance FromJSONKey Tag where
    fromJSONKey = FromJSONKeyTextParser $ parseJSON . JSON.String

-- | An ExifTool tag value, enclosed in a type wrapper.
data Value
    = String !Text
    | Binary !ByteString
    | Number !Scientific
    | Bool   !Bool
    | List   ![Value]
    -- | Struct (Map Text Value)
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

-- | Start an ExifTool instance.
--
-- Use 'stopExifTool' when done, or 'withExifTool' to combine both steps.
startExifTool :: IO ExifTool
startExifTool = do
    (Just i, Just o, _, p) <- createProcess conf
    return $ ET i o p
  where
    conf = (proc "exiftool" options) {std_in = CreatePipe, std_out = CreatePipe}
    options =
        [ "-stay_open"
        , "True"
        , "-@"
        , "-"
        , "-common_args"
        , "-json"
        , "-a"
        , "-G:0:1"
        , "-s"
        , "-binary"
        ]

-- | Stop a running ExifTool instance.
stopExifTool :: ExifTool -> IO ()
stopExifTool (ET i o p) = do
    hPutStrLn i "-stay_open"
    hPutStrLn i "False"
    hFlush i
    cleanupProcess (Just i, Just o, Nothing, p)

-- | Start an ExifTool instance, do something with it, then stop it.
withExifTool :: (ExifTool -> IO a) -> IO a
withExifTool = bracket startExifTool stopExifTool

-- | Send a sequence of command-line arguments to a running ExifTool instance
-- and return the corresponding output.
--
-- The final @-execute@ argument is added automatically.
sendCommand :: ExifTool -> [Text] -> IO Text
sendCommand (ET i o _) cmds = do
    mapM_ (hPutStrLn i) cmds
    hPutStrLn i "-execute"
    hFlush i
    readOutput o ""
  where
    readOutput :: Handle -> Text -> IO Text
    readOutput h acc = do
        l <- hGetLine h
        if "{ready}" `isPrefixOf` l
            then return acc
            else readOutput h (acc <> l)

-- | Read all metadata from a file.
getMetadata :: ExifTool                               -- ^ ExifTool instance
            -> Text                                   -- ^ file name
            -> IO (Either String (HashMap Tag Value)) -- ^ tag/value Map
getMetadata et file = parseOutput <$> sendCommand et [file]
  where
    parseOutput :: Text -> Either String (HashMap Tag Value)
    -- TODO Use total variant of head?
    parseOutput = fmap head . eitherDecode . cs
