# exiftool-haskell

[![Hackage](https://img.shields.io/hackage/v/exiftool)](https://hackage.haskell.org/package/exiftool)
[![CI](https://github.com/marhop/exiftool-haskell/actions/workflows/ci.yml/badge.svg)](https://github.com/marhop/exiftool-haskell/actions/workflows/ci.yml)

Haskell bindings to the [ExifTool](https://exiftool.org) command-line
application that enable reading, writing and deleting metadata in various file
formats.

Full documentation is on [Hackage](https://hackage.haskell.org/package/exiftool/docs/ExifTool.html).
A short code example:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import ExifTool

data Foo = Foo
  { description :: Text,
    resolution :: Int
  }
  deriving (Show)

main :: IO ()
main = withExifTool $ \et -> do
  m <- readMeta et [] "a.jpg"
  print $ Foo <$> get (Tag "Description") m <*> get (Tag "XResolution") m
  let m' = del (Tag "Description") . set (Tag "XResolution") (42 :: Int) $ m
  writeMeta et m' "a.jpg"
```
