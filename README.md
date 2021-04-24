# exiftool-haskell

[![Hackage](https://img.shields.io/hackage/v/exiftool)](https://hackage.haskell.org/package/exiftool)
[![CI](https://github.com/marhop/exiftool-haskell/actions/workflows/ci.yml/badge.svg)](https://github.com/marhop/exiftool-haskell/actions/workflows/ci.yml)

Haskell bindings to the [ExifTool](https://exiftool.org) command-line
application that enable reading, writing and deleting metadata in various file
formats.

Full documentation is on [Hackage](https://hackage.haskell.org/package/exiftool/docs/ExifTool.html).
A short code example:

```haskell
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.HashMap.Strict ((!?))
import ExifTool

example :: IO ()
example =
  withExifTool $ \et -> do
    -- Read metadata, with exact (!?) and fuzzy (~~) tag lookup.
    m <- getMeta et "a.jpg"
    print $ m !? Tag "EXIF" "ExifIFD" "DateTimeOriginal"
    print $ m ~~ Tag "EXIF" "" "XResolution"
    print $ m ~~ Tag "XMP" "" ""
    -- Write and delete metadata.
    setMeta et [(Tag "XMP" "XMP-dc" "Description", String "...")] "a.jpg"
    deleteMeta et [Tag "XMP" "XMP-dc" "Description"] "a.jpg"
```
