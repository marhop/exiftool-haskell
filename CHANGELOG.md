# Revision history for exiftool

## 0.2.0.5 -- 2024-04-01

* Maintenance release.

## 0.2.0.4 -- 2023-03-13

* Bugfix re version number in cabal file.

## 0.2.0.3 -- 2023-03-13

* Maintenance release.

## 0.2.0.2 -- 2022-11-14

* Maintenance release.

## 0.2.0.1 -- 2022-08-18

* Dependency version bumps.

## 0.2.0.0 -- 2021-09-28

* New (backwards-incompatible) API. Highlights:
  * The simplified Tag type allows greater syntactic flexibility when specifying
    tag names.
  * The new FromValue/ToValue type classes and new functions for querying and
    manipulating metadata (get/set/del) support polymorphic values.

## 0.1.1.0 -- 2021-04-24

* The getMeta* functions now extract unknown tags as well (exiftool -U option).
* Improved documentation in [README](README.md).
* Dependency version bumps.

## 0.1.0.0 -- 2020-09-14

* Initial release.
