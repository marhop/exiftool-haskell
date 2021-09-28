# Revision history for exiftool

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
