# gpkg 0.0.2.9002

* Updated definitions of `lazy.frame()` and `dplyr.frame()` and related docs

* Updated `gpkg_write()` for raster, vector, and attribute data

  * General cleanup and refactored approach for splitting input data sources between attributes, vector, and tiled grid coverage.

# gpkg 0.0.2

* Added a `NEWS.md` file to track changes to the package.

* Added {dbplyr} backend option to return `tibble` object via `geopackage` object internal SQLiteConnection

* Changed formatting of PRAGMA `table_info` child data.frame in `lazy.frame()` result

# gpkg 0.0.1

* Initial draft of key features, R package structure, and naming of functions
