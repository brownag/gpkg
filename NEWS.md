# gpkg 0.0.2.9003

* Updated definitions of `lazy.frame()` and `dplyr.frame()` and related docs

* Updated `gpkg_write()` for raster, vector, and attribute data

  * General cleanup and refactored approach for splitting input data sources between attributes, vector, and tiled grid coverage. 
  
  * New approach still contains hard-coded logic related to the allowed object types and file data sources (that will be generalized in the future)
  
  * `RASTER_TABLE` name is now properly injected into GDAL options from named list input (when not otherwise specified)

* Bug fix in `geopackage(<list>)` related to appending v.s. overwriting raster data

* `gpkg_update_contents()`: Handle `gpkgext_` and `sqlite_sequence` as standard GPKG table prefixes  
  * fixes error with updating contents of GPKG with extensions loaded
  
# gpkg 0.0.2

* Added a `NEWS.md` file to track changes to the package.

* Added {dbplyr} backend option to return `tibble` object via `geopackage` object internal SQLiteConnection

* Changed formatting of PRAGMA `table_info` child data.frame in `lazy.frame()` result

# gpkg 0.0.1

* Initial draft of key features, R package structure, and naming of functions
