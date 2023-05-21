# gpkg 0.0.5

 - Fixed bug in `gpkg_tables()` when multiple attribute tables were present.
 
 - Added `gpkg_ogr_query()` (and `ogr` argument to `gpkg_query()`) to support querying OGR data sources using a 'terra' _SpatVectorProxy_ back end. This allows for use of GeoPackage-specific SQL functions and the OGR/SQLite dialects: <https://gdal.org/user/sql_sqlite_dialect.html> and <https://gdal.org/user/ogr_sql_dialect.html>; requires latest 'terra' (1.7-33+)
 
 - Added `gpkg_rast()` (analog of `gpkg_vect()` for `terra::rast()`) for lazy manipulation of gridded data in a GeoPackage
 
 - Added `gpkg_create_dummy_features()` which is a workaround for use of `gpkg_vect()` on a GeoPackage with only gridded or attribute data.
 
 - Added `gpkg_list_contents()` and `gpkg_ogr_contents()`; the former returns only table names registered in `gpkg_contents` table, which are optionally intersected with tables defined in `gpkg_ogr_contents` when `ogr=TRUE`.
 
# gpkg 0.0.4

 - Fixed bug in `.gpkg_gdaloptions_add()` that could cause addition of options with no value set
 
 - Added `gpkg_vect()`: a `terra::vect()`-based analog of `gpkg_table()` for lazy manipulation of vector and attribute data in a GeoPackage
 
 - `gpkg_update_table()` defaults to `wherecol=NULL` which allows updating rows with no SQL `WHERE` clause specified

# gpkg 0.0.3

 - Refactoring table accessor methods (https://github.com/brownag/gpkg/issues/2)
 
   - Renamed `lazy.frame()` -> `gpkg_table_pragma()`
  
   - Renamed `dplyr.frame()` -> `gpkg_table()`
   
   - `gpkg_table()` gains `collect` argument (toggles materializing data.frame v.s. 'dbplyr' approach). `collect` requires only 'RSQLite' not 'dbplyr'.
   
   - `gpkg_tables()` gains `pragma` argument to toggle use of `gpkg_table_pragma()` over `gpkg_table()`. It also supports `collect` argument. 
   
 - `gpkg_update_table()` is now exported; this method is/was used internally for updating the NoData entry for existing layers via `gpkg_tile_set_data_null()`

# gpkg 0.0.2

* Updated definitions of `lazy.frame()` and `dplyr.frame()` and related docs

* Updated `gpkg_write()` for raster, vector, and attribute data

  * General cleanup and refactored approach for splitting input data sources between attributes, vector, and tiled grid coverage. 
  
  * New approach still contains hard-coded logic related to the allowed object types and file data sources (that will be generalized in the future)
  
  * `RASTER_TABLE` name is now properly injected into GDAL options from named list input (when not otherwise specified)

* Bug fix in `geopackage(<list>)` related to appending v.s. overwriting raster data

* `gpkg_update_contents()`: Handle `gpkgext_` and `sqlite_sequence` as standard GPKG table prefixes  
  * fixes error with updating contents of GPKG with extensions loaded
  
* Added {dbplyr} backend option to return `tibble` object via `geopackage` object internal SQLiteConnection

* Changed formatting of PRAGMA `table_info` child data.frame in `lazy.frame()` result

# gpkg 0.0.1

* Initial draft of key features, R package structure, and naming of functions
