# gpkg 0.0.13

 - Deprecated `destfile` argument to `gpkg_write()` and replaced with `y`, which now accepts _geopackage_ or _DBIConnection_ objects
 
# gpkg 0.0.12
 
 - Major cleanup and more consistent use of internal _SQLiteConnection_ usage to ensure open connections don't get garbage collected
 
 - Added `gpkg_disconnect()` convenience methods for `tbl_SQLiteConnection` and `src_SQLiteConnection` 
 
 - Added `gpkg_connection()` a user-level function for accessing or creating SQLiteConnections given a `geopackage` object or a path to GeoPackage file

# gpkg 0.0.11

 - Added `[` and `[[` methods for accessing/replacing tables in a `geopackage` object
 
 - Replaced `vapour` functionality for driver detection with `gdalraster` equivalents

# gpkg 0.0.10

 - Added `gpkg()` alias for `geopackage()`
 
 - Added `gpkg_create_geometry_columns()`, `gpkg_geometry_columns()` and `gpkg_add_geometry_columns()`
 
 - Added `gpkg_sf()` convenience method for creating an _sf_ object from tables. Defaults to a _sf_ _tbl_df_, use `as_tibble=FALSE` for _data.frame_.
 
 - Now using new `gpkg_create_spatial_ref_sys()` function internally to ensure GeoPackages have the minimum required tables
 
 - `gpkg_collect()` and `gpkg_table(collect=TRUE)` gain support for selecting a subset of columns of interest
 
 - Deprecate `gpkg_create_dummy_features()` function name and replace with `gpkg_create_empty_features()`
 
 - Deprecate `gpkg_contents(template=)` argument, provide new arguments for each data element (SRS ID and bounding box)

# gpkg 0.0.9

 - Implemented GDAL driver detection for file paths via {vapour} for #15
 
 - Implemented functions for creating `gpkg_spatial_ref_sys` table and adding or removing spatial reference system records:
   - `gpkg_create_spatial_ref_sys()`, `gpkg_add_spatial_ref_sys()` `gpkg_delete_spatial_ref_sys()`, `gpkg_list_srs()`

 - Implemented basic `gpkg_validate()` routine (will be expanded)
   - Checks that `gpkg_contents` and `gpkg_spatial_ref_sys` exist
   - Checks that at least one tile or vector dataset (with at least 0 rows) is in the database and `gpkg_contents`
 
# gpkg 0.0.8

 - Added `gpkg_create_spatial_view()` for creating spatial views, which are dynamic layers accessible as if they were typical static geometry layers (for #6).
 
 - `gpkg_write()` now properly writing attributes from a _data.frame_ source (which is not nested in a list) when `table_name` argument is 
 
 - `gpkg_tables()` better handling for `terra::vect()` failures with non-spatial table function fallback. 
   - Found a regression in {terra} behavior that was reported and fixed, so the likelihood of error in the future is lower. 
 
 - `gpkg_write()` now can handle _SpatVectorProxy_ input by querying it. Note this will not work for data that will not fit in memory. May try to implement an ogr2ogr-like interface for this in the future (#15).

# gpkg 0.0.7

 - Added `gpkg_bbox()` as an application of `gpkg_ogr_query()`
 
# gpkg 0.0.6

 - `geopackage()` internal connection object has been moved to an internal environment (`<geopackage>$env$con` rather than `<geopackage>$con`)
 
 - `gpkg_connect()` allows for in-place modification of an existing object for the purposes of creating an internal _SQLiteConnection_

 - Improvement to `gpkg_add_contents()` and `gpkg_update_contents()`: more intelligent choice of data type when heterogeneous (non-attribute) data are in database but not registered. Fix for broken tests on Windows/GDAL 3.5.2.

# gpkg 0.0.5

 - Fixed bug in `gpkg_tables()` when multiple attribute tables were present.
 
 - Added `gpkg_ogr_query()` (and `ogr` argument to `gpkg_query()`) to support querying OGR data sources using a 'terra' _SpatVectorProxy_ back end. This allows for use of GeoPackage-specific SQL functions and the OGR/SQLite dialects: <https://gdal.org/en/stable/user/sql_sqlite_dialect.html> and <https://gdal.org/en/stable/user/ogr_sql_dialect.html>; requires latest 'terra' (1.7-33+)
 
 - Added `gpkg_rast()` (analog of `gpkg_vect()` for `terra::rast()`) for lazy manipulation of gridded data in a GeoPackage
 
 - Added `gpkg_create_dummy_features()` which is a workaround for use of `gpkg_vect()` on a GeoPackage with only attribute data. This creates innocuous entries in `gpkg_geometry_columns` and an empty table to "trick" GDAL into reading arbitrary data sources.
 
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
