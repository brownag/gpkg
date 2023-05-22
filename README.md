
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpkg - Utilities for the OGC ‘GeoPackage’ Format

<!-- badges: start -->

[![R-CMD-check](https://github.com/brownag/gpkg/actions/workflows/R-CMD-check.yml/badge.svg?branch=main)](https://github.com/brownag/gpkg/actions/workflows/R-CMD-check.yml)
[![gpkg HTML
Manual](http://img.shields.io/badge/docs-HTML-informational)](https://humus.rocks/gpkg/)
[![CRAN
status](https://www.r-pkg.org/badges/version/gpkg)](https://CRAN.R-project.org/package=gpkg)
[![Codecov test
coverage](https://codecov.io/gh/brownag/gpkg/branch/main/graph/badge.svg)](https://app.codecov.io/gh/brownag/gpkg?branch=main)
<!-- badges: end -->

High-level wrapper functions to build [Open Geospatial Consortium (OGC)
‘GeoPackage’ files](https://www.geopackage.org/).
[GDAL](https://www.gdal.org/) utilities for read and write of spatial
data ([vector](https://www.gdal.org/drv_geopackage.html) and
[gridded](https://www.gdal.org/drv_geopackage_raster.html)) are provided
via the {[terra](https://cran.r-project.org/package=terra)} package.
Additional ‘GeoPackage’ and ‘SQLite’ specific functions manipulate
attributes and tabular data via the
{[RSQLite](https://cran.r-project.org/package=RSQLite)} package.

<a href="https://raw.githubusercontent.com/brownag/gpkg/main/man/figures/gpkg_sticker_v1.png">
<img src = "https://raw.githubusercontent.com/brownag/gpkg/main/man/figures/gpkg_sticker_v1.png" alt = "gpkg hexsticker" title = "gpkg hexsticker: {gpkg} provides high-level wrapper functions to build GeoPackages containing a variety of different data." width = "35%" height = "35%" hspace="25" vspace="25" align="right"/></a>

## Installation

The package can be installed from GitHub with {remotes}

``` r
if (!requireNamespace("remotes")) 
  install.packages("remotes")
remotes::install_github("brownag/gpkg")
```

## Background

### What is a GeoPackage?

[GeoPackage](https://www.geopackage.org/) is an open, standards-based,
platform-independent, portable, self-describing, compact format for
transferring geospatial information. The [GeoPackage Encoding
Standard](https://www.ogc.org/standards/geopackage) describes a set of
conventions for storing the following within an SQLite database:

- vector features

- tile matrix sets of imagery and raster maps at various scales

- attributes (non-spatial data)

- extensions

## Create a Geopackage

`gpkg_write()` can handle a variety of different input types. Here we
start by adding two DEM (GeoTIFF) files.

``` r
library(gpkg)
library(terra)
#> terra 1.7.33

dem <- system.file("extdata", "dem.tif", package = "gpkg")
stopifnot(nchar(dem) > 0)
gpkg_tmp <- tempfile(fileext = ".gpkg")

if (file.exists(gpkg_tmp))
  file.remove(gpkg_tmp)

# write a gpkg with two DEMs in it
gpkg_write(
  dem,
  destfile = gpkg_tmp,
  RASTER_TABLE = "DEM1",
  FIELD_NAME = "Elevation"
)

gpkg_write(
  dem,
  destfile = gpkg_tmp,
  append = TRUE,
  RASTER_TABLE = "DEM2",
  FIELD_NAME = "Elevation",
  NoData = -9999
)
```

## Insert Vector Layers

We can also write vector data to GeoPackage. Here we use `gpkg_write()`
to add a bounding box polygon layer derived from extent of `"DEM1"`.

``` r
# add bounding polygon vector layer via named list
r <- gpkg_tables(geopackage(gpkg_tmp))[['DEM1']]
v <- terra::as.polygons(r, ext = TRUE)
gpkg_write(list(bbox = v), destfile = gpkg_tmp, append = TRUE)
```

## Insert Attribute Table

Similarly, `data.frame`-like objects (non-spatial “attributes”) can be
written to GeoPackage.

``` r
z <- data.frame(a = 1:10, b = LETTERS[1:10])
gpkg_write(list(myattr = z), destfile = gpkg_tmp, append = TRUE)
```

## Read a GeoPackage

`geopackage()` is a constructor that can create a simple container for
working with geopackages from several types of inputs. Often you will
have a *character* file path to a GeoPackage (.gpkg) file.

``` r
g <- geopackage(gpkg_tmp, connect = TRUE)
g
#> <geopackage>
#> --------------------------------------------------------------------------------
#> # of Tables: 18
#>  
#>  DEM1, DEM2, bbox, gpkg_2d_gridded_coverage_ancillary,
#>  gpkg_2d_gridded_tile_ancillary, gpkg_contents, gpkg_extensions,
#>  gpkg_geometry_columns, gpkg_ogr_contents, gpkg_spatial_ref_sys,
#>  gpkg_tile_matrix, gpkg_tile_matrix_set, myattr, rtree_bbox_geom,
#>  rtree_bbox_geom_node, rtree_bbox_geom_parent, rtree_bbox_geom_rowid,
#>  sqlite_sequence
#> --------------------------------------------------------------------------------
#> <SQLiteConnection>
#>   Path: /tmp/RtmpD9BEIk/file13a68483b2538.gpkg
#>   Extensions: TRUE
class(g)
#> [1] "geopackage"
```

Other times you may have a list of tables and layers you want to be in a
GeoPackage that does not exist yet.

``` r
g2 <- geopackage(list(dem = r, bbox = v))
g2
#> <geopackage>
#> --------------------------------------------------------------------------------
#> # of Tables: 16
#>  
#>  bbox, dem, gpkg_2d_gridded_coverage_ancillary,
#>  gpkg_2d_gridded_tile_ancillary, gpkg_contents, gpkg_extensions,
#>  gpkg_geometry_columns, gpkg_ogr_contents, gpkg_spatial_ref_sys,
#>  gpkg_tile_matrix, gpkg_tile_matrix_set, rtree_bbox_geom,
#>  rtree_bbox_geom_node, rtree_bbox_geom_parent, rtree_bbox_geom_rowid,
#>  sqlite_sequence
#> --------------------------------------------------------------------------------
#> <SQLiteConnection>
#>   Path: /tmp/RtmpD9BEIk/Rgpkg13a68241601a0.gpkg
#>   Extensions: TRUE
class(g2)
#> [1] "geopackage"
```

Note that a temporary GeoPackage
(/tmp/RtmpD9BEIk/Rgpkg13a68241601a0.gpkg) is automatically created when
using the `geopackage(<list>)` constructor.

You also may have a *DBIConnection* to a GeoPackage database already
opened that you want to use. In any case (*character*, *list*,
*SQLiteConnection*) there is an S3 method to facilitate creating the
basic *geopackage* class provided by {gpkg}. All other methods are
designed to be able to work smoothly with *geopackage* class input.

## Inspect Contents of GeoPackage

We can list the table names in a GeoPackage with `gpkg_list_tables()`
and fetch pointers (SpatRaster, SpatVectorProxy, and lazy data.frame) to
the data in them with `gpkg_table()`. We can check the status of the
internal `geopackage` class `SQLiteConnection` with
`gpkg_is_connected()` and disconnect it with `gpkg_disconnect()`.

``` r
# enumerate tables
gpkg_list_tables(g)
#>  [1] "DEM1"                               "DEM2"                              
#>  [3] "bbox"                               "gpkg_2d_gridded_coverage_ancillary"
#>  [5] "gpkg_2d_gridded_tile_ancillary"     "gpkg_contents"                     
#>  [7] "gpkg_extensions"                    "gpkg_geometry_columns"             
#>  [9] "gpkg_ogr_contents"                  "gpkg_spatial_ref_sys"              
#> [11] "gpkg_tile_matrix"                   "gpkg_tile_matrix_set"              
#> [13] "myattr"                             "rtree_bbox_geom"                   
#> [15] "rtree_bbox_geom_node"               "rtree_bbox_geom_parent"            
#> [17] "rtree_bbox_geom_rowid"              "sqlite_sequence"

# inspect tables
gpkg_tables(g)
#> $DEM1
#> class       : SpatRaster 
#> dimensions  : 30, 31, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 6.008333, 6.266667, 49.69167, 49.94167  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : file13a68483b2538.gpkg:DEM1 
#> varname     : file13a68483b2538 
#> name        : DEM1 
#> 
#> $DEM2
#> class       : SpatRaster 
#> dimensions  : 30, 31, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 6.008333, 6.266667, 49.69167, 49.94167  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : file13a68483b2538.gpkg:DEM2 
#> varname     : file13a68483b2538 
#> name        : DEM2 
#> min value   :  195 
#> max value   :  500 
#> 
#> $myattr
#> # Source:   table<myattr> [10 x 2]
#> # Database: sqlite 3.41.2 [/tmp/RtmpD9BEIk/file13a68483b2538.gpkg]
#>        a b    
#>    <int> <chr>
#>  1     1 A    
#>  2     2 B    
#>  3     3 C    
#>  4     4 D    
#>  5     5 E    
#>  6     6 F    
#>  7     7 G    
#>  8     8 H    
#>  9     9 I    
#> 10    10 J    
#> 
#> $bbox
#>  class       : SpatVectorProxy
#>  geometry    : polygons 
#>  dimensions  : 1, 0  (geometries, attributes)
#>  extent      : 6.008333, 6.266667, 49.69167, 49.94167  (xmin, xmax, ymin, ymax)
#>  source      : file13a68483b2538.gpkg (bbox)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326)

# inspect a specific table
gpkg_table(g, "myattr", collect = TRUE)
#>     a b
#> 1   1 A
#> 2   2 B
#> 3   3 C
#> 4   4 D
#> 5   5 E
#> 6   6 F
#> 7   7 G
#> 8   8 H
#> 9   9 I
#> 10 10 J
```

Note that the `collect = TRUE` forces data be loaded into R memory for
vector and attribute data; this is the difference in result object class
of *SpatVectorProxy*/*SpatVector* and
*tbl_SQLiteConnection*/*data.frame* for vector and attribute data,
respectively.

`gpkg_collect()` is a helper method to call
`gpkg_table(..., collect = TRUE)` for in-memory loading of specific
tables.

``` r
gpkg_collect(g, "DEM1")
#>   id zoom_level tile_column tile_row     tile_data
#> 1  1          0           0        0 blob[3.98 kB]
```

Note that with grid data returned from `gpkg_collect()` you get a table
result with the tile contents in a blob column of a *data.frame* instead
of *SpatRaster* object.

The inverse function of `gpkg_collect()` is `gpkg_tbl()` which always
returns a *tbl_SQLiteConnection*.

``` r
gpkg_tbl(g, "gpkg_contents")
#> # Source:   table<gpkg_contents> [4 x 10]
#> # Database: sqlite 3.41.2 [/tmp/RtmpD9BEIk/file13a68483b2538.gpkg]
#>   table_name data_type   identifier description last_change   min_x min_y  max_x
#>   <chr>      <chr>       <chr>      <chr>       <chr>         <dbl> <dbl>  <dbl>
#> 1 DEM1       2d-gridded… DEM1       ""          2023-05-22…    6.01  49.7   6.27
#> 2 DEM2       2d-gridded… DEM2       ""          2023-05-22…    6.01  49.7   6.27
#> 3 bbox       features    bbox       ""          2023-05-22…    6.01  49.7   6.27
#> 4 myattr     attributes  myattr     ""          2023-05-21… -180    -90   180   
#> # ℹ 2 more variables: max_y <dbl>, srs_id <int>
```

More on how to use this type of result next.

### Lazy Data Access

There are several other methods that can be used for working with
tabular data in a GeoPackage in a “lazy” fashion.

#### Method 1: `gpkg_table_pragma()`

`gpkg_table_pragma()` is a low-frills `data.frame` result containing
important table information, but not values. The `PRAGMA table_info()`
is stored as a nested data.frame `table_info`. This representation has
no dependencies beyond {RSQLite} and is efficient for inspection of
table structure and attributes, though it is less useful for data
analysis.

``` r
head(gpkg_table_pragma(g))
#>                                      dsn table_name nrow table_info.cid
#> 1 /tmp/RtmpD9BEIk/file13a68483b2538.gpkg       DEM1    1              0
#> 2 /tmp/RtmpD9BEIk/file13a68483b2538.gpkg       DEM1    1              1
#> 3 /tmp/RtmpD9BEIk/file13a68483b2538.gpkg       DEM1    1              2
#> 4 /tmp/RtmpD9BEIk/file13a68483b2538.gpkg       DEM1    1              3
#> 5 /tmp/RtmpD9BEIk/file13a68483b2538.gpkg       DEM1    1              4
#> 6 /tmp/RtmpD9BEIk/file13a68483b2538.gpkg       DEM2    1              0
#>   table_info.name table_info.type table_info.notnull table_info.dflt_value
#> 1              id         INTEGER                  0                  <NA>
#> 2      zoom_level         INTEGER                  1                  <NA>
#> 3     tile_column         INTEGER                  1                  <NA>
#> 4        tile_row         INTEGER                  1                  <NA>
#> 5       tile_data            BLOB                  1                  <NA>
#> 6              id         INTEGER                  0                  <NA>
#>   table_info.pk
#> 1             1
#> 2             0
#> 3             0
#> 4             0
#> 5             0
#> 6             1
```

#### Method 2: `gpkg_vect()` and `gpkg_query()`

`gpkg_vect()` is a wrapper around `terra::vect()` you can use to create
‘terra’ `SpatVector` objects from the tables found in a GeoPackage.

``` r
gpkg_vect(g, 'bbox')
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 1, 0  (geometries, attributes)
#>  extent      : 6.008333, 6.266667, 49.69167, 49.94167  (xmin, xmax, ymin, ymax)
#>  source      : file13a68483b2538.gpkg (bbox)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326)
```

The table of interest need not have a geometry column, but this method
does not work on GeoPackage that contain only gridded data, and some
layer in the GeoPackage must have some geometry.

``` r
gpkg_vect(g, 'gpkg_ogr_contents')
#>  class       : SpatVector 
#>  geometry    : none 
#>  dimensions  : 0, 2  (geometries, attributes)
#>  extent      : 0, 0, 0, 0  (xmin, xmax, ymin, ymax)
#>  source      : file13a68483b2538.gpkg (SELECT)
#>  coord. ref. :  
#>  names       : table_name feature_count
#>  type        :      <chr>         <int>
```

The *SpatVectorProxy* is used for “lazy” references to of vector and
attribute contents of a GeoPackage; this object for vector data is
analogous to the *SpatRaster* for gridded data. The ‘terra’ package
provides “GDAL plumbing” for filter and query utilities.

`gpkg_query()` by default uses the ‘RSQLite’ driver, but the richer
capabilities of OGR data sources can be harnessed with [SQLite SQL
dialect](https://gdal.org/user/sql_sqlite_dialect.html). These
additional features can be utilized with the `ogr=TRUE` argument to
`gpkg_query()`, or `gpkg_ogr_query()` for short. This assumes that
GDAL/OGR is built with support for SQLite (ideally also with Spatialite
support).

For example, we use built-in functions such as `ST_MinX()` to calculate
summaries for `"bbox"` table, geometry column `"geom"`. In this case we
expect the calculated quantities to match the coordinates/boundaries of
the bounding box:

``` r
res <- gpkg_ogr_query(g, "SELECT 
                           ST_MinX(geom) AS xmin,
                           ST_MinY(geom) AS ymin, 
                           ST_MaxX(geom) AS xmax, 
                           ST_MaxY(geom) AS ymax 
                          FROM bbox")
as.data.frame(res)
#>       xmin     ymin     xmax     ymax
#> 1 6.008333 49.69167 6.266667 49.94167
```

#### Method 3: `gpkg_rast()`

Using `gpkg_rast()` you can quickly access references to all
tile/gridded datasets in a GeoPackage.

For example:

``` r
gpkg_rast(g)
#> class       : SpatRaster 
#> dimensions  : 30, 31, 2  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 6.008333, 6.266667, 49.69167, 49.94167  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> sources     : file13a68483b2538.gpkg:DEM1  
#>               file13a68483b2538.gpkg:DEM2  
#> varnames    : file13a68483b2538 
#>               file13a68483b2538 
#> names       : DEM1, DEM2 
#> min values  :   ? ,  195 
#> max values  :   ? ,  500
```

#### Method 4: `gpkg_table()`

With the `gpkg_table()` method you access a specific table (by name) and
get a “lazy” `tibble` object referencing that table.

This is achieved via {dplyr} and the {dbplyr} database connection to the
GeoPackage via the {RSQLite} driver. The resulting object’s data can be
used in more complex analyses by using other {dbplyr}/{tidyverse}
functions.

For example, we inspect the contents of the `gpkg_contents` table that
contains critical information on the data contained in a GeoPackage.

``` r
gpkg_table(g, "gpkg_contents")
#> # Source:   table<gpkg_contents> [4 x 10]
#> # Database: sqlite 3.41.2 [/tmp/RtmpD9BEIk/file13a68483b2538.gpkg]
#>   table_name data_type   identifier description last_change   min_x min_y  max_x
#>   <chr>      <chr>       <chr>      <chr>       <chr>         <dbl> <dbl>  <dbl>
#> 1 DEM1       2d-gridded… DEM1       ""          2023-05-22…    6.01  49.7   6.27
#> 2 DEM2       2d-gridded… DEM2       ""          2023-05-22…    6.01  49.7   6.27
#> 3 bbox       features    bbox       ""          2023-05-22…    6.01  49.7   6.27
#> 4 myattr     attributes  myattr     ""          2023-05-21… -180    -90   180   
#> # ℹ 2 more variables: max_y <dbl>, srs_id <int>
```

As a more complicated example we access the
`gpkg_2d_gridded_tile_ancillary` table, and perform some data
processing.

We `dplyr::select()` `mean` and `std_dev` columns from the
`dplyr::filter()`ed rows where `tpudt_name == "DEM2"`. Finally we
materialize a `tibble` with `dplyr::collect()`:

``` r
library(dplyr, warn.conflicts = FALSE)

gpkg_table(g, "gpkg_2d_gridded_tile_ancillary") %>% 
  filter(tpudt_name == "DEM2") %>% 
  select(mean, std_dev) %>% 
  collect()
#> # A tibble: 1 × 2
#>    mean std_dev
#>   <dbl>   <dbl>
#> 1  324.    58.5
```

### Managing Connections

Several helper methods are available for checking GeoPackage
`SQLiteConnection` status, as well as connecting and disconnecting an
existing `geopackage` object (`g`).

``` r
# still connected
gpkg_is_connected(g)
#> [1] TRUE

# disconnect geopackage
gpkg_disconnect(g)
#> <geopackage>
#> --------------------------------------------------------------------------------
#> # of Tables: 19
#>  
#>  DEM1, DEM2, bbox, dummy_feature, gpkg_2d_gridded_coverage_ancillary,
#>  gpkg_2d_gridded_tile_ancillary, gpkg_contents, gpkg_extensions,
#>  gpkg_geometry_columns, gpkg_ogr_contents, gpkg_spatial_ref_sys,
#>  gpkg_tile_matrix, gpkg_tile_matrix_set, myattr, rtree_bbox_geom,
#>  rtree_bbox_geom_node, rtree_bbox_geom_parent, rtree_bbox_geom_rowid,
#>  sqlite_sequence
#> --------------------------------------------------------------------------------

# reconnect
g <- gpkg_connect(g)

# disconnect
gpkg_disconnect(g)
#> <geopackage>
#> --------------------------------------------------------------------------------
#> # of Tables: 19
#>  
#>  DEM1, DEM2, bbox, dummy_feature, gpkg_2d_gridded_coverage_ancillary,
#>  gpkg_2d_gridded_tile_ancillary, gpkg_contents, gpkg_extensions,
#>  gpkg_geometry_columns, gpkg_ogr_contents, gpkg_spatial_ref_sys,
#>  gpkg_tile_matrix, gpkg_tile_matrix_set, myattr, rtree_bbox_geom,
#>  rtree_bbox_geom_node, rtree_bbox_geom_parent, rtree_bbox_geom_rowid,
#>  sqlite_sequence
#> --------------------------------------------------------------------------------
```
