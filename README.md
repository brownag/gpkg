
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpkg

<!-- badges: start -->
<!-- badges: end -->

# gpkg - Utilities for OGC GeoPackages

{gpkg} provides high-level wrapper functions to build GeoPackages
containing a variety of different data. Reading and writing of spatial
([vector](http://www.gdal.org/drv_geopackage.html) and
[gridded](http://www.gdal.org/drv_geopackage_raster.html) data) is done
via standard [GDAL](http://www.gdal.org/) utilities provided primarily
by the {terra} package. Additional functions are provided to manipulate
attributes and tabular data via
[RSQLite](https://cran.r-project.org/web/packages/RSQLite/index.html).

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

-   vector features

-   tile matrix sets of imagery and raster maps at various scales

-   attributes (non-spatial data)

-   extensions

## Create a Geopackage

`gpkg_write()` can handle a variety of different input types. Here we
start by adding two DEM (GeoTIFF) files.

``` r
library(gpkg)
library(terra)
#> terra 1.5.29

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
  FIELD_NAME = "Elevation"
)
```

## Insert Vector Layers

We can also write vector data to GeoPackage. Here we use
`terra::writeVector(..., insert = TRUE)` to add a bounding box polygon
layer derived from `"DEM1"`.

``` r
# add bounding polygon vector layer
# TODO: wrap this into gpkg_write() and add support for in-memory rasters
terra::writeVector(
  terra::set.crs(terra::as.polygons(terra::ext(
    gpkg_tables(geopackage(gpkg_tmp))[['DEM1']])
  ), "OGC:CRS84"),
  filename = gpkg_tmp,
  insert = TRUE
)
```

## Read a GeoPackage

`geopackage()` is a constructor that can create a simple container for
working with geopackages from several types of inputs. Often you will
have a *character* file path to a GeoPackage (.gpkg) file. Other times
you may have a list of tables and layers you want to be in a GeoPackage
that does not exist yet. Or, you may have a connection to a GeoPackage
database already opened that you want to use. In any case (`character`,
`list`, `SQLiteConnection`) there is an S3 method to facilitate creating
the basic `geopackage` class provided by {gpkg}.

``` r
g <- geopackage(gpkg_tmp, connect = TRUE)
g
#> <geopackage>
#> # of Tables: 3
#> <SQLiteConnection>
#>   Path: /tmp/RtmpT65Kck/file139ed2705a27.gpkg
#>   Extensions: TRUE
class(g)
#> [1] "geopackage"
```

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
#>  [3] "file139ed2705a27"                   "gpkg_2d_gridded_coverage_ancillary"
#>  [5] "gpkg_2d_gridded_tile_ancillary"     "gpkg_contents"                     
#>  [7] "gpkg_extensions"                    "gpkg_geometry_columns"             
#>  [9] "gpkg_ogr_contents"                  "gpkg_spatial_ref_sys"              
#> [11] "gpkg_tile_matrix"                   "gpkg_tile_matrix_set"              
#> [13] "rtree_file139ed2705a27_geom"        "rtree_file139ed2705a27_geom_node"  
#> [15] "rtree_file139ed2705a27_geom_parent" "rtree_file139ed2705a27_geom_rowid" 
#> [17] "sqlite_sequence"

# inspect tables
gpkg_tables(g)
#> $DEM1
#> class       : SpatRaster 
#> dimensions  : 30, 31, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 6.008333, 6.266667, 49.69167, 49.94167  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : file139ed2705a27.gpkg:DEM1 
#> varname     : file139ed2705a27 
#> name        : DEM1 
#> 
#> $DEM2
#> class       : SpatRaster 
#> dimensions  : 30, 31, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 6.008333, 6.266667, 49.69167, 49.94167  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : file139ed2705a27.gpkg:DEM2 
#> varname     : file139ed2705a27 
#> name        : DEM2 
#> 
#> $file139ed2705a27
#>  class       : SpatVectorProxy
#>  geometry    : polygons 
#>  dimensions  : 1, 0  (geometries, attributes)
#>  extent      : 6.008333, 6.266667, 49.69167, 49.94167  (xmin, xmax, ymin, ymax)
#>  source      : file139ed2705a27.gpkg:file139ed2705a27 (file139ed2705a27)
#>  coord. ref. : lon/lat WGS 84

# still connected
gpkg_is_connected(g)
#> [1] TRUE

# disconnect geopackage
gpkg_disconnect(g)
```
