
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpkg

<!-- badges: start -->
<!-- badges: end -->

# gpkg - Utilities for OGC GeoPackages

{gpkg} provides high-level wrapper functions to build GeoPackages
containing a variety of different data. Reading and writing of spatial
([vector](http://www.gdal.org/drv_geopackage.html) and
[gridded](http://www.gdal.org/drv_geopackage_raster.html) data) is done
via standard [GDAL](http://www.gdal.org/) utilities. Additional
functions are provided to manipulate attributes and tabular data via
[RSQLite](https://cran.r-project.org/web/packages/RSQLite/index.html).

<a href="https://raw.githubusercontent.com/brownag/gpkg/main/man/figures/gpkg_sticker_v1.png">
<img src = "https://raw.githubusercontent.com/brownag/gpkg/main/man/figures/gpkg_sticker_v1.png" alt = "gpkg hexsticker" title = "gpkg hexsticker: {gpkg} provides high-level wrapper functions to build GeoPackages containing a variety of different data." width = "35%" height = "35%" hspace="25" vspace="25" align="right"/></a>

## Installation

The package can be installed from GitHub with {remotes}

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("brownag/gpkg")
```

## Background

### What is a GeoPackage?

[GeoPackage](https://www.geopackage.org/) is an open, standards-based,
platform-independent, portable, self-describing, compact format for
transferring geospatial information. The [GeoPackage Encoding
Standard](https://www.ogc.org/standards/geopackage) describes a set of
conventions for storing the following within an SQLite database: -
vector features - tile matrix sets of imagery and raster maps at various
scales - attributes (non-spatial data) - extensions

## Create a Geopackage

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

## Read a GeoPackage

``` r
g <- geopackage(gpkg_tmp, connect = TRUE)
g
#> <geopackage>
#> # of Tables: 2
class(g)
#> [1] "geopackage"
```

## Insert Vector Layers

``` r
# add bounding polygon vector layer
writeVector(
  terra::set.crs(terra::as.polygons(terra::ext(
    gpkg_tables(g)[['DEM1']])
  ), "OGC:CRS84"),
  filename = g$dsn,
  insert = TRUE
)
```

## Read a GeoPackage

``` r
# enumerate tables
gpkg_list_tables(g)
#>  [1] "DEM1"                                "DEM2"                               
#>  [3] "file131871f0ae809"                   "gpkg_2d_gridded_coverage_ancillary" 
#>  [5] "gpkg_2d_gridded_tile_ancillary"      "gpkg_contents"                      
#>  [7] "gpkg_extensions"                     "gpkg_geometry_columns"              
#>  [9] "gpkg_ogr_contents"                   "gpkg_spatial_ref_sys"               
#> [11] "gpkg_tile_matrix"                    "gpkg_tile_matrix_set"               
#> [13] "rtree_file131871f0ae809_geom"        "rtree_file131871f0ae809_geom_node"  
#> [15] "rtree_file131871f0ae809_geom_parent" "rtree_file131871f0ae809_geom_rowid" 
#> [17] "sqlite_sequence"

# still connected
gpkg_is_connected(g)
#> [1] TRUE

# disconnect geopackage
gpkg_disconnect(g)
```
