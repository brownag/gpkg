
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpkg - Utilities for OGC GeoPackages

<!-- badges: start -->

[![R-CMD-check](https://github.com/brownag/gpkg/actions/workflows/R-CMD-check.yml/badge.svg?branch=main)](https://github.com/brownag/gpkg/actions/workflows/R-CMD-check.yml)
[![gpkg HTML
Manual](https://img.shields.io/badge/docs-HTML-informational)](http://humus.rocks/gpkg/)
<!-- badges: end -->

{gpkg} provides high-level wrapper functions to build GeoPackages
containing a variety of different data. Reading and writing of spatial
([vector](http://www.gdal.org/drv_geopackage.html) and
[gridded](http://www.gdal.org/drv_geopackage_raster.html)) data are done
via standard [GDAL](http://www.gdal.org/) utilities provided primarily
by the {[terra](https://cran.r-project.org/package=terra)} package.
Additional functions are provided to manipulate attributes and tabular
data via {[RSQLite](https://cran.r-project.org/package=RSQLite)}.

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
non-proprietary, platform-independent, portable, self-describing,
compact format for transferring geospatial information. The [GeoPackage
Encoding Standard](https://www.ogc.org/standards/geopackage) describes a
set of conventions for storing the following within an SQLite database:

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
#> terra 1.6.17

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

We can also write vector data to GeoPackage. Here we use `gpkg_write()`
to add a bounding box polygon layer derived from extent of `"DEM1"`.

``` r
# add bounding polygon vector layer via named list
r <- gpkg_tables(geopackage(gpkg_tmp))[['DEM1']]
v <- terra::as.polygons(r, ext = TRUE)
gpkg_write(list(bbox = v), destfile = gpkg_tmp)
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
#> # of Tables: 12
#>  
#> DEM1, DEM2, gpkg_2d_gridded_coverage_ancillary, gpkg_2d_gridded_tile_ancillary, gpkg_contents, gpkg_extensions, gpkg_geometry_columns, gpkg_ogr_contents, gpkg_spatial_ref_sys, gpkg_tile_matrix, gpkg_tile_matrix_set, sqlite_sequence
#> <SQLiteConnection>
#>   Path: /tmp/RtmpFVIye3/file4a54224961741.gpkg
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
#>  [3] "gpkg_2d_gridded_coverage_ancillary" "gpkg_2d_gridded_tile_ancillary"    
#>  [5] "gpkg_contents"                      "gpkg_extensions"                   
#>  [7] "gpkg_geometry_columns"              "gpkg_ogr_contents"                 
#>  [9] "gpkg_spatial_ref_sys"               "gpkg_tile_matrix"                  
#> [11] "gpkg_tile_matrix_set"               "sqlite_sequence"

# inspect tables
gpkg_tables(g)
#> $DEM1
#> class       : SpatRaster 
#> dimensions  : 30, 31, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 6.008333, 6.266667, 49.69167, 49.94167  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : file4a54224961741.gpkg:DEM1 
#> varname     : file4a54224961741 
#> name        : DEM1 
#> 
#> $DEM2
#> class       : SpatRaster 
#> dimensions  : 30, 31, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 6.008333, 6.266667, 49.69167, 49.94167  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : file4a54224961741.gpkg:DEM2 
#> varname     : file4a54224961741 
#> name        : DEM2

# still connected
gpkg_is_connected(g)
#> [1] TRUE

# disconnect geopackage
gpkg_disconnect(g)
```
