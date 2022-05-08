# gpkg - Utilities for OGC GeoPackages

{gpkg} provides high-level wrapper functions to build GeoPackages containing a variety of different data. Reading and writing of spatial ([vector](http://www.gdal.org/drv_geopackage.html) and [gridded](http://www.gdal.org/drv_geopackage_raster.html) data) is done via standard [GDAL](http://www.gdal.org/) utilities. Additional functions are provided to manipulate attributes and tabular data via [RSQLite](https://cran.r-project.org/web/packages/RSQLite/index.html).


### What is a GeoPackage?

[GeoPackage](https://www.geopackage.org/) is an open, standards-based, platform-independent, portable, self-describing, compact format for transferring geospatial information. The [GeoPackage Encoding Standard](https://www.ogc.org/standards/geopackage) describes a set of conventions for storing the following within an SQLite database:
  -  vector features
  -  tile matrix sets of imagery and raster maps at various scales
  -  attributes (non-spatial data)
  -  extensions

### Installation

The package can be installed from GitHub with {remotes}

```r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("brownag/gpkg")
```
