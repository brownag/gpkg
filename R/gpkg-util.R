# GeoPackage utilities

#' Get Tables from a _geopackage_ object
#'
#' @param x A _geopackage_ object
#' @param collect Default: `FALSE`. Should tables be materialized as 'data.frame' objects in memory? (i.e. not "lazy") Default: `FALSE`; if `TRUE` 'dbplyr' is not required. Always `TRUE` for `pragma=TRUE` (pragma information are always "collected").
#' @param pragma Default: `FALSE`. Use `gpkg_table_pragma()` instead of `gpkg_table()`? The former does not require 'dbplyr'.
#'
#' @return a list of SpatVectorProxy, SpatRaster, data.frame (lazy tbl?)
#' @export
#' @rdname gpkg_tables
gpkg_tables <- function(x, collect = TRUE, pragma = TRUE)
  UseMethod("gpkg_tables", x)

#' @export
#' @rdname gpkg_tables
gpkg_tables.default <- function(x, collect = TRUE, pragma = TRUE) {
  con <- .gpkg_connection_from_x(x)
  dsn <- con@dbname
  contents <- gpkg_contents(con)
  y <- split(contents, contents$data_type)
  
  .LAZY.FUN <- ifelse(isTRUE(pragma),
                      yes = gpkg_table_pragma, 
                      no = function(x, ...) {
                        gpkg_table(x, ..., collect = TRUE)
                      })
                    
  res <- unlist(lapply(names(y), function(z) {
    switch(
      z,
      "2d-gridded-coverage" = {
        sapply(y[[z]]$table_name, function(i)
          terra::rast(dsn, i))
      },
      "features" = {
        sapply(y[[z]]$table_name, function(i) {
          res <- try(terra::vect(dsn, proxy = !collect, layer = i), silent = TRUE)
          if (inherits(res, 'try-error')) {
            # message(i, " : ", res[1])
            res <- .LAZY.FUN(con, table_name = i)
          }
          res
        })
      },
      "attributes" = {
        sapply(y[[z]]$table_name, function(i)
          list(.LAZY.FUN(con, table_name = i)))
      }
    )
  }), recursive = FALSE)
  
  if (attr(con, 'disconnect')) {
    gpkg_disconnect(con)
  }
  
  res
  
}

#' Get Source File of a _geopackage_ object
#'
#' @param x A _geopackage_ object
#' @return _character_ file path
#' @export
#' @rdname gpkg_source
gpkg_source <- function(x)
  UseMethod("gpkg_source", x)

#' @export
#' @rdname gpkg_source
gpkg_source.geopackage <- function(x) {
  x$dsn
}

#' List Tables in a GeoPackage
#'
#' @param x A _geopackage_ object, path to a GeoPackage or an _SQLiteConnection_
#' @return a character vector with names of all tables and views in the database
#' @importFrom DBI dbListTables dbDisconnect
#' @export
gpkg_list_tables <- function(x) {
  con <- .gpkg_connection_from_x(x)
  res <- character(0)
  if (!is.null(con) && DBI::dbIsValid(con)) {
    res <- DBI::dbListTables(con)
    if (attr(con, 'disconnect')) {
      DBI::dbDisconnect(con)
    }
  }
  res
}

#' Set `data_null` Metadata for a GeoPackage Tile Dataset
#'
#' @param x A _geopackage_ object, path to a GeoPackage or an _SQLiteConnection_
#' @param name character. Tile matrix set name(s) (`tile_matrix_set_name`)
#' @param value numeric. Value to use as "NoData" (`data_null` value)
#' @param query_string logical. Return SQLite query rather than executing it? Default: `FALSE`
#' @return logical. `TRUE` if number of `data_null` records updated is greater than `0`.
#' @export
gpkg_tile_set_data_null <- function(x, name, value, query_string = FALSE) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop('package `RSQLite` is required to set `data_null`', call. = FALSE)
  }

  invisible(
    gpkg_update_table(
      x,
      table_name = "gpkg_2d_gridded_coverage_ancillary",
      updatecol = "data_null",
      updatevalue = value,
      wherecol = "tile_matrix_set_name",
      wherevector = name
    ) > 0
  )
}

#' Get Default NoData Value for GeoPackage Raster Datatypes
#'
#' Returns a default NoData value for a given raster datatype, following GDAL
#' and terra conventions, and avoiding `NaN` values which produce a warning when
#' passed as `data_null` to GDAL. NOTE: at this time (GDAL 3.12) only Byte
#' (INT1U), Int16 (INT2S), UInt16 (INT2U) or Float32 (INT4S) datatypes are
#' supported in the [GPKG raster
#' driver](https://gdal.org/en/stable/drivers/raster/gpkg.html).
#' 
#' @param datatype character. A raster datatype code in terra format.
#'   Supported types are:
#'   - Signed integers: "INT1S", "INT2S", "INT4S", "INT8S"
#'   - Unsigned integers: "INT1U", "INT2U", "INT4U", "INT8U"
#'   - Floats: "FLT4S" (single precision), "FLT8S" (double precision)
#'
#' @return numeric. The default NoData value for the given datatype, or
#'   `NA_real_` if no default is available or the datatype is not recognized.
#'
#' @details 
#' 
#' While `NaN` is technically a valid value for grids stored in a
#' geopackage, it is generally prefered to use a numeric value, as `NaN` is not
#' supported by SQLite 3 and is stored in the metadata column as SQL `NULL`.
#' These defaults are used by [gpkg_write()] when `auto_nodata = TRUE`.
#'
#' For signed integer types, the minimum representable value (e.g., INT32_MIN)
#' is returned, as it is unlikely to occur in valid data. For unsigned integer
#' types, the maximum representable value is returned (e.g., UINT16_MAX), with
#' the exception of INT1U (byte) which returns 255 (following terra convention).
#' For float types, the negative extreme value is returned, as GeoPackage
#' metadata cannot represent NaN.
#' 
#' \tabular{ll}{
#'   \strong{Datatype} \tab \strong{Default NoData} \cr
#'   INT1S \tab -128 \cr
#'   INT2S \tab -32768 \cr
#'   INT4S \tab -2147483648 \cr
#'   INT8S \tab -9223372036854775808 \cr
#'   INT1U \tab 255 \cr
#'   INT2U \tab 65534 \cr
#'   INT4U \tab 4294967295 \cr
#'   INT8U \tab 18446744073709549568 \cr
#'   FLT4S \tab -3.40282346638528860e+38 \cr
#'   FLT8S \tab -1.79769313486231571e+308 \cr
#' }
#'
#' Note: The INT8U default (`18446744073709549568`) is `UINT64_MAX - 1101` to
#' account for floating-point precision loss when converting to double,
#' following terra's convention. R does not have a native unsigned 64-bit
#' integer type. 
#' 
#' @seealso [gpkg_write()] for using these defaults when writing rasters to
#'   GeoPackage
#'
#' @examples
#' gpkg_default_nodata("FLT4S")
#' gpkg_default_nodata("INT2S")
#' gpkg_default_nodata("INT2U") 
#'
#' @export
gpkg_default_nodata <- function(datatype) {
  # c.f. terra conventions from src/gdalio.cpp getNAvalue()
  # Signed types:  use INT*_MIN; Unsigned types:  use INT*_MAX or UINT*_MAX (or adjusted for precision)
  
  # Note: at this time (GDAL 3.12) only Byte (INT1U), Int16 (INT2S), UInt16
  # (INT2U) or Float32 (INT4S) supported. Additional values are provided for
  # completeness
  
  defaults <- list(
    # Unsigned integer types:  use maximum value
    INT1U = 255,                        # GDT_Byte (max value)
    INT2U = 65535,                      # UINT16_MAX
    INT4U = 4294967295,                 # UINT32_MAX
    INT8U = 18446744073709549568,       # UINT64_MAX - 1101 (precision adjustment per terra)
    
    # Signed integer types: use minimum value (INT*_MIN from stdint. h)
    INT1S = -128,                       # INT8_MIN
    INT2S = -32768,                     # INT16_MIN
    INT4S = -2147483648,                # INT32_MIN
    INT8S = -9223372036854775808,       # INT64_MIN
    
    # Float types: use negative extreme (NAN in terra, but invalid for GeoPackage metadata)
    FLT4S = -3.40282346638528860e+38,   # -FLT_MAX (IEEE 754 single precision)
    FLT8S = -1.79769313486231571e+308   # -DBL_MAX (IEEE 754 double precision)
  )
  
  if (datatype %in% names(defaults)) {
    return(defaults[[datatype]])
  } else {
    return(NA_real_)
  }
}

#' Get `gpkg_2d_gridded_coverage_ancillary` Table
#'
#' @param x A _geopackage_ object, path to a GeoPackage or an _SQLiteConnection_
#' @return a data.frame containing columns `id`, `tile_matrix_set_name`, `datatype`, `scale`, `offset`, `precision`, `data_null`, `grid_cell_encoding`, `uom`, `field_name`, `quantity_definition`
#' @export
gpkg_2d_gridded_coverage_ancillary <- function(x) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop('package `RSQLite` is required to get the `gpkg_2d_gridded_coverage_ancillary` table', call. = FALSE)
  }
  gpkg_table(x, "gpkg_2d_gridded_coverage_ancillary", collect = TRUE)
}


#' Get `gpkg_2d_gridded_tile_ancillary` Table
#'
#' @param x A _geopackage_ object, path to a GeoPackage or an _SQLiteConnection_
#' @return a data.frame containing columns `id`, `tile_matrix_set_name`, `datatype`, `scale`, `offset`, `precision`, `data_null`, `grid_cell_encoding`, `uom`, `field_name`, `quantity_definition`
#' @export
gpkg_2d_gridded_tile_ancillary <- function(x) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop('package `RSQLite` is required to get the `gpkg_2d_gridded_tile_ancillary` table', call. = FALSE)
  }
  gpkg_table(x, "gpkg_2d_gridded_tile_ancillary", collect = TRUE)
}