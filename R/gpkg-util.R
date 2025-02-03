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
gpkg_tables <- function(x, collect = FALSE, pragma = FALSE)
  UseMethod("gpkg_tables", x)

#' @export
#' @rdname gpkg_tables
gpkg_tables.geopackage <- function(x, collect = FALSE, pragma = FALSE) {
  src <- gpkg_source(x)
  xx <- .gpkg_connection_from_x(x)
  contents <- gpkg_contents(xx)
  y <- split(contents, contents$data_type)
  
  .LAZY.FUN <- ifelse(isTRUE(pragma), gpkg_table_pragma, 
                      function(x, ...) {
                        gpkg_table(x, ..., collect = collect)
                      })
                    
  unlist(lapply(names(y), function(z) {
    switch(z, 
           "2d-gridded-coverage" = { sapply(y[[z]]$table_name, function(i) terra::rast(src, i)) },
           "features" = { sapply(y[[z]]$table_name, function(i) {
             res <- try(terra::vect(src, proxy = !collect, layer = i), silent = TRUE)
             if (inherits(res, 'try-error')) {
               message(i, " : ", res[1])
               res <- .LAZY.FUN(xx, table_name = i)
             }
             res
            }) },
           "attributes" = { sapply(y[[z]]$table_name, function(i) list(.LAZY.FUN(xx, table_name = i))) })
  }), recursive = FALSE)
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