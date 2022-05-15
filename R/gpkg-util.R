# GeoPackage utilities

#' Get Tables from a `geopackage` Object
#'
#' @param x a `geopackage` object
#'
#' @return a list of SpatVectorProxy, SpatRaster, data.frame (lazy tbl?)
#' @export
#' @rdname gpkg_tables
gpkg_tables <- function(x)
  UseMethod("gpkg_tables", x)

#' @export
#' @rdname gpkg_tables
gpkg_tables.geopackage <- function(x) {
  x$tables
}

#' List Tables in a GeoPackage
#' 
#' @param x A a `geopackage` object, path to a GeoPackage or an `SQLiteConnection`
#' @return a character vector with names of all tables and views in the database
#' @importFrom DBI dbListTables dbDisconnect
#' @export
gpkg_list_tables <- function(x) {
  con <- .gpkg_connection_from_x(x)
  res <- DBI::dbListTables(con)
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  res
}

#' Set `data_null` Metadata for a GeoPackage Tile Dataset
#' 
#' @param x A a `geopackage` object, path to a GeoPackage or an `SQLiteConnection`
#' @param name character. Tile matrix set name(s) (`tile_matrix_set_name`)
#' @param value numeric. Value to use as "NoData" (`data_null` value)
#' @param query_string logical. Return SQLite query rather than executing it? Default: `FALSE`
#' @return (invisibly) a scalar numeric that specifies the number of rows where `data_null` was updated
#' @importFrom DBI dbDisconnect
#' @export
gpkg_tile_set_data_null <- function(x, name, value, query_string = FALSE) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop('package `RSQLite` is required to set `data_null`', call. = FALSE)
  }
  
  con <- .gpkg_connection_from_x(x)
  # TODO: general function for T table name to set column X to scalar A where column Y is in vector B 
  q <- sprintf("UPDATE gpkg_2d_gridded_coverage_ancillary SET data_null = %s WHERE tile_matrix_set_name IN %s", 
               value, paste0("(", paste0(paste0("'", name, "'"), collapse = ","), ")"))
  if (query_string) return(q)
  res <- RSQLite::dbExecute(con, q)
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  invisible(res)
}

#' Get `gpkg_2d_gridded_coverage_ancillary` Table
#' 
#' @param x A a `geopackage` object, path to a GeoPackage or an `SQLiteConnection`
#' @return a data.frame containing columns `id`, `tile_matrix_set_name`, `datatype`, `scale`, `offset`, `precision`, `data_null`, `grid_cell_encoding`, `uom`, `field_name`, `quantity_definition`
#' @importFrom DBI dbDisconnect
#' @export
gpkg_2d_gridded_coverage_ancillary <- function(x) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop('package `RSQLite` is required to get the `gpkg_2d_gridded_coverage_ancillary` table', call. = FALSE)
  }
  con <- .gpkg_connection_from_x(x)
  res <- RSQLite::dbGetQuery(con, "SELECT * FROM gpkg_2d_gridded_coverage_ancillary")
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  res
}