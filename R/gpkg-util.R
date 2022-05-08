# GeoPackage utilities

#' Get Tables from a `geopackage` Object
#'
#' @param x a `geopackage`
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
#' @param x A a `geopackage`, path to a GeoPackage or an `SQLiteConnection`
#' @return a character vector with names of all tables and views in the database
#' @importFrom DBI dbListTables
#' @export
gpkg_list_tables <- function(x) {
  con <- .connection_from_x(x)
  res <- DBI::dbListTables(con)
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  res
}

#' .connection_from_x
#' @param x A `geopackage`, a path to a GeoPackage or an `SQLiteConnection`
#' @return An SQLiteConnection with logical attribute `"disconnect"` indicating whether it should be disconnected after use.
#' @noRd
#' @keywords internal
.connection_from_x <- function(x) {
  disconnect <- TRUE
  if (is.character(x)) {
    con <- gpkg_connect(x)$con
  } else if (inherits(x, 'geopackage')) {
    if (!gpkg_is_connected(x)) {
      p <- x$dsn
      con <- gpkg_connect(p)$con
    } else {
      con <- x$con
      disconnect <- FALSE
    }
  } else if (inherits(x, 'SQLiteConnection')) {
    con <- x
    disconnect <- FALSE
  } else {
    stop('`x` should be a `geopackage`, a path to a GeoPackage or an `SQLiteConnection`')
  }
  attr(con, 'disconnect') <- disconnect
  con
}