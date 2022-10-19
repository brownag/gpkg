#' Execute an SQL statement in a GeoPackage
#'
#' @param x a _geopackage_ object
#' @param statement a SQLite statement
#'
#' @return a _data.frame_ result of `RSQLite::dbGetQuery()`
#' @export
gpkg_execute <- function(x, statement) {
  con <- .gpkg_connection_from_x(x)
  res <- RSQLite::dbExecute(con, statement)
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  res
}
