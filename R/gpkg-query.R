#' Query a GeoPackage for tabular result
#'
#' @param x a _geopackage_ object
#' @param query a SQLite query
#'
#' @return a _data.frame_ result of `RSQLite::dbGetQuery()`
#' @export
gpkg_query <- function(x, query) {
  con <- .gpkg_connection_from_x(x)
  res <- RSQLite::dbGetQuery(con, query)
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  res
}
