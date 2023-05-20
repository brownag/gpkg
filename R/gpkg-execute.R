#' Execute an SQL statement in a GeoPackage
#'
#' @param x A _geopackage_ object
#' @param statement An SQLite statement
#' @param ... Additional arguments to `RSQLite::dbExecute()`
#' @param silent Used to suppress error messages, passed to `try()`. Default: `FALSE`.
#' @return Invisible result of `RSQLite::dbExecute()`; or `try-error` on error.
#' 
#' @export
#' 
gpkg_execute <- function(x, statement, ..., silent = FALSE) {
  con <- .gpkg_connection_from_x(x)
  res <- try(RSQLite::dbExecute(con, statement, ...), silent = silent)
  if (attr(con, 'disconnect')) DBI::dbDisconnect(con)
  invisible(res)
}
