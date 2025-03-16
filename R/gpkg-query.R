#' Query a GeoPackage for Tabular Result
#'
#' @param x A _geopackage_ object
#' @param query _character_. An SQLite/Spatialite/GeoPackage query. The `query`
#'   argument is forwarded to `sql` argument when `ogr=TRUE`.
#' @param ogr _logical_. Use the OGR query interface (via `terra::query()`). See
#'   details. Default: `FALSE` uses 'RSQLite' driver instead of 'terra'.
#' @param ... Additional arguments to `terra::query()` (such as  `start`, `n`,
#'   `vars`, `where`, `extent`, `filter`) are passed when `ogr=TRUE` (or using
#'   alias `gpkg_ogr_query()`). Otherwise not used.
#'
#' @details The GeoPackage driver supports OGR attribute filters. Provide
#'   filters in the SQLite dialect, as they will be executed directly against
#'   the database. If Spatialite is used, a recent version (4.2.0) is needed and
#'   cast operators are required to transform GeoPackage geometries to
#'   Spatialite geometries. A variety of SQL functions are available, see: <
#'   https://gdal.org/en/stable/drivers/vector/gpkg.html#sql-functions>
#'
#' @return a _data.frame_ result of `RSQLite::dbGetQuery()` or _SpatVector_
#'   result from `terra::query()`.
#' @export
#' @importFrom utils packageVersion
gpkg_query <- function(x, query, ogr = FALSE, ...) {
  res <- NULL
  if (isTRUE(ogr)) {
    if (!requireNamespace("terra") && utils::packageVersion("terra") < "1.7.33")
      stop("terra version 1.7-33 or higher is required to use `ogr=TRUE`")
    prx <- .gpkg_proxy_from_x(x)
    res <- terra::query(prx, ..., sql = query)
  } else {
    con <- .gpkg_connection_from_x(x)
    res <- RSQLite::dbGetQuery(con, query)
    if (attr(con, 'disconnect')) {
      DBI::dbDisconnect(con)
    }
  }
  res
}

#' @export
#' @description `gpkg_ogr_query()`: an alias for `gpkg_query(..., ogr=TRUE)`
#' @rdname gpkg_query
gpkg_ogr_query <- function(x, query, ...) {
  gpkg_query(x, query, ogr = TRUE, ...)
}
