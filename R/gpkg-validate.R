#' Validate a GeoPackage
#'
#' Checks for presence of required tables, valid values and other constraints.
#'
#' @param x A _geopackage_ object, or _character_ path to GeoPackage
#' @param diagnostics Return a list containing individual diagnostic test results (see Details)
#' @details
#' Several tests are run on the input GeoPackage, including:
#'   - `required_tables`: _logical_. `TRUE` if `gpkg_contents` and `gpkg_spatial_ref_sys` tables exist
#'   - `has_contents`: _logical_. `TRUE` if the number of rows in `gpkg_contents` table is greater than `0` and all tables listed in `gpkg_contents` are in the database
#'   - `has_spatial_tables`: _logical_. `TRUE` if the number of tables in `gpkg_contents` with `data_type` `"features"` or `"2d-gridded-coverage"` is greater than `0`
#' 
#' @return _logical_. `TRUE` if valid. `FALSE` if one or more problems are found. For full diagnostics run with `diagnostics = TRUE` to return a list containing results for each test run on the input GeoPackage.
#' @export
gpkg_validate <- function(x, diagnostics = FALSE) {
  x <- .gpkg_connection_from_x(x)
  lt <- gpkg_list_tables(x)
  cg <- gpkg_contents(x)
  res <- list(
    required_tables = all(c("gpkg_contents","gpkg_spatial_ref_sys") %in% lt),
    has_contents = (nrow(cg) > 0) && all(cg$table_name %in% lt),
    has_spatial_tables = sum(cg$data_type %in% c("features", "2d-gridded-coverage")) > 0
  )
  if (is.null(diagnostics) || 
      length(diagnostics) == 0 ||
      nchar(diagnostics) == 0) {
    diagnostics <- names(res)
  } 
  if (is.character(diagnostics) || isTRUE(diagnostics)) {
    return(res[diagnostics])
  }
  all(sapply(res, isTRUE))
}
