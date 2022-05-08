#' Validate a GeoPackage
#'
#' Checks for presence of required tables, valid values and other constraints.
#'
#' @param x Path to GeoPackages
#' @param diagnostics Return a list containing diagnostics (missing table names, invalid values, other errors)
#'
#' @return `TRUE` if valid. `FALSE` if one or more problems are found. For full diagnostics run with `diagnostics = TRUE` to return a list containing results for each input GeoPackage.
#' @export
gpkg_validate <- function(x, diagnostics = FALSE) {
  stop("This is not implemented yet")
}
