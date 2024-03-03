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
  x <- .gpkg_connection_from_x(x)
  lt <- gpkg_list_tables(x)
  cg <- gpkg_contents(x)
  res <- list(
    required_tables = all(c("gpkg_contents","gpkg_spatial_ref_sys") %in% lt),
    has_contents = (nrow(cg) > 0),
    has_spatial_tables = all(cg$table_name %in% lt) &&
                          sum(cg$data_type %in% 
                                c("features", "2d-gridded-coverage")) > 0
  )
  if (is.null(diagnostics) || 
      length(diagnostics) == 0 ||
      nchar(diagnostics) == 0) {
    diagnostics <- names(res)
  } 
  if (is.character(diagnostics) || isTRUE(diagnostics)) {
    return(res[diagnostics])
  }
  all(sapply(res[diagnostics], isTRUE))
}
