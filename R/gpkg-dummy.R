#' Create a Dummy Feature Dataset in a GeoPackage
#' 
#' This function has been deprecated. Please use `gpkg_create_empty_features()`.
#' 
#' Create a minimal (empty) feature table and `gpkg_geometry_columns` table entry.
#' 
#' @details This is a workaround so that `gpkg_vect()` (via `terra::vect()`) will recognize a GeoPackage as containing geometries and allow for use of OGR query utilities. The "dummy table" is not added to `gpkg_contents` and you should not try to use it for anything. The main purpose is to be able to use `gpkg_vect()` and `gpkg_ogr_query()` on a GeoPackage that contains only gridded and/or attribute data.
#' 
#' @seealso [gpkg_create_empty_features()] [gpkg_vect()] [gpkg_ogr_query()]
#'
#' @param x A _geopackage_ object
#' @param table_name A table name; default `"dummy_feature"`
#' @param values Values to use for new table. Defaults to default geometry name (`"geom"`), with generic `GEOMETRY` data type, with no spatial reference system.
#'
#' @return logical. `TRUE` on success.
#' @export
gpkg_create_dummy_features <- function(x, 
                                       table_name = "dummy_feature", 
                                       values = NULL) {
  
  .Deprecated("gpkg_create_empty_features")
  
  if (!is.null(values) && length(values) == 1 && is.character(values)) {
    values <- strsplit(gsub("\\(|\\)|'|\"", "", values), ",")[[1]]
    if (!length(values) == 6) {
      stop("Invalid `values` argument. Six values are required.", call. = FALSE)
    }
  } else {
    values <- c(table_name, 'geom', 'GEOMETRY', '-1', '0', '0')
  }
  
  res <- gpkg_create_empty_features(x,
                                    table_name = values[1],
                                    column_name = values[2],
                                    geometry_type_name = values[3],
                                    srs_id = as.integer(values[4]),
                                    z = as.integer(values[5]),
                                    m = as.integer(values[6]),
                                    contents = FALSE)
  
  !inherits(res, 'try-error')
}