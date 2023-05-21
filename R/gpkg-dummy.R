#' Create a Dummy Feature Dataset in a GeoPackage
#' 
#' This function creates a minimal (empty) feature table and `gpkg_geometry_columns` table entry.
#' 
#' @details This is a workaround so that `gpkg_vect()` (via `terra::vect()`) will recognize a GeoPackage as containing geometries and allow for use of OGR query utilities. The "dummy table" is not added to `gpkg_contents` and you should not try to use it for anything. The main purpose is to be able to use `gpkg_vect()` and `gpkg_ogr_query()` on a GeoPackage that contains only gridded and/or attribute data.
#' 
#' @seealso [gpkg_vect()] [gpkg_ogr_query()]
#'
#' @param x A _geopackage_ object
#' @param table_name A table name; default `"dummy_feature"`
#' @param values Values to use for new table. Defaults to default geometry name (`"geom"`), with generic `GEOMETRY` data type, with no spatial reference system.
#'
#' @return logical. `TRUE` on success.
#' @export
gpkg_create_dummy_features <- function(x, table_name = "dummy_feature", 
                                       values = paste0("'", table_name, "', 
                                                       'geom', 'GEOMETRY', -1, 0, 0")) {

  res <- gpkg_execute(x, "CREATE TABLE dummy_feature (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    geom GEOMETRY
  );")
  
  if (isTRUE(res)) {
    res <- gpkg_execute(x, paste0(
      "INSERT INTO gpkg_geometry_columns (table_name, column_name, 
                                          geometry_type_name, srs_id, z, m) 
       VALUES (", values, ");"
    ))
  }
  
  !inherits(res, 'try-error')
}