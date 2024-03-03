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
gpkg_create_dummy_features <- function(x, 
                                       table_name = "dummy_feature", 
                                       values = NULL) {
  if (is.null(values)) {
    values <- paste0("'", table_name, "', 'geom', 'GEOMETRY', -1, 0, 0")
  }
  
  res <- 0
  if (!table_name %in% gpkg_list_tables(x)) {
    res <- gpkg_execute(x, paste0("CREATE TABLE ", table_name, " (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      geom GEOMETRY
    );"))
  }
  
  gpkg_create_spatial_ref_sys(x)
  
  if (!inherits(res, 'try-error') && res == 0 && 
      !"gpkg_geometry_columns" %in% gpkg_list_tables(x)) {
    res <- gpkg_execute(x, " CREATE TABLE gpkg_geometry_columns (
      table_name TEXT NOT NULL,
      column_name TEXT NOT NULL,
      geometry_type_name TEXT NOT NULL,
      srs_id INTEGER NOT NULL,
      z TINYINT NOT NULL,
      m TINYINT NOT NULL,
      CONSTRAINT pk_geom_cols PRIMARY KEY (table_name, column_name),
      CONSTRAINT uk_gc_table_name UNIQUE (table_name),
      CONSTRAINT fk_gc_tn FOREIGN KEY (table_name) REFERENCES gpkg_contents(table_name),
      CONSTRAINT fk_gc_srs FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys (srs_id));")
  }
  
  if (!inherits(res, 'try-error') && res == 0) {
    if (!table_name %in% gpkg_collect(x, "gpkg_geometry_columns")$table_name) {
      res <- gpkg_execute(x, paste0(
        "INSERT INTO gpkg_geometry_columns (table_name, column_name, 
                                            geometry_type_name, srs_id, z, m) 
         VALUES (", values, ");"
      ))
    }
  }
  
  !inherits(res, 'try-error')
}