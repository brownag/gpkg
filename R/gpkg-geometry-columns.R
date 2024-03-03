#' GeoPackage Geometry Columns
#' 
#' Create `gpkg_geometry_columns` table to account for geometry columns within the database with `gpkg_create_geometry_columns()`. Register new geometry columns with `gpkg_add_geometry_columns()`.
#'
#' @param x A _geopackage_ object, or path to GeoPackage file.
#'
#' @return _integer_. `1` if table created or row inserted successfully, `0` otherwise.
#' @export
#' @rdname gpkg-geometry-columns
gpkg_create_geometry_columns <- function(x) {
  res <- 0
  if (!"gpkg_geometry_columns" %in% gpkg_list_tables(x))
    res <- gpkg_execute(x, "CREATE TABLE gpkg_geometry_columns (
            table_name TEXT NOT NULL,
            column_name TEXT NOT NULL,
            geometry_type_name TEXT NOT NULL,
            srs_id INTEGER NOT NULL,
            z TINYINT NOT NULL,
            m TINYINT NOT NULL,
            CONSTRAINT pk_geom_cols PRIMARY KEY (table_name, column_name),
            CONSTRAINT uk_gc_table_name UNIQUE (table_name),
            CONSTRAINT fk_gc_tn FOREIGN KEY (table_name) REFERENCES gpkg_contents (table_name),
            CONSTRAINT fk_gc_srs FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys (srs_id));")
  res
}

#' @param table_name _character_. New table name.
#' @param geometry_type_name _character_. Geometry type name. Default: `"GEOMETRY"`
#' @param column_name _character_. Geometry column name. Default `"geom"`
#' @param srs_id _integer_. Spatial Reference System ID. Must be defined in `gpkg_spatial_ref_sys` table. 
#' @param z _integer_. Default: `0`
#' @param m _integer_. Default: `0`
#'
#' @export
#' @rdname gpkg-geometry-columns
gpkg_add_geometry_columns <- function(x,
                                      table_name,
                                      column_name,
                                      geometry_type_name = "GEOMETRY",
                                      srs_id, z, m) {
  res <- 0
  if (!table_name %in% gpkg_collect(x, "gpkg_geometry_columns")$table_name) {
    values <- paste0("'", table_name, "', '", column_name,
                     "', '", geometry_type_name, "', ",
                     srs_id, ", ", z, ", ", m)
    res <- gpkg_execute(x, paste0(
      "INSERT INTO gpkg_geometry_columns 
        (table_name, column_name, geometry_type_name, srs_id, z, m) 
       VALUES (", values, ");"
    ))
  }
  res
}