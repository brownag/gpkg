#' Create a Spatial View
#'
#' @param g a `geopackage`
#' @param viewname _character_. Name of view.
#' @param viewquery _character_. Query for view contents. 
#' @param geom_column _character_. Column name of view geometry. Default: `"geom"`
#' @param geometry_type_name _character_. View geometry type. Default: `"GEOMETRY"`
#' @param data_type _character_. View data type. Default `"features"`
#' @param srs_id _integer_. Spatial Reference System ID. Default: `4326` (WGS84)
#' @param z _integer_. Default: `0`
#' @param m _integer_. Default: `0`
#'
#' @return _integer_. Returns `1` if a new record in `gpkg_geometry_columns` is successfully made.
#' @export
#'
gpkg_create_spatial_view <- function(g,
                                     viewname,
                                     viewquery, 
                                     geom_column = "geom",
                                     geometry_type_name = "GEOMETRY",
                                     data_type = "features",
                                     srs_id = 4326,
                                     z = 0,
                                     m = 0) {
gpkg_execute(g, sprintf("CREATE VIEW %s AS %s", viewname, viewquery))
gpkg_execute(g, sprintf("INSERT INTO gpkg_contents (table_name, identifier, data_type, srs_id) 
                        VALUES ('%s', '%s', '%s', %s)", 
                                viewname, viewname, data_type, srs_id))
gpkg_execute(g, sprintf("INSERT INTO gpkg_geometry_columns (table_name, column_name, geometry_type_name, srs_id, z, m) 
                        VALUES ('%s', '%s', '%s', %s, %s, %s)",
                                viewname, geom_column, geometry_type_name, srs_id, z, m))
}