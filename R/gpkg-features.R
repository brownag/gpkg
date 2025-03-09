# gpkg feature tables

#' Create an empty feature table
#' 
#' Create an empty feature table and associated entries for `gpkg_spatial_ref_sys`, and `gpkg_geometry_columns`.
#'
#' @param x A `geopackage` Object 
#' @param table_name _character_. New table name.
#' @param geometry_type_name _character_. Geometry type name. Default: `"GEOMETRY"`
#' @param column_name _character_. Geometry column name. Default `"geom"`
#' @param srs_id _integer_. Spatial Reference System ID. Must be defined in `gpkg_spatial_ref_sys` table. 
#' @param z _integer_. Default: `0`
#' @param m _integer_. Default: `0`
#' @param contents _logical_. If `TRUE` (default) add the new table to `gpkg_contents` table.
#' @param description _character_. Description for `gpkg_contents` table. Default: `""`
#' @param ext _numeric_. A numeric vector of length four specifying the bounding box extent.
#' 
#' @return _integer_ result of `gpkg_execute()`. Returns `1` if a new geometry record is appended to `gpkg_geometry_columns` table.
#' 
#' @export
#' @seealso [gpkg_create_empty_grid()]
#' @rdname gpkg-features
gpkg_create_empty_features <- function(x, 
                                       table_name, 
                                       column_name = "geom", 
                                       geometry_type_name = "GEOMETRY",
                                       srs_id = 4326,
                                       z = 0L,
                                       m = 0L,
                                       contents = TRUE,
                                       description = "",
                                       ext = c(-180, -90, 180, 90)) {
  con <- .gpkg_connection_from_x(x)
  gpkg_create_spatial_ref_sys(con)
  gpkg_create_geometry_columns(con)
  
  res <- 0
  if (!table_name %in% gpkg_list_tables(con)) {
    res <- gpkg_execute(con, paste0("CREATE TABLE ", table_name, " (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        geom ", geometry_type_name, ");"))
  }
  
  if (!inherits(res, 'try-error') && res == 0) {
    if (contents) {
      gpkg_add_contents(con, 
                        data_type = "features",
                        table_name = table_name, 
                        description = description,
                        srs_id = srs_id,
                        ext = ext)
    }
    
    res <- gpkg_add_geometry_columns(
      con,
      table_name = table_name,
      column_name = column_name,
      geometry_type_name = geometry_type_name,
      srs_id = srs_id,
      z = z,
      m = m
    )
  }
  
  if (attr(con, 'disconnect')) {
    gpkg_disconnect(con)
  }
  
  res
}
