#' Create an empty grid table
#'
#'
#' Create an empty grid table and associated entries for `gpkg_spatial_ref_sys`, `gpkg_2d_gridded_coverage_ancillary`, and `gpkg_2d_gridded_tile_ancillary`.
#'
#' @param x A _geopackage_ object
#' @param tile_matrix_set_name _character_. New table name.
#' @param datatype _character_. Either `"integer"` (default) or `"float"`.
#' @param scale _numeric_. Default: `1.0` 
#' @param offset _numeric_. Default: `0.0`
#' @param precision _numeric_. Default: `1.0`
#' @param data_null _numeric_. Default: `NULL`
#' @param grid_cell_encoding _character_ Default: `"grid-value-is-center"`
#' @param uom _character_. Unit of measure. Default: `NULL`
#' @param field_name _character_. Default: `"Height"`
#' @param quantity_definition _character_. Default: `"Height"`
#' @param srs_id _integer_. Spatial Reference System ID. Must be defined in `gpkg_spatial_ref_sys` table. 
#'
#' @return _integer_
#' @export
gpkg_create_empty_grid <- function(x, 
                                   tile_matrix_set_name,
                                   datatype = "integer",
                                   scale = 1.0,
                                   offset = 0.0,
                                   precision = 1.0,
                                   data_null = NULL,
                                   grid_cell_encoding = "grid-value-is-center",
                                   uom = NULL,
                                   field_name = "Height",
                                   quantity_definition = "Height",
                                   srs_id = 4326, 
                                   z = 0L,
                                   m = 0L,
                                   contents = TRUE,
                                   description = "",
                                   ext = c(-180, -90, 180, 90)) {
  # create contents?
  gpkg_create_contents(x)
  
  gpkg_create_extensions(x)
  
  # create srs, coverage, tile tables if needed
  gpkg_create_spatial_ref_sys(x)
  gpkg_create_2d_gridded_coverage_ancillary(x)
  gpkg_create_2d_gridded_tile_ancillary(x)
  gpkg_add_2d_gridded_coverage_extensions(x)
  
  gpkg_add_contents(
    x,
    table_name = tile_matrix_set_name,
    data_type = "2d-gridded-coverage",
    description = description,
    srs_id = srs_id,
    ext = ext
  )
  
  tb <- data.frame(
      id = 1L,
      zoom_level = 0L,
      tile_column = 0L,
      tile_row = 0L,
      tile_data = structure(c(tile_data = raw(1)), ptype = raw(0), class = "list")
    )
  
  gpkg_create_table(x, table_name = tile_matrix_set_name, fields = tb[0,])
  
  value <- data.frame(
    table_name = tile_matrix_set_name,
    column_name = "tile_data",
    extension_name = "gpkg_2d_gridded_coverage",
    definition = "http://docs.opengeospatial.org/is/17-066r1/17-066r1.html",
    scope = "read-write"
  )
  
  gpkg_append_table(x, "gpkg_extensions", value)
  
  if (is.null(data_null))
    data_null <- NA
  
  if (is.null(uom))
    uom <- ""
  
  gpkg_append_table(
    x,
    "gpkg_2d_gridded_coverage_ancillary",
    value = data.frame(
      id = 1,
      tile_matrix_set_name = tile_matrix_set_name,
      datatype = datatype,
      scale = scale,
      offset = offset,
      precision = precision,
      data_null = data_null,
      grid_cell_encoding = grid_cell_encoding,
      uom = uom,
      field_name = field_name,
      quantity_definition = quantity_definition
    )
  )
  
}

gpkg_tile_matrix_set <- function(x) {
  
}

gpkg_tile_matrix <- function(x) {
  
} 

gpkg_create_tile_matrix <- function(x) {
  value <- data.frame(table_name = "test", zoom_level = 0L, matrix_width = 1L, 
                      matrix_height = 1L, tile_width = 256L, tile_height = 256L, 
                      pixel_x_size = 1, pixel_y_size = 1)
  gpkg_create_table(x, "gpkg_tile_matrix", value[0,])
}

gpkg_create_tile_matrix_set <- function(x) {
  
}