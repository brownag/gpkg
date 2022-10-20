#' Write an attribute table to GeoPackage
#' 
#' Specify a target geopackage, data.frame of input data, and name for table.
#'
#' @param x a geopackage object
#' @param table a data.frame
#' @param table_name character. The name for `table` in `x`
#' @param description Optional description. Default `""`
#' @param template a list (containing elements `"ext"` and `"crs"`, or a terra object. These are defining xmin/ymin/xmax/ymax and CRS of the attribute table.
#' @param overwrite overwrite? Default `FALSE`
#' @param append append? Default `FALSE`
#'
#' @export
gpkg_write_attributes <-
  function(x,
           table,
           table_name,
           description = "",
           template = NULL,
           overwrite = FALSE,
           append = FALSE) {
    
  con <- .gpkg_connection_from_x(x)
  
  # write new table
  if (!is.null(con)) {
    RSQLite::dbWriteTable(con, table_name, table, overwrite = overwrite, append = append)
    x <- gpkg_add_contents(x, table_name = table_name, description = description, template = template)
  }

  # close connection if needed
  if (attr(con, 'disconnect')) {
    gpkg_disconnect(x)
  }
  x
}