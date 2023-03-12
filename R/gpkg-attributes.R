#' Write or Remove Attribute Table in a GeoPackage
#' 
#' `gpkg_write_attributes()`: Specify a target geopackage and name for new table. For adding attributes, specify the new data as data.frame. The table name will be registered in the `gpkg_contents` table. Optionally include a description and/or a template terra object that can be used to define the extent of the attribute data.
#'
#' @param x a geopackage object
#' @param table a data.frame
#' @param table_name character. The name for `table` in `x`
#' @param description Optional description. Default `""`
#' @param template a list (containing elements `"ext"` and `"crs"`, or a terra object. These are defining xmin/ymin/xmax/ymax and CRS of the attribute table.
#' @param overwrite overwrite? Default `FALSE`
#' @param append append? Default `FALSE`
#' @rdname gpkg-attributes
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
    RSQLite::dbWriteTable(con, table_name, table, overwrite = overwrite, append = !overwrite && append)
    res <- gpkg_delete_contents(x, table_name = table_name) + 
            gpkg_add_contents(x, table_name = table_name, description = description, template = template)
  }

  # close connection if needed
  if (attr(con, 'disconnect')) {
    gpkg_disconnect(x)
  }
  (res == 2)
}

#' @description `gpkg_remove_attributes()`: Remove an attribute table and corresponding `gpkg_contents` record
#' @rdname gpkg-attributes
#' @export
gpkg_remove_attributes <- function(x, table_name) {
  
  con <- .gpkg_connection_from_x(x)
  
  # write new table
  if (!is.null(con)) {
    for (y in table_name) {
      RSQLite::dbRemoveTable(con, y)
      if (y %in% gpkg_contents(x)$table_name) {
        gpkg_delete_contents(x, y)
      }
    }
  }
  
  # close connection if needed
  if (attr(con, 'disconnect')) {
    gpkg_disconnect(x)
  }
  
  x
}