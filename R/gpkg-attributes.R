#' Write or Remove Attribute Table in a GeoPackage
#' 
#' `gpkg_write_attributes()`: Specify a target geopackage and name for new table. For adding attributes, specify the new data as data.frame. The table name will be registered in the `gpkg_contents` table. Optionally include a custom `description` and/or use a `template`  object to define the spatial extent associated with attribute data.
#'
#' @param x A `geopackage` object
#' @param table A `data.frame`
#' @param table_name `character`. The name for `table` in `x`
#' @param description Optional description. Default `""`
#' @param template A `list` (containing elements `"ext"` and `"crs"`, or a `terra` object. These objects defining xmin/ymin/xmax/ymax and spatial reference system for the attribute table.
#' @param overwrite Overwrite? Default `FALSE`
#' @param append Append? Default `FALSE`
#' @return `logical`. `TRUE` on successful table write or remove.
#' @rdname gpkg-attributes
#' @export
gpkg_write_attributes <-  function(x,
                                   table,
                                   table_name,
                                   description = "",
                                   template = NULL,
                                   overwrite = FALSE,
                                   append = FALSE) {
  
  con <- .gpkg_connection_from_x(x)
  
  stopifnot(requireNamespace("RSQLite", quietly = TRUE))
  
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
  
  stopifnot(requireNamespace("RSQLite", quietly = TRUE))
  
  res <- list()
  if (!is.null(con)) {
    res <- lapply(table_name, function(y) {
      # remove existing table
      i <- RSQLite::dbRemoveTable(con, y)
      
      # remove gpkg_contents record
      if (y %in% gpkg_contents(x)$table_name) {
        gpkg_delete_contents(x, y)
      }
      i
    })
  }
  
  # close connection if needed
  if (attr(con, 'disconnect')) {
    gpkg_disconnect(x)
  }
  
  (sum(sapply(res, sum)) > 0)
}