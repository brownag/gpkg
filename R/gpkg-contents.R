
#' Add, Remove, Update and Create `gpkg_contents` table and records
#' @description `gpkg_add_contents()`: Add a record to `gpkg_contents`
#' @param x A _geopackage_
#' @param table_name Name of table to add or remove record for in _gpkg_contents_
#' @param description Default `""`
#' @param template Default `NULL` uses global EPSG:4326 with bounds -180,-90:180,90
#'
#' @return A _geopackage_
#' @rdname gpkg-contents
#' @export
gpkg_add_contents <- function(x, table_name, description = "", template = NULL) {
  
  if (!missing(template) && !is.null(template)) {
    # template as a list
    if (is.list(template) && all(c("ext", "srsid") %in% names(template))) {
      ex <- template$ext
      cr <- as.integer(template$srsid)
    } else {
      ## TODO: calculate ext from object, calculate srsid from WKT (?)
      # if (!requireNamespace("terra")) {
      #   stop("package `terra` is required to add contents with a custom extent", call. = FALSE)
      # }
      # 
      # # convert sf object to SpatVector
      # if (inherits(template, 'sf')) {
      #   template <- terra::vect(template)
      # }
      # 
      # # template as terra object
      # if (inherits(template, c("SpatRaster", "SpatVector", "SpatVectorProxy"))){
      #   ex <- as.numeric(terra::ext(template))
      #   cr <- as.character(terra::crs(template))
      # }
    }
  } else {
    ex <- c(-180, -90, 180, 90)
    cr <- 4326
  }
  
  # create gpkg_contents empty table if needed
  if (!"gpkg_contents" %in% gpkg_list_tables(x)){
    x <- gpkg_create_contents(x)
  }
  
  # append to gpkg_contents
  x <- gpkg_execute(x,
                    paste0(
                      "INSERT INTO gpkg_contents (table_name, data_type, identifier, 
                                  description, last_change,
                                  min_x, min_y, max_x, max_y, srs_id) 
       VALUES ('",
                      table_name ,
                      "', 'attributes', '",
                      table_name,
                      "', '",
                      description,
                      "','",
                      strftime(Sys.time(), '%Y-%m-%dT%H:%M:%OS3Z'),
                      "', ", ex[1], ", ", ex[2], ", ",
                             ex[3], ", ", ex[4], ", ", 
                             cr,"
                      );"
                    )
  )
  x
}

#' @description `gpkg_update_contents()`: Add and remove `gpkg_contents` records to match existing tables
#' @rdname gpkg-contents
#' @export
gpkg_update_contents <- function(x) {
  contents <- try(gpkg_contents(x))
  if (inherits(contents, 'try-error')) {
    # create minimal gpkg_contents table
    gpkg_create_contents(x)
  }
  tables <- gpkg_list_tables(x)
  tables_nonstandard <- tables[!grepl("^gpkg_.*|rtree_.*", tables)]
  todo <- tables_nonstandard[!tables_nonstandard %in% contents$table_name]
  torem <- contents$table_name[!contents$table_name %in% tables]
  
  # create gpkg_contents records, 
  # TODO: set extent via template?
  for (y in todo) {
    x <- gpkg_add_contents(x, table_name = y, description = y)
  }
  for (y in torem) {
    x <- gpkg_delete_contents(x, table_name = y)
  }
  
  x
}

#' @description `gpkg_delete_contents()`: Delete a record from `gpkg_contents` based on table name
#' @rdname gpkg-contents
#' @export
gpkg_delete_contents <- function(x, table_name) {
  gpkg_execute(x, paste0("DELETE FROM gpkg_contents WHERE table_name = ", table_name))
}

#' @description `gpkg_create_contents()`: Create an empty `gpkg_contents` table
#' @rdname gpkg-contents
#' @export
gpkg_create_contents <- function(x) {
  q <- "CREATE TABLE gpkg_contents (
          table_name TEXT NOT NULL PRIMARY KEY,
          data_type TEXT NOT NULL,
          identifier TEXT UNIQUE,
          description TEXT DEFAULT '',
          last_change DATETIME NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
          min_x DOUBLE,
          min_y DOUBLE,
          max_x DOUBLE,
          max_y DOUBLE,
          srs_id INTEGER,
          CONSTRAINT fk_gc_r_srs_id FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys(srs_id)
        )"
  gpkg_execute(x, q)
}
