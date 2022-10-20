#' Build `gpkg_contents` table based on existing tables
#'
#' @param x a _geopackage_ object
#'
#' @return logical. `TRUE` on successful update (all non-standard tables added to gpkg_contents table)
#' @export
gpkg_update_contents <- function(x) {
  contents <- try(gpkg_contents(x))
  if (inherits(contents, 'try-error')) {
    # create minimal gpkg_contents table
    .gpkg_create_contents(x)
  }
  tables <- gpkg_list_tables(x)
  tables_nonstandard <- tables[!grepl("^gpkg_.*|rtree_.*", tables)]
  todo <- tables_nonstandard[!tables_nonstandard %in% contents$table_name]

  # create gpkg_contents records, 
  # TODO: set extent (? is this needed for non-features/grids?)
  for (y in todo) {
    x <- gpkg_add_contents(x, y, y)
  }
  x
}

.gpkg_create_contents <- function(x) {
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

#' Add a record to `gpkg_contents`
#'
#' @param x A _geopackage_
#' @param table_name Name of table to add record for in gpkg_contents
#' @param description Default `""`
#' @param template Default `NULL` uses global EPSG:4326 with bounds -180,-90:180,90
#'
#' @return A _geopackage_
#' @export
gpkg_add_contents <- function(x, table_name, description = "", template = NULL) {
  
  if (!missing(template) && !is.null(template)) {
    
    # convert sf object to SpatVector
    if (inherits(template, 'sf')) {
      template <- terra::vect(template)
    }
    
    # template as a list
    if (is.list(template)) {
      ex <- template$ext
      cr <- template$crs
    }
    
    # template as terra object
    if (inherits(template, c("SpatRaster", "SpatVector", "SpatVectorProxy"))){
      ex <- as.numeric(terra::ext(template))
      cr <- as.character(terra::crs(template))
    }
  }
  
  # append to gpkg_contents
  # TODO: allow extent/CRS to be set based on `template` object
  #  for now probably fine to have global extent for attributes tables
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
                 "', -180, -90, 180, 90, 4326
      );"
               )
  )
  x
}
