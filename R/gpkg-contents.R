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

  # TODO: create table records, set extent (? is this needed for non-features/grids?), update table
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
