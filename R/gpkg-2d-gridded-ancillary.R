#' Create 2D Gridded Coverage and Tile Ancillary Tables
#'
#' @param x A _geopackage_ object
#'
#' @references http://www.opengis.net/doc/IS/geopackage-gr/1.0
#' 
#' @return `gpkg_create_2d_gridded_coverage_ancillary()`: _integer_. Result of running sequential `gpkg_execute()` statements. 
#' @export
#' @rdname gridded-ancillary
gpkg_create_2d_gridded_coverage_ancillary <- function(x) {
  res <- 0
  if (!"gpkg_2d_gridded_coverage_ancillary" %in% gpkg_list_tables(x))
    res <- gpkg_execute(x, "CREATE TABLE 'gpkg_2d_gridded_coverage_ancillary' (
            id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
            tile_matrix_set_name TEXT NOT NULL UNIQUE,
            datatype TEXT NOT NULL DEFAULT 'integer',
            scale REAL NOT NULL DEFAULT 1.0,
            offset REAL NOT NULL DEFAULT 0.0,
            precision REAL DEFAULT 1.0,
            data_null REAL,
            grid_cell_encoding TEXT DEFAULT 'grid-value-is-center',
            uom TEXT,
            field_name TEXT DEFAULT 'Height',
            quantity_definition TEXT DEFAULT 'Height',
            CONSTRAINT fk_g2dgtct_name FOREIGN KEY('tile_matrix_set_name') REFERENCES
            gpkg_tile_matrix_set ( table_name )
            CHECK (datatype in ('integer','float')));")
  res
}

#' @return `gpkg_create_2d_gridded_tile_ancillary()`: _integer_. Result of running sequential `gpkg_execute()` statements.
#' @export
#' @rdname gridded-ancillary
gpkg_create_2d_gridded_tile_ancillary <- function(x) {
  res <- 0
  if (!"gpkg_2d_gridded_tile_ancillary" %in% gpkg_list_tables(x))
    res <- gpkg_execute(x, "CREATE TABLE gpkg_2d_gridded_tile_ancillary (
            id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
            tpudt_name TEXT NOT NULL,
            tpudt_id INTEGER NOT NULL,
            scale REAL NOT NULL DEFAULT 1.0,
            offset REAL NOT NULL DEFAULT 0.0,
            min REAL DEFAULT NULL,
            max REAL DEFAULT NULL,
            mean REAL DEFAULT NULL,
            std_dev REAL DEFAULT NULL,
            CONSTRAINT fk_g2dgtat_name FOREIGN KEY (tpudt_name) REFERENCES gpkg_contents(table_name),
            UNIQUE (tpudt_name, tpudt_id));")
  res
}