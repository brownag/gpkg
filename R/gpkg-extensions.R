#' Get Geopackage Extensions
#' @param x A `geopackage`
#' @return a data.frame
#' @export
gpkg_extensions <- function(x) {
  gpkg_collect(x, "gpkg_extensions")
}

#' Add 'Metadata' extension
#'
#' Adds the "Metadata" extension tables.
#'
#' @param x a `geopackage`
#' @return `0` (invisible). Called for side effects.
#' @export
gpkg_add_metadata_extension <- function(x) {
  tbls <- gpkg_list_tables(x)
  
  if (!"gpkg_extensions" %in% tbls)
    .gpkg_add_extensions(x, tbls)
  
  # TODO: insert if not exists
  RSQLite::dbExecute(x$env$con, "INSERT INTO gpkg_extensions(table_name,column_name,extension_name,definition,scope) VALUES (
  'gpkg_metadata', NULL, 'gpkg_metadata', 'http://www.geopackage.org/spec121/#extension_metadata', 'read-write'
  )")
  RSQLite::dbExecute(x$env$con, "INSERT INTO gpkg_extensions(table_name,column_name,extension_name,definition,scope) VALUES (
  'gpkg_metadata_reference', NULL, 'gpkg_metadata', 'http://www.geopackage.org/spec121/#extension_metadata', 'read-write'
  )")

  if (!"gpkg_metadata" %in% tbls)
    RSQLite::dbExecute(x$env$con, "CREATE TABLE gpkg_metadata (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      md_scope TEXT NOT NULL DEFAULT 'dataset',
      md_standard_uri TEXT NOT NULL,
      mime_type TEXT NOT NULL DEFAULT 'text/xml',
      metadata TEXT NOT NULL DEFAULT ''
    );")
  
  if (!"gpkg_metadata_reference" %in% tbls)
    RSQLite::dbExecute(x$env$con, "CREATE TABLE gpkg_metadata_reference (
      reference_scope TEXT NOT NULL,
      table_name TEXT,
      column_name TEXT,
      row_id_value INTEGER,
      timestamp DATETIME NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
      md_file_id INTEGER NOT NULL,
      md_parent_id INTEGER,
      CONSTRAINT crmr_mfi_fk FOREIGN KEY (md_file_id) REFERENCES gpkg_metadata(id),
      CONSTRAINT crmr_mpi_fk FOREIGN KEY (md_parent_id) REFERENCES gpkg_metadata(id)
    );")
  invisible(0)
}

#' Add 'Related Tables' extension
#'
#' Adds the "Related Tables" extension tables.
#' 
#' @param x a `geopackage`
#' @return `0` (invisible). Called for side effects.
#' @export
gpkg_add_relatedtables_extension <- function(x) {
  
  tbls <- gpkg_list_tables(x)
  
  if (!"gpkg_extensions" %in% tbls)
    .gpkg_add_extensions(x, tbls)
    
  RSQLite::dbExecute(x$env$con, "INSERT INTO gpkg_extensions(table_name,column_name,extension_name,definition,scope) VALUES (
    'gpkgext_relations', NULL, 'related_tables', 'http://docs.opengeospatial.org/is/18-000/18-000.html#_gpkg_extensions', 'read-write'
  )")
  
  if (!"gpkgext_relations" %in% tbls)
    RSQLite::dbExecute(x$env$con, "CREATE TABLE 'gpkgext_relations' (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      base_table_name TEXT NOT NULL,
      base_primary_column TEXT NOT NULL DEFAULT 'id',
      related_table_name TEXT NOT NULL,
      related_primary_column TEXT NOT NULL DEFAULT 'id',
      relation_name TEXT NOT NULL,
      mapping_table_name TEXT NOT NULL UNIQUE
     );")
  0
}

gpkg_add_spatial_ref_sys_extensions <- function(x) {
  value <- data.frame(
    table_name = "gpkg_spatial_ref_sys",
    column_name = "definition_12_063",
    extension_name = "gpkg_crs_wkt",
    definition = "http://www.geopackage.org/spec120/#extension_crs_wkt",
    scope = "read-write"
  )
  gpkg_append_table(x, "gpkg_extensions", value)
}


gpkg_add_2d_gridded_coverage_extensions <- function(x) {
  value <- data.frame(
    table_name = c(
      "gpkg_2d_gridded_coverage_ancillary",
      "gpkg_2d_gridded_tile_ancillary"
    ),
    column_name = NA_character_,
    extension_name = "gpkg_2d_gridded_coverage",
    definition = "http://docs.opengeospatial.org/is/17-066r1/17-066r1.html",
    scope = "read-write"
  )
  gpkg_append_table(x, "gpkg_extensions", value)
}


gpkg_create_extensions <- function(x) {
  x <- .gpkg_connection_from_x(x)
  .gpkg_add_extensions(x)
  if (attr(x, 'disconnect')) {
    DBI::dbDisconnect(x)
  }
}

.gpkg_add_extensions <- function(x, tbls = gpkg_list_tables(x)) {
  
  if (inherits(x, 'geopackage'))
    x <- x$env$con
  
  if (!"gpkg_extensions" %in% tbls)
    RSQLite::dbExecute(x, "CREATE TABLE gpkg_extensions (
        table_name TEXT,
        column_name TEXT,
        extension_name TEXT NOT NULL,
        definition TEXT NOT NULL,
        scope TEXT NOT NULL,
        CONSTRAINT ge_tce UNIQUE (table_name, column_name, extension_name)
      );")
}