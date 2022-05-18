#' Add Metadata extension
#'
#' @param x a `geopackage`
#' @export
gpkg_add_metadata_extension <- function(x) {
  # TODO: insert if not exists
  RSQLite::dbExecute(x$con, "INSERT INTO gpkg_extensions(table_name,column_name,extension_name,definition,scope) VALUES (
  'gpkg_metadata', NULL, 'gpkg_metadata', 'http://www.geopackage.org/spec121/#extension_metadata', 'read-write'
  )")
  RSQLite::dbExecute(x$con, "INSERT INTO gpkg_extensions(table_name,column_name,extension_name,definition,scope) VALUES (
  'gpkg_metadata_reference', NULL, 'gpkg_metadata', 'http://www.geopackage.org/spec121/#extension_metadata', 'read-write'
  )")

  RSQLite::dbExecute(x$con, "CREATE TABLE gpkg_metadata (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    md_scope TEXT NOT NULL DEFAULT 'dataset',
    md_standard_uri TEXT NOT NULL,
    mime_type TEXT NOT NULL DEFAULT 'text/xml',
    metadata TEXT NOT NULL DEFAULT ''
  );")

  RSQLite::dbExecute(x$con, "CREATE TABLE gpkg_metadata_reference (
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
}

#' Add Related Tables extension
#'
#' @param x a `geopackage`
#' @export
gpkg_add_relatedtables_extension <- function(x) {
  RSQLite::dbExecute(x$con, "INSERT INTO gpkg_extensions(table_name,column_name,extension_name,definition,scope) VALUES (
    'gpkgext_relations', NULL, 'related_tables', 'http://docs.opengeospatial.org/is/18-000/18-000.html#_gpkg_extensions', 'read-write'
  )")

  RSQLite::dbExecute(x$con, "CREATE TABLE 'gpkgext_relations' (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    base_table_name TEXT NOT NULL,
    base_primary_column TEXT NOT NULL DEFAULT 'id',
    related_table_name TEXT NOT NULL,
    related_primary_column TEXT NOT NULL DEFAULT 'id',
    relation_name TEXT NOT NULL,
    mapping_table_name TEXT NOT NULL UNIQUE
   );")

}
