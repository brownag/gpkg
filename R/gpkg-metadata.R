gpkg_create_metadata <- function(x) {
  value <- data.frame(
    id = 1L,
    md_scope = "dataset",
    md_standard_uri = "http://gdal.org",
    mime_type = "text/xml",
    metadata = "<GDALMultiDomainMetadata>\n  <Metadata domain=\"BAND_1\">\n    <MDI key=\"STATISTICS_MAXIMUM\">nan</MDI>\n    <MDI key=\"STATISTICS_MEAN\">-9999</MDI>\n    <MDI key=\"STATISTICS_MINIMUM\">nan</MDI>\n    <MDI key=\"STATISTICS_STDDEV\">-9999</MDI>\n  </Metadata>\n</GDALMultiDomainMetadata>\n"
  )
  gpkg_create_table(x, "gpkg_metadata", value[0,])
}

gpkg_create_metadata_reference <- function(x) {
  value <- data.frame(
    reference_scope = "table",
    table_name = "test",
    column_name = NA_character_,
    row_id_value = NA_integer_,
    timestamp = "2024-12-01T21:56:42.565Z",
    md_file_id = 1L,
    md_parent_id = NA_integer_
  )
  gpkg_create_table(x, "gpkg_metadata_reference", value[0,])
}

gpkg_metadata <- function(x) {
  gpkg_collect(x, "gpkg_metadata")
}

gpkg_metadata_reference <- function(x) {
  gpkg_collect(x, "gpkg_metadata_reference")
}