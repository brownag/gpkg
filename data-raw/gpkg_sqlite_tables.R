## code to prepare `gpkg_sqlite_tables` dataset goes here
# TODO: add table metadata
gpkg_sqlite_tables <- data.frame(table_name = c(
  "gpkg_2d_gridded_coverage_ancillary",
  "gpkg_2d_gridded_tile_ancillary",
  "gpkg_contents",
  "gpkg_extensions",
  "gpkg_geometry_columns",
  "gpkg_ogr_contents",
  "gpkg_spatial_ref_sys",
  "gpkg_tile_matrix",
  "gpkg_tile_matrix_set",
  "sqlite_sequence"
))
usethis::use_data(gpkg_sqlite_tables, overwrite = TRUE)
