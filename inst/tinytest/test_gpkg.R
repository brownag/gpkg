# test terra gpkg grids
stopifnot(requireNamespace("terra", quietly = TRUE))

dem <- system.file("extdata", "dem.tif", package = "gpkg")
stopifnot(nchar(dem) > 0)
gpkg_tmp <- tempfile(fileext = ".gpkg")

if (file.exists(gpkg_tmp))
  file.remove(gpkg_tmp)

# write a gpkg with two DEMs in it
RASTER_TABLE <- "DEM"

gpkg_write(
  dem,
  destfile = gpkg_tmp,
  RASTER_TABLE = RASTER_TABLE,
  FIELD_NAME = "Elevation"
)

# overwrite=FALSE default
expect_error(gpkg_write(dem, gpkg_tmp))

gpkg_write(
  dem,
  destfile = gpkg_tmp,
  append = TRUE,
  RASTER_TABLE = paste0(RASTER_TABLE, "1"),
  FIELD_NAME = "Elevation"
)

# create geopackage
g <- gpkg_connect(gpkg_tmp)
expect_true(gpkg_is_connected(g))

# expected tables are present
expect_true(all(
  c(
    gpkg_sqlite_tables$table_name,
    RASTER_TABLE, # gsub("\\.gpkg$", "", basename(gpkg_tmp)) # default
    paste0(RASTER_TABLE, "1")
  ) %in% gpkg_list_tables(g)
))

# disconnect
expect_true(gpkg_disconnect(g))

# create a geopackage then connect it
g <- geopackage(gpkg_tmp, connect = TRUE)

# add bounding polygon vector dataset
b <- terra::as.polygons(gpkg_tables(g)[[1]], ext = TRUE)
terra::writeVector(b, g$dsn, insert = TRUE)

# enumerate tables
tl <- gpkg_list_tables(g)
expect_true(is.character(tl) && basename(gsub("\\.gpkg", "", g$dsn)) %in% tl)

# still connected
expect_true(gpkg_is_connected(g))

# disconnect it
expect_true(gpkg_disconnect(g))

# cleanup
unlink(gpkg_tmp)
