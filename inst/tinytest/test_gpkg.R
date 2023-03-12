# test terra gpkg grids
stopifnot(requireNamespace("terra", quietly = TRUE))

dem <- system.file("extdata", "dem.tif", package = "gpkg")
stopifnot(nchar(dem) > 0)
gpkg_tmp <- tempfile(fileext = ".gpkg")

if (file.exists(gpkg_tmp))
  file.remove(gpkg_tmp)

# write a gpkg with two DEMs in it
gpkg_write(
  dem,
  destfile = gpkg_tmp,
  RASTER_TABLE = "DEM1",
  FIELD_NAME = "Elevation"
)

# overwrite=FALSE default
expect_error(gpkg_write(dem, gpkg_tmp))

gpkg_write(
  dem,
  destfile = gpkg_tmp,
  append = TRUE,
  RASTER_TABLE = "DEM2",
  FIELD_NAME = "Elevation",
  NoData = -9999
)

# create geopackage
g <- gpkg_connect(gpkg_tmp)
expect_true(gpkg_is_connected(g))

# expected tables are present
expect_true(all(
  c(
    gpkg_sqlite_tables$table_name,
    "DEM1", "DEM2"
  ) %in% gpkg_list_tables(g)
))

# disconnect
expect_true(gpkg_disconnect(g))

# create a geopackage then connect it
g <- geopackage(gpkg_tmp, connect = TRUE)

# add bounding polygon vector dataset
b <- terra::as.polygons(gpkg_tables(g)[["DEM1"]], ext = TRUE)
expect_silent(gpkg_write(b, destfile = gpkg_tmp, insert = TRUE))

d  <- data.frame(a = 1:10, b = LETTERS[1:10])
expect_silent(gpkg_write(list(myattr = d), destfile = gpkg_tmp, append = TRUE))

# enumerate tables
tl <- gpkg_list_tables(g)
expect_true(is.character(tl) && all(c("layer1", "myattr") %in% tl))

options(gpkg.use_dplyr = FALSE)
tlex <- gpkg_tables(g)
expect_equal(length(tlex), 4)

if (!inherits(try(requireNamespace("dbplyr", quietly = TRUE)), 'try-error')) {
  options(gpkg.use_dplyr = TRUE)
  tlex2 <- gpkg_tables(g)
  expect_equal(length(tlex2), 4)
}

# still connected
expect_true(gpkg_is_connected(g))

# extensions
expect_equal(gpkg_add_metadata_extension(g), 0)
expect_equal(gpkg_add_relatedtables_extension(g), 0)

# TODO
expect_error(gpkg_validate(g))

# disconnect it
expect_true(g <- gpkg_disconnect(g))

# cleanup
unlink(gpkg_tmp)
