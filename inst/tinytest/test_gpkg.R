# cleanup
if (interactive()) 
  rm(list = ls())

# RSQLite and terra used heavily in tests
# d(b)plyr used conditionally
if (requireNamespace("tinytest", quietly = TRUE)) library(tinytest)
stopifnot(requireNamespace("RSQLite", quietly = TRUE))
stopifnot(requireNamespace("terra", quietly = TRUE))

dem <- system.file("extdata", "dem.tif", package = "gpkg")
stopifnot(nchar(dem) > 0)
gpkg_tmp <- tempfile(fileext = ".gpkg")

# basic error conditions
if (Sys.info()["sysname"] != "Windows")
  expect_error(suppressWarnings(geopackage(connect = TRUE, tmpdir = ""))) # permission denied
expect_error(.gpkg_connection_from_x(NULL)) # empty reference
expect_true(inherits(gpkg_execute(gpkg_tmp, "select * from foo", silent = TRUE), 'try-error')) # nonexistent table

if (file.exists(gpkg_tmp))
  unlink(gpkg_tmp)

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
expect_message(geopackage(g))
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
expect_true(inherits(g, 'geopackage'))
expect_stdout(print(g))

# without connecting
g0 <- geopackage(gpkg_tmp)
expect_true(inherits(g0, 'geopackage'))

# from existing connection
g1 <- geopackage(gpkg_source(g))
expect_true(inherits(g1, 'geopackage'))

# heterogeneous input from list
tfcsv <- tempfile(fileext = ".csv")
tfgpkg <- tempfile(fileext = ".gpkg")
v <- terra::as.polygons(terra::rast(dem), ext = TRUE)
expect_error(gpkg_write(list(testgpkg = tfgpkg), destfile = tfgpkg))
expect_silent(gpkg_write(list(testgpkg = v), destfile = tfgpkg))
expect_true(is.character(gpkg_list_tables(tfgpkg)))
write.csv(data.frame(id = 1:3, code = LETTERS[1:3]), tfcsv)
g2 <- geopackage(list(
  dem1 = dem,
  dem2 = terra::rast(dem),
  bbox1 = v,
  bbox2 = tfgpkg,
  data1 = data.frame(id = 1:3, code = LETTERS[1:3]),
  data2 = tfcsv
))
expect_true(inherits(g2, 'geopackage'))
expect_true(inherits(gpkg_table_pragma(g2), 'data.frame'))
expect_error(gpkg_table_pragma(g2, "dem3"))
expect_error(gpkg_table(g2, "dem3"))
expect_true(is.character(gpkg_table(g2, "dem2", query_string = TRUE)))
expect_true(is.character(
  gpkg_update_table(g2, "dem2", "zoom_level", 1, "id", 1, query_string = TRUE)
))
gpkg_disconnect(g2)
unlink(tfcsv)
unlink(tfgpkg)

# missing input
g3 <- geopackage(connect = TRUE)
expect_true(inherits(g3, 'geopackage'))

# manipulating an empty gpkg_contents table
expect_true(gpkg_create_contents(g3))

# add dummy row
expect_true(gpkg_add_contents(g3, "foo", "bar",
                              template = list(
                                ext = c(0, 0, 0, 0),
                                srsid = 4326
                              )))

# add dummy attribute table
expect_true(gpkg_write_attributes(g3, data.frame(id = 1), "A", "the letter A"))

# try various 'lazy' accessor methods
expect_warning({d1 <- gpkg_table_pragma(g3$dsn, "gpkg_contents")})
expect_true(inherits(d1, 'data.frame'))
expect_true(inherits(gpkg_table_pragma(g3$con, "gpkg_contents"), 'data.frame'))

expect_silent({d2 <- gpkg_table(g3$dsn, "gpkg_contents")})
expect_true(inherits(gpkg_table(g3$con, "gpkg_contents"), 'tbl_SQLiteConnection'))
expect_true(inherits(d2, 'tbl_SQLiteConnection'))

# verify insert/delete of dummy gpkg_contents rows
expect_equal(nrow(gpkg_query(g3, "select * from gpkg_contents;")), 2)
expect_true(gpkg_update_contents(g3))
expect_true(gpkg_delete_contents(g3, "foo"))
expect_equal(gpkg_execute(g3, "select * from gpkg_contents;"), 0)

# # disconnect sqliteconnection directly
expect_true(gpkg_disconnect(g3$con))

# add bounding polygon vector dataset
b <- terra::as.polygons(gpkg_tables(g)[["DEM1"]], ext = TRUE)
expect_silent(gpkg_write(b, destfile = gpkg_tmp, insert = TRUE))

d  <- data.frame(a = 1:10, b = LETTERS[1:10])
expect_silent(gpkg_write(list(myattr = d), destfile = gpkg_tmp, append = TRUE))

# enumerate tables
tl <- gpkg_list_tables(g)
expect_true(is.character(tl) && all(c("layer1", "myattr") %in% tl))

tlex <- gpkg_tables(g)
expect_equal(length(tlex), 4)

expect_true(inherits(gpkg_2d_gridded_coverage_ancillary(g), 'data.frame'))

expect_true(gpkg_remove_attributes(g, "myattr"))

# still connected
expect_true(gpkg_is_connected(g))

expect_stdout(gpkg_read(g))

# extensions
expect_equal(gpkg_add_metadata_extension(g), 0)
expect_equal(gpkg_add_relatedtables_extension(g), 0)

# TODO: validator
expect_error(gpkg_validate(g))

# checking ability to clean up corrupted contents
RSQLite::dbRemoveTable(g$con, "gpkg_contents")
RSQLite::dbWriteTable(g$con, "bar", data.frame(b = 2))
tf <- tempfile()
sink(file = tf)
expect_stdout(gpkg_update_contents(g))
sink()
unlink(tf)
expect_true("bar" %in% gpkg_contents(g)$table_name)

# disconnect it
expect_true(gpkg_disconnect(g))

# cleanup
unlink(gpkg_tmp)

