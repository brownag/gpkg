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
g <- gpkg_disconnect(g)
expect_false(gpkg_is_connected(g))

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
rdem <- terra::rast(dem)
v <- terra::as.polygons(rdem, ext = TRUE)

expect_error(gpkg_write(list(testgpkg = tfgpkg), destfile = tfgpkg))
expect_silent(gpkg_write(list(testgpkg = v), destfile = tfgpkg))
v <- terra::crop(v, terra::ext(rdem) / 2)
expect_true(is.character(gpkg_list_tables(tfgpkg)))
write.csv(data.frame(id = 1:3, code = LETTERS[1:3]), tfcsv)
g2 <- geopackage(list(
  dem1 = dem,
  dem2 = terra::rast(dem),
  bbox1 = tfgpkg,
  bbox2 = v,
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
expect_true(inherits(gpkg_vect(g2, 'dem2'), 'SpatVector'))
expect_true(inherits(gpkg_vect(g2, 'gpkg_contents'), 'SpatVector'))
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
                                ext = c(0, 0, 0, 0),
                                srs_id = 4326
                              ))

# add dummy attribute table
expect_true(gpkg_write_attributes(g3, data.frame(id = 1), "A", "the letter A"))

# try various 'lazy' accessor methods
expect_warning({d1 <- gpkg_table_pragma(g3$dsn, "gpkg_contents")})
expect_true(inherits(d1, 'data.frame'))
expect_true(inherits(gpkg_table_pragma(g3, "gpkg_contents"), 'data.frame'))

if (requireNamespace("dbplyr", quietly = TRUE)) {
  expect_silent({d2 <- gpkg_table(g3$dsn, "gpkg_contents")})
  expect_true(inherits(gpkg_tbl(g3, "gpkg_contents"), 'tbl_SQLiteConnection'))
  expect_true(inherits(d2, 'tbl_SQLiteConnection'))
}

# verify insert/delete of dummy gpkg_contents rows
expect_equal(nrow(gpkg_query(g3, "select * from gpkg_contents;")), 2)
expect_true(gpkg_update_contents(g3))
expect_true(gpkg_delete_contents(g3, "foo"))
expect_equal(gpkg_execute(g3, "select * from gpkg_contents;"), 0)

# # disconnect sqliteconnection directly
expect_true(gpkg_disconnect(g3$env$con))

# add bounding polygon vector dataset
b <- terra::as.polygons(gpkg_rast(g, "DEM1"), ext = TRUE)
expect_silent(gpkg_write(list(layer1 = b, layerB = b), destfile = gpkg_tmp, insert = TRUE))

if (utils::packageVersion("terra") >= "1.7.33") {
  res <- gpkg_ogr_query(g, "SELECT 
                             ST_MinX(geom) AS xmin,
                             ST_MinY(geom) AS ymin, 
                             ST_MaxX(geom) AS xmax, 
                             ST_MaxY(geom) AS ymax 
                            FROM layerB")
  expect_equal(as.matrix(as.data.frame(res))[1,], terra::ext(b)[c(1,3,2,4)])
}

d  <- data.frame(a = 1:10, b = LETTERS[1:10])
expect_silent(gpkg_write(list(myattr = d), destfile = gpkg_tmp, append = TRUE))

# enumerate tables
tl <- gpkg_list_tables(g)
expect_true(is.character(tl) && all(c("layer1", "myattr") %in% tl))

if (requireNamespace("dbplyr", quietly = TRUE)) {
  tlex <- gpkg_tables(g)
  expect_equal(length(tlex), 5)
}

expect_true(inherits(gpkg_2d_gridded_coverage_ancillary(g), 'data.frame'))

expect_true(gpkg_remove_attributes(g, "myattr"))

# still connected
expect_true(gpkg_is_connected(g))

expect_stdout(gpkg_read(g))

# extensions
gempty <- geopackage(connect = TRUE)
expect_equal(gpkg_add_metadata_extension(gempty), 0)
expect_equal(gpkg_add_relatedtables_extension(gempty), 0)
gpkg_disconnect(gempty)
unlink(gempty$dsn)

expect_true(gpkg_validate(g))

# checking ability to clean up corrupted contents
RSQLite::dbRemoveTable(g$env$con, "gpkg_contents")
RSQLite::dbWriteTable(g$env$con, "bar", data.frame(b = 2))
tf <- tempfile()
sink(file = tf)
expect_stdout(gpkg_update_contents(g))
sink()
unlink(tf)
expect_true("bar" %in% gpkg_contents(g)$table_name)

expect_true(inherits(gpkg_vect(g, 'bar'), 'SpatVector'))

# cleanup
unlink(gpkg_tmp)

# geopackage<list> constructor with set outfile
r <- terra::rast(dem)
g <- geopackage(list(DEM1 = r, DEM2 = r, bar = data.frame(b = 2)), connect = FALSE, dsn = gpkg_tmp)
expect_equal(names(gpkg_rast(g)), c("DEM1", "DEM2"))
expect_true(inherits(gpkg_vect(g, 'bar'), 'SpatVector'))
expect_false(gpkg_is_connected(gpkg_disconnect(g)))
unlink(g$dsn)

# two grids + attributes into temp gpkg
r <- terra::rast(dem)
g <- geopackage(list(DEM1 = r, DEM2 = r, bar = data.frame(b = 2)))
expect_equal(names(gpkg_rast(g)), c("DEM1", "DEM2"))
expect_true(inherits(gpkg_vect(g, 'bar'), 'SpatVector'))
expect_false(gpkg_is_connected(gpkg_disconnect(g)))
unlink(g$dsn)

# attributes only (requires creation of "dummy" feature dataset) into temp gpkg
expect_warning(g <- geopackage(list(bar = data.frame(b = 2))))
gpkg_create_dummy_features(g)
expect_true(inherits(gpkg_vect(g, 'bar'), 'SpatVector'))

# disconnect it
expect_false(gpkg_is_connected(gpkg_disconnect(g)))
unlink(g$dsn)