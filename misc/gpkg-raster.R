library(soilDB)    
library(terra)
library(gpkg)

b <- c(-119.747629, -119.67935, 36.912019, 36.944987)

bbox.sp <- sf::st_as_sf(wk::rct(
  xmin = b[1], xmax = b[2], ymin = b[3], ymax = b[4],
  crs = sf::st_crs(4326)
))

ssurgo.geom <- SDA_spatialQuery(
  bbox.sp,
  what = 'mupolygon',
  db = 'SSURGO',
  geomIntersection = TRUE
)

sf::write_sf(ssurgo.geom, "test.shp")

x <- mukey.wcs(ssurgo.geom)

lback <- levels(x)

plot(x)
levels(x) <- NULL

r <- terra::writeRaster(x, "test.tif", datatype = "FLT4S", overwrite = TRUE)
plot(r)

r <- terra::classify(r,
                     matrix(c(463442, -9999), ncol = 2),
                     filename = "foo.tif",
                     overwrite = TRUE)

gpkg_write(
  list(bar = terra::rast("foo.tif")),
  NoData = -9999,
  destfile = "test.gpkg",
  overwrite = TRUE
)

gpkg_write(list(foo = terra::vect("test.shp", proxy = TRUE)),
           destfile = "test.gpkg",
           append = TRUE)

x2 <- terra::rast("test.gpkg")
ux2 <- unique(x2)
levels(x2) <- data.frame(value = ux2, category = ux2)
plot(x2)

x <- terra::vect("test.gpkg")
plot(terra::project(x,  x2), add = TRUE)
plot(is.na(x2))
