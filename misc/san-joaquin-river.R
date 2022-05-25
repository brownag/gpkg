library(terra)
library(nhdplusTools)
library(soilDB)

library(gpkg)

start_point <- sf::st_as_sf(vect(data.frame(x = -120.82618779786624, y = 37.28014612572742),
                                 geom = c("x", "y"), crs = "EPSG:4269"))
start_comid <- discover_nhdplus_id(start_point)
flowline <- navigate_nldi(list(featureSource = "comid",
                               featureID = start_comid),
                          mode = "upstreamTributaries",
                          distance_km = 1000)
if (!dir.exists("~/SJR"))
  dir.create("~/SJR")
subset_file <- "~/SJR/nhdtest.gpkg"
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

# could do the whole basin, but here just use the flowline segments that == "San Joaquin River"
x <- st_geometry(subset(subset$NHDFlowline_Network, gnis_name == "San Joaquin River"))

# inspect
plot(x, lwd = 4, col = "RED")
plot(st_geometry(fetchSDA_spatial(
  SDA_spatialQuery(x, 'areasymbol')$areasymbol,
  by.col = "areasymbol",
  geom.src = "sapolygon"
)), add = TRUE)

# create GPKG
downloadSSURGO(x, destdir = "~/SJR")
createSSURGO("~/SJR/SJR.gpkg", exdir = tempdir())

# inspect
gpkg_contents("~/SJR/SJR.gpkg")
gpkg_list_tables("~/SJR/SJR.gpkg")
g <- gpkg_read("~/SJR/SJR.gpkg")

g$tables[["soilmu_a"]] |>
  terra::query() |>
  terra::plot()
