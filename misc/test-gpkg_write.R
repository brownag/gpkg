library(gpkg)

options(gpkg.debug = TRUE)

gpkg_write("~/Geodata/ISSR800/caco3_kg_sq_m.tif",
           "misc/caco3.gpkg", 
           overwrite = TRUE, 
           RASTER_TABLE = "caco3_kg_sq_m", 
           RASTER_IDENTIFIER = "caco3_kg_sq_m",
           RASTER_DESCRIPTION = "Calcium Carbonate Surface Density (kilograms per square meter)",
           FIELD_NAME = "caco3_kg_sq_m", 
           BLOCKSIZE = 2048, # N.B:  BLOCKSIZE must be larger than default 256
           UOM = "unit:KiloGM-PER-M2", 
           GRID_CELL_ENCODING = "grid-value-is-area")
# TODO: terra? NoData does not work quite right for tiles that are dropped 
# Would be more efficient w/ smaller tiles and NoData mapped to something other than 0; looking into it

gpkg_write("~/Geodata/ISSR800/ec.tif",
           "misc/caco3.gpkg",
           append = TRUE, 
           RASTER_TABLE = "ec", 
           RASTER_IDENTIFIER = "ec",
           RASTER_DESCRIPTION = "Electrical Conductivity (deciSiemens per meter)",
           FIELD_NAME = "ec", 
           BLOCKSIZE = 2048,
           UOM = "unit:DeciS-PER-M", 
           GRID_CELL_ENCODING = "grid-value-is-area")

# read (just wraps gdalinfo/terra::describe() for now)
x <- gpkg_read("misc/caco3.gpkg", quiet = TRUE)
x

# inspect
terra::plot(x[[1]] == 0) 
