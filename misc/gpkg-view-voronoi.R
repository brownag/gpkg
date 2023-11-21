library(gpkg)
library(terra)

gpkg_tmp <- tempfile(fileext = ".gpkg")

if (file.exists(gpkg_tmp))
  file.remove(gpkg_tmp)

v <- vect(system.file("ex", "lux.shp", package = "terra"))

gpkg_write(list(lux = v), destfile = gpkg_tmp, append = TRUE)

g <- geopackage(gpkg_tmp, connect = TRUE)

gpkg_create_spatial_view(g, "my_vor", "SELECT lux.fid AS OGC_FID, 
                                              lux.fid AS fid,
                                              ST_VoronojDiagram(geom) AS geom2 
                                       FROM lux WHERE ID_2 <= 3", 
                         geom_column = "geom2", 
                         geometry_type_name = "MULTIPOLYGON") 
                         # NB: terra::vect() sensitive to geom type
gpkg_contents(g)

plot(gpkg_vect(g, "my_vor"))        
gpkg_vect(g, "lux") |> 
  subset(ID_2 <= 3, NSE = TRUE) |> 
  as.lines() |> 
  plot(col = "BLUE", lwd = 2, add = TRUE)

# compare to:
plot(sf::st_read(g$dsn, layer = "my_vor"))              
plot(svc(g$dsn, layer = "my_vor")[1])                    
plot(query(vect(g$dsn, layer = "my_vor", proxy = TRUE))) 
