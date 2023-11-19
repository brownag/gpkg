library(gpkg)
library(terra)

f <- "~/Downloads/All_SSURGO_10_06_2023.gpkg"
file.size(f)/(1024^3) # GB

all_ssurgo <- geopackage(f)
all_ssurgo

# survey area polygon SpatVector
v <- gpkg_vect(all_ssurgo, "sapolygon")
plot(v)

gpkg_table(all_ssurgo, "sdvcatalog")

# terra query very powerful / fast on large 100GB gpkg
gpkg_vect(all_ssurgo, "mupolygon", proxy = TRUE) |>
  query(where = "areasymbol = 'CA041'") -> v2

plot(v2)
plot(as.lines(subset(v, v$areasymbol == "CA041")),
     col = "BLUE",
     lwd = 2,
     add = TRUE)
