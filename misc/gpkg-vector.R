library(gpkg)
library(soilDB)

if (!dir.exists("misc/test-ca630")) {
  downloadSSURGO(areasymbols = "CA630", destdir = "misc/test-ca630")
}
createSSURGO("misc/test-ca630.gpkg", exdir = "misc/test-ca630")

x <- geopackage("misc/test-ca630.gpkg")
gpkg_contents(x)

# need to specify dsn, even though query_string=TRUE, b/c default is T-SQL not SQLite
q <- soilDB::get_SDA_muaggatt("CA630", query_string = TRUE, dsn = x$dsn)
muaggatt <- gpkg_query(x, q)
gpkg_write(muaggatt, x$dsn, table_name = "muaggatt_sda", append=TRUE)
gpkg_update_contents(x)
gpkg_table(x, "muaggatt_sda")
gpkg_contents(x)
