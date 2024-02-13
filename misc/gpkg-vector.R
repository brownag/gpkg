library(gpkg)
library(soilDB)

if (!dir.exists("misc/test-XI")) {
  downloadSSURGO(areasymbols = c("CA628", "CA630", "CA067"), destdir = "misc/test-XI")
}
createSSURGO("misc/test-XI.gpkg", exdir = "misc/test-XI")

x <- geopackage("misc/test-XI.gpkg")
gpkg_contents(x)

# need to specify dsn, even though query_string=TRUE, b/c default is T-SQL not SQLite
q <- soilDB::get_SDA_muaggatt("CA630", query_string = TRUE, dsn = x$dsn)
muaggatt <- gpkg_query(x, q)
gpkg_write(muaggatt, x$dsn, table_name = "muaggatt_sda", append=TRUE)
gpkg_update_contents(x)
gpkg_table(x, "muaggatt_sda")
gpkg_contents(x)
y <- gpkg_tables(x)
y[["muaggatt_sda"]]
y[["mupoint"]]
