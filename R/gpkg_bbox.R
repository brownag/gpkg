#' Get Bounding Box of a GeoPackage Layer
#' 
#' This function is not currently exported. It relies on new features of the terra package and is intended primarily as a demonstration of an application of the OGR SQL query related features <https://gdal.org/user/sql_sqlite_dialect.html> and <https://gdal.org/user/ogr_sql_dialect.html>
#' 
#' @param x A _geopackage_ object
#' @param table_name character. One or more table names.
#'
#' @return a _data.frame_ containing columns `"xmin"`, `"ymin"`, `"xmax"`, `"ymax"`
#' @keywords internal
#' @noRd
#' @examplesIf !inherits(try(requireNamespace("terra", quietly=TRUE), silent=TRUE), 'try-error')
#' 
#' @examples
#' \dontrun{
#' tf <- tempfile(fileext = ".gpkg")
#' 
#' r <- terra::rast(system.file("extdata", "dem.tif", package = "gpkg"))
#' v <- as.polygons(r, ext = TRUE)
#' g <- geopackage(list(bbox = v))
#' 
#' gpkg_bbox(g, 'bbox')
#' }
gpkg_bbox <- function(x, table_name) {
  as.data.frame(gpkg_ogr_query(x, paste0("SELECT ST_MinX(geom) AS xmin, 
                                                 ST_MinY(geom) AS ymin, 
                                                 ST_MaxX(geom) AS xmax, 
                                                 ST_MaxY(geom) AS ymax 
                                          FROM ", table_name)))
}