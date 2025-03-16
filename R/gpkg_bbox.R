#' Get Bounding Box of a GeoPackage Layer
#'
#' This function applies an OGR SQL query to obtain bounding coordinates of a
#' table containing a geometry column.
#' <https://gdal.org/en/stable/user/sql_sqlite_dialect.html> and
#' <https://gdal.org/en/stable/user/ogr_sql_dialect.html>
#'
#' @param x A _geopackage_ object
#' @param table_name character. One or more table names.
#' @param geom_column character. Geometry column name, default `"geom"`
#'
#' @return a _data.frame_ containing columns `"xmin"`, `"ymin"`, `"xmax"`,
#'   `"ymax"`
#' @keywords internal
#' @export
#' @examplesIf requireNamespace("terra", quietly=TRUE)
#' \donttest{
#' tf <- tempfile(fileext = ".gpkg")
#'
#' r <- terra::rast(system.file("extdata", "dem.tif", package = "gpkg"))
#' v <- terra::as.polygons(r, ext = TRUE)
#' g <- geopackage(list(bbox = v))
#'
#' gpkg_bbox(g, 'bbox')
#' }
gpkg_bbox <- function(x, table_name, geom_column = "geom") {
  as.data.frame(gpkg_ogr_query(x, paste(gsub("geom", geom_column,
                                              "SELECT 
                                                 ST_MinX(geom) AS xmin,
                                                 ST_MinY(geom) AS ymin,
                                                 ST_MaxX(geom) AS xmax,
                                                 ST_MaxY(geom) AS ymax
                                               FROM"), table_name)))
}