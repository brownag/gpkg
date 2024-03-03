#' GeoPackage Spatial Reference System
#'
#' @param x A geopackage object
#' @param column_name Default: `"srs_id"`
#'
#' @return `gpkg_spatial_ref_sys()`: _data.frame_
#' @export
#' @rdname gpkg-srs
gpkg_spatial_ref_sys <- function(x) {
  gpkg_collect(x, "gpkg_spatial_ref_sys")
} 

#' @return `gpkg_list_srs()`: _vector_ of values from specified `column_name`
#' @export
#' @rdname gpkg-srs
gpkg_list_srs <- function(x, column_name = "srs_id") {
  x <- .gpkg_connection_from_x(x)
  res <- gpkg_spatial_ref_sys(x)
  if (length(column_name) == 0 || 
      !column_name %in% colnames(res)) {
    stop("`column_name` should be one of: ", 
         paste0(shQuote(colnames(res)), collapse = ", "),
         call. = FALSE)
  }
  res[[column_name]]
}

#' @param default _logical_ or _character_. If `TRUE`, or one or more of `"cartesian"`, `"geographic"`, or `"crs84"`, then these default Spatial Reference Systems are added.
#' @return `gpkg_create_spatial_ref_sys()`: _integer_. Result of running sequential `gpkg_execute()` statements. This method is run for the side-effect of creating the table if needed, and adding any "default" records.
#' @export
#' @rdname gpkg-srs
gpkg_create_spatial_ref_sys <- function(x, default = TRUE, query_string = FALSE) { 
  x <- .gpkg_connection_from_x(x) 
  qout <- character()
  q <- character()
  if (!"gpkg_spatial_ref_sys" %in% gpkg_list_tables(x)) {
    qout <- "CREATE TABLE gpkg_spatial_ref_sys (
      srs_name TEXT NOT NULL,
      srs_id INTEGER PRIMARY KEY,
      organization TEXT NOT NULL,
      organization_coordsys_id INTEGER NOT NULL,
      definition  TEXT NOT NULL,
      description TEXT
    );"
    gsrs <- data.frame(srs_id = integer(0L))
    if (!query_string) {
      res <- gpkg_execute(x, qout)
    }
  } else {
    gsrs <- gpkg_spatial_ref_sys(x)
  }
  res <- FALSE
  if (!inherits(res, 'try-error') && 
    (isTRUE(default) || is.character(default))) {
    if (is.logical(default) || length(default) == 0) {
      default <- c("cartesian", "geographic", "EPSG:4326")
    } 
    for (d in default) {
      srs <- switch(tolower(d),
                    `cartesian` = .gpkg_srs_cartesian(),
                    `geographic` = .gpkg_srs_geographic(),
                    `epsg:4326` = .gpkg_srs_wgs84_geodetic())
      if (!srs[["srs_id"]] %in% gsrs[["srs_id"]]) {
        q <- c(
          q, 
          gpkg_add_spatial_ref_sys(
            x,
            srs_name = srs[["srs_name"]],
            srs_id = srs[["srs_id"]],
            organization = srs[["organization"]],
            organization_coordsys_id = srs[["organization_coordsys_id"]],
            definition = srs[["definition"]],
            description = srs[["description"]],
            query_string = TRUE
          )
        )
      }
    }
  }
  if (query_string) {
    return(c(qout, q))
  }
  unlist(lapply(q, function(qq) gpkg_execute(x, qq)))
}

.gpkg_srs_cartesian <- function() {
  list(
    srs_name = "Undefined Cartesian SRS",
    srs_id = -1L,
    organization = "NONE",
    organization_coordsys_id = -1L,
    definition = "undefined",
    description = "undefined Cartesian coordinate reference system"
  )
}

.gpkg_srs_geographic <- function() {
  list(
    srs_name = "Undefined geographic SRS",
    srs_id = 0L,
    organization = "NONE",
    organization_coordsys_id = 0L,
    definition = "undefined",
    description = "undefined geographic coordinate reference system"
  )
}

.gpkg_srs_wgs84_geodetic <- function() {
  list(
    srs_name = "WGS 84 geodetic",
    srs_id = 4326L,
    organization = "EPSG",
    organization_coordsys_id = 4326L,
    definition = "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AXIS[\"Latitude\",NORTH],AXIS[\"Longitude\",EAST],AUTHORITY[\"EPSG\",\"4326\"]]",
    description = "longitude/latitude coordinates in decimal degrees on the WGS 84 spheroid"
  )
}

#' @param srs_name _character_. Spatial Reference System Name, for example `"WGS 84 geodetic"`
#' @param srs_id _integer_. Spatial Reference System ID, for example `4326L`
#' @param organization _character_. Organization, for example `"EPSG"`
#' @param organization_coordsys_id _integer_. Organization Coordinate System ID, for example `4326L`
#' @param definition _character_. WKT2019 Coordinate Reference System description string
#' @param description _character_. Description
#' @param query_string _logical_. Return SQL queries without executing? Default: `FALSE`
#'
#' @return `gpkg_add_spatial_ref_sys()`: _integer_ result of executing SQL statement
#' @export
#' @rdname gpkg-srs
gpkg_add_spatial_ref_sys <- function(x, 
                                     srs_name = "", 
                                     srs_id = NULL, 
                                     organization = "",
                                     organization_coordsys_id = 0L,
                                     definition = "",
                                     description = "",
                                     query_string = FALSE) {
  x <- .gpkg_connection_from_x(x)
  if (!"gpkg_spatial_ref_sys" %in% gpkg_list_tables(x)) {
    gpkg_create_spatial_ref_sys(x, )
  }
  
  q <- sprintf("INSERT INTO gpkg_spatial_ref_sys 
        VALUES ('%s', %s, '%s', %s, '%s', '%s');",
        srs_name, srs_id, organization, organization_coordsys_id,
        definition, description)
  
  if (query_string) {
    return(q)
  }
  gpkg_execute(x, q)
}

#' @return `gpkg_delete_spatial_ref_sys()`: _integer_ result of executing SQL statement
#' @export
#' @rdname gpkg-srs
gpkg_delete_spatial_ref_sys <- function(x, srs_id = NULL) {
  x <- .gpkg_connection_from_x(x)
  if (!is.null(srs_id)) {
    sapply(srs_id, function(s) {
      gpkg_execute(x, paste0("DELETE FROM gpkg_spatial_ref_sys WHERE srs_id = ",
                             s))
    })
  } else NULL
}
