#' Get `gpkg_contents` or `gpkg_ogr_contents` Table
#'
#' These functions provide convenient access to the contents of the standard GeoPackage tables of the same name.
#'
#' @param x A _geopackage_ object, path to a GeoPackage or an _SQLiteConnection_
#' @param create Create table `gpkg_contents` if does not exist? Default: ``
#' @return `gpkg_contents()`: a _data.frame_ containing columns `table_name`, `data_type`, `identifier`, `description`, `last_change`, `min_x`, `min_y`, `max_x`, `max_y`, `srs_id`
#' @importFrom DBI dbDisconnect
#' @export
gpkg_contents <- function(x, create = FALSE) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop('package `RSQLite` is required to get the `gpkg_contents` table', call. = FALSE)
  }
  if (!"gpkg_contents" %in% gpkg_list_tables(x) && isTRUE(create)) {
    res <- gpkg_create_contents(x)
    if (!res) {
      stop("Failed to create gpkg_contents table", call. = FALSE)
    }
  }
  gpkg_collect(x, "gpkg_contents")
}

#' @export
#' @return `gpkg_ogr_contents()`: a _data.frame_ containing columns `table_name` and `feature_count`.
#' @rdname gpkg_contents
gpkg_ogr_contents <- function(x) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop('package `RSQLite` is required to get the `gpkg_ogr_contents` table', call. = FALSE)
  }
  gpkg_collect(x, "gpkg_ogr_contents")
}


#' List Tables Registered in a GeoPackage `gpkg_contents`
#'
#' Get a vector of grid, feature and attribute table names registered in GeoPackage.
#'
#' @param x A _geopackage_ object, path to a GeoPackage or an _SQLiteConnection_
#' @param ogr Intersect `gpkg_contents` table name result with OGR tables that are listed in `gpkg_ogr_contents`? Default: `FALSE`
#' @export
#' @return character. Vector of grid, feature and attribute table names registered in GeoPackage.
#' @seealso [gpkg_contents()] [gpkg_list_tables()]
gpkg_list_contents <- function(x, ogr = FALSE) {
  y <- gpkg_contents(x)$table_name
  if (is.null(y))
    y <- character()
  if (isTRUE(ogr)) {
    z <- gpkg_ogr_contents(x)$table_name
  } else {
    z <- y
  }
  intersect(y, z)
}

#' Add, Remove, Update and Create `gpkg_contents` table and records
#' @description `gpkg_add_contents()`: Add a record to `gpkg_contents`
#'
#' @param x A _geopackage_
#' @param table_name Name of table to add or remove record for in _gpkg_contents_
#' @param data_type _character_. One of: `2d-gridded-coverage`, `"features"`, `"attributes"`. Default `NULL` will attempt to auto-detect table type based on `gpkg_table_pragma()` information; falls back to `"attributes"` if raster or vector data are not detected.
#' @param description Default: `""`
#' @param template Deprecated. A list containing elements `"srsid"` and `"ext"`.
#' @param srs_id _integer_. Spatial Reference System ID. Must be defined in `gpkg_spatial_ref_sys` table.
#' @param ext _numeric_. A numeric vector of length four specifying the bounding box extent.
#' @param query_string _logical_. Return SQLite statement rather than executing it? Default: `FALSE`
#'
#' @return logical. TRUE on successful execution of SQL statements.
#' @rdname gpkg-contents
#' @export
gpkg_add_contents <- function(x, table_name, data_type = NULL, description = "", srs_id = NULL, ext = NULL, template = NULL, query_string = FALSE) {
  
  con <- .gpkg_connection_from_x(x)
  dt <- data_type
  
  if (!missing(srs_id) && !is.null(srs_id)) {
    if (!length(srs_id) == 1 || !is.integer(as.integer(srs_id)))
      stop("`srs_id` should be an integer of length 1")
    cr <- srs_id
  }

  if (!missing(ext) && !is.null(ext)) {
    if (!length(ext) == 4 || !is.numeric(ext))
      stop("`ext` should be a numeric vector of length 4")
    ex <- ext
  }

  if (!missing(template) && !is.null(template)) {
    .Deprecated(msg = "`template` argument is deprecated, use `ext` and `srs_id` arguments directly")
    # template as a list
    if (is.list(template) && all(c("ext", "srsid") %in% names(template))) {
      ex <- template$ext
      cr <- as.integer(template$srsid)
      dt <- template$data_type
    } else {
      ## TODO: calculate ext from object, calculate srsid from WKT (?)
      # if (!requireNamespace("terra", quietly = TRUE)) {
      #   stop("package `terra` is required to add contents with a custom extent", call. = FALSE)
      # }
      #
      # # convert sf object to SpatVector
      # if (inherits(template, 'sf')) {
      #   template <- terra::vect(template)
      # }
      #
      # # template as terra object
      # if (inherits(template, c("SpatRaster", "SpatVector", "SpatVectorProxy"))){
      #   ex <- as.numeric(terra::ext(template))
      #   cr <- as.character(terra::crs(template))
      # }
    }
  } else {
    ex <- c(-180, -90, 180, 90)
    cr <- 4326
  }

  if (is.null(dt)) {
    gtp <- try(suppressWarnings(gpkg_table_pragma(con, table_name)), silent = TRUE)
    if (inherits(gtp, 'try-error')) {
      gtp <- NULL
    }
    if (all(
      c("id", "zoom_level", "tile_column", "tile_row", "tile_data")
      %in% gtp$table_info.name
    )) {
      # has tile information: 2D coverage
      dt <- "2d-gridded-coverage"
    } else if (any(c("POINT", "CURVE","LINESTRING", "SURFACE",
                     "CURVEPOLYGON", "POLYGON",
                     "GEOMETRY", "GEOMETRYCOLLECTION",
                     "MULTISURFACE", "MULTIPOLYGON", "MULTICURVE",
                     "MULTILINESTRING", "MULTIPOINT")
                   %in% toupper(gtp$table_info.type))) {
      # has a geometry column: vector geometry
      dt <- "features"
    } else {
      # all other cases are attributes
      dt <- "attributes"
    }
  }

  # create empty gpkg_contents table if needed
  if (!"gpkg_contents" %in% gpkg_list_tables(con)) {
    x <- gpkg_create_contents(con)
  }
  
  q <- paste0(
    "INSERT INTO gpkg_contents (table_name, data_type, identifier,
                                  description, last_change,
                                  min_x, min_y, max_x, max_y, srs_id)
       VALUES ('",
          table_name ,
          "', '", dt, "', '",
          table_name,
          "', '",
          description,
          "','",
          strftime(Sys.time(), '%Y-%m-%dT%H:%M:%OS3Z'),
          "', ", ex[1], ", ", ex[2], ", ",
          ex[3], ", ", ex[4], ", ",
          cr,"
                            );")

  if (query_string) {
    return(q)
  }

  # append to gpkg_contents
  res <- gpkg_execute(con, q)

  if (attr(con, 'disconnect')) {
    gpkg_disconnect(con)
  }
  
  !inherits(res, 'try-error')
}

#' @description `gpkg_update_contents()`: Add and remove `gpkg_contents` records to match existing tables
#' @rdname gpkg-contents
#' @export
gpkg_update_contents <- function(x) {
  contents <- try(gpkg_contents(x), silent = TRUE)
  if (inherits(contents, 'try-error') || !inherits(contents, 'data.frame')) {
    # create minimal gpkg_contents table
    if (gpkg_create_contents(x)) {
     contents <- try(gpkg_contents(x), silent = TRUE)
    }
    if (inherits(contents, 'try-error')) return(contents)
  }
  tables <- gpkg_list_tables(x)
  tables_nonstandard <- tables[!grepl("^gpkg_.*|rtree_.*|gpkgext_|sqlite_sequence", tables)]
  todo <- tables_nonstandard[!tables_nonstandard %in% contents$table_name]
  torem <- contents$table_name[!contents$table_name %in% tables]

  # create gpkg_contents records,
  # TODO: set extent via template?
  for (y in todo) {
    gpkg_add_contents(x, table_name = y, description = y)
  }

  # remove gpkg_contents records
  for (y in torem) {
    gpkg_delete_contents(x, table_name = y)
  }

  !inherits(x, 'try-error')
}

#' @description `gpkg_delete_contents()`: Delete a record from `gpkg_contents` based on table name
#' @rdname gpkg-contents
#' @export
gpkg_delete_contents <- function(x, table_name, query_string = FALSE) {
  q <- paste0("DELETE FROM gpkg_contents WHERE table_name = '", table_name, "'")

  if (query_string) {
    return(q)
  }
  res <- gpkg_execute(x, q)
  !inherits(res, 'try-error')
}

#' @description `gpkg_create_contents()`: Create an empty `gpkg_contents` table
#' @rdname gpkg-contents
#' @export
gpkg_create_contents <- function(x, query_string = FALSE) {
  q <- "CREATE TABLE gpkg_contents (
          table_name TEXT NOT NULL PRIMARY KEY,
          data_type TEXT NOT NULL,
          identifier TEXT UNIQUE,
          description TEXT DEFAULT '',
          last_change DATETIME NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
          min_x DOUBLE,
          min_y DOUBLE,
          max_x DOUBLE,
          max_y DOUBLE,
          srs_id INTEGER,
          CONSTRAINT fk_gc_r_srs_id FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys(srs_id)
        )"

  if (query_string) {
    return(q)
  }
  res <- gpkg_execute(x, q)
  !inherits(res, 'try-error')
}

# TODO: GDAL generally creates this as needed; consider add/update/delete/create
gpkg_create_ogr_contents <- function(x, dummy = FALSE) {
  res <- gpkg_execute(x,  "CREATE TABLE gpkg_ogr_contents (
          table_name TEXT NOT NULL PRIMARY KEY,
          feature_count INTEGER
        )")
  !inherits(res, 'try-error')
}

gpkg_add_ogr_contents <- function(x, table_name, feature_count) {
  res <- gpkg_execute(x, paste0(
    "INSERT INTO gpkg_ogr_contents (table_name, feature_count)
       VALUES ('", table_name, "', ", feature_count, ");"
  ))
  !inherits(res, 'try-error')
}

