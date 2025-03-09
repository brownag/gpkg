# lazy data.frame implementation for tables in a geopackage
#' @export
#' @rdname gpkg_table
gpkg_table_pragma <- function(x, table_name = NULL, ...)
  UseMethod("gpkg_table_pragma", x)

#' Lazy Access to Tables by Name
#' 
#' `gpkg_table_pragma()`: Get information on a table in a GeoPackage (without returning the whole table).
#' 
#' @param x A _geopackage_ object or character path to GeoPackage file
#' @param table_name _character_. One or more table names; for `gpkg_table_pragma()` if `table_name=NULL` returns a record for each table. `gpkg_table()` requires `table_name` be specified
#' @param collect _logical_. Materialize a data.frame object in memory? Default: `FALSE` requires 'dbplyr' package. `TRUE` uses 'RSQLite'.
#' @param column_names _character_. Used only when `collect=TRUE`. A _character_ vector of column names to select from `table_name`.
#' @param query_string _logical_. Return SQLite query rather than executing it? Default: `FALSE`
#' @param ... Additional arguments. In `gpkg_table()` arguments in `...` are passed to `dplyr::tbl()`. For `gpkg_table_pragma()`, `...` arguments are (currently) not used. For `gpkg_rast()` additional arguments are passed to `terra::rast()`. For `gpkg_vect()` additional arguments (such as `proxy=TRUE`) are passed to `terra::vect()`.
#' @export
#' @rdname gpkg_table
#' @importFrom DBI dbGetQuery dbDisconnect
gpkg_table_pragma.default <- function(x, table_name = NULL, ...) {
  con <- .gpkg_connection_from_x(x)
  tbls <- gpkg_list_tables(x)
  dsn <- con@dbname
  
  if (is.null(table_name)) {
    table_name <- tbls
  }
  
  if (!all(table_name %in% tbls)) 
    stop("no table with name: '", paste0(table_name[!table_name %in% tbls], collapse = "', '"), "' in ", dsn)
  
  res <- do.call('rbind', lapply(table_name, function(xx) {
    data.frame(dsn = dsn, 
      table_name = xx,
      n_row = DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS nrow FROM", xx)[[1]]),
      table_info = I(list(DBI::dbGetQuery(con, paste0(
        "PRAGMA table_info(", xx, ")"
      ))))[[1]] #TODO: custom print method for PRAGMA table_info?
    )
  }))
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  attr(res, 'class') <- c("gpkg_table_pragma", "data.frame")
  res
}

#' @export
#' @rdname gpkg_table
#' @description `gpkg_table()`: Access a specific table (by name) and get a _tbl_SQLiteConnection_ object referencing that table
#' @return `gpkg_table()`: A 'dbplyr' object of class _tbl_SQLiteConnection_
#' @examplesIf !inherits(try(requireNamespace("RSQLite", quietly = TRUE)), 'try-error') &&!inherits(try(requireNamespace("dbplyr", quietly = TRUE)), 'try-error') && !inherits(try(requireNamespace("terra", quietly = TRUE)), 'try-error')
#' 
#' tf <- tempfile(fileext = ".gpkg")
#' 
#' r <- terra::rast(system.file("extdata", "dem.tif", package = "gpkg"))
#'
#' gpkg_write(r,
#'            destfile = tf,
#'            RASTER_TABLE = "DEM1",
#'            FIELD_NAME = "Elevation")
#' 
#' gpkg_write(r,
#'            destfile = tf,
#'            append = TRUE,
#'            RASTER_TABLE = "DEM2",
#'            FIELD_NAME = "Elevation")
#'
#' g <- geopackage(tf, connect = TRUE)
#' 
#' # inspect gpkg_contents table
#' gpkg_table(g, "gpkg_contents")
#' 
#' gpkg_contents(g)
#' 
#' # materialize a data.frame from gpkg_2d_gridded_tile_ancillary
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' gpkg_table(g, "gpkg_2d_gridded_tile_ancillary") %>% 
#'   dplyr::filter(tpudt_name == "DEM2") %>% 
#'   dplyr::select(mean, std_dev) %>% 
#'   dplyr::collect()
#' 
#' gpkg_disconnect(g)
gpkg_table <- function(x,
                       table_name,
                       collect = FALSE,
                       column_names = "*",
                       query_string = FALSE,
                       ...)
  UseMethod("gpkg_table", x)

#' @rdname gpkg_table
#' @export
gpkg_table.default <- function(x,
                               table_name,
                               collect = FALSE,
                               column_names = "*",
                               query_string = FALSE,
                               ...) {
  
  if (is.null(column_names) ||
      length(column_names) == 0 ||
      nchar(as.character(column_names)) == 0) {
    column_names <- "*"
  }
  
  q <- sprintf("SELECT %s FROM %s",
               paste0(column_names, collapse = ", "),
               table_name)
  
  if (isTRUE(query_string)) {
    return(q)
  }
  
  con <- .gpkg_connection_from_x(x)
  
  if (isTRUE(collect)) { 
    res <- gpkg_query(con, q)
    if (attr(con, 'disconnect')) {
      gpkg_disconnect(con)
    }
    return(res)
  }
  
  stopifnot(requireNamespace("dbplyr", quietly = TRUE))
  
  res <- try(dplyr::tbl(con, table_name, ...), silent = FALSE)
  
  if (inherits(res, 'try-error')) {
    tbls <- gpkg_list_tables(con)

    if (length(tbls) == 0) {
      tbls <- "<none available>"
    }
    
    stop("table name should be one of: ",
         paste0(tbls, collapse = ", "), call. = FALSE)
  }
  
  # keep tbl_SQLiteconnection open
  if (attr(con, 'disconnect')) {
    attr(con, 'disconnect') <-  FALSE
  }
  
  res
}

#' @description `gpkg_collect()`: Alias for `gpkg_table(..., collect=TRUE)`
#' @return `gpkg_collect()`: An object of class _data.frame_
#' @rdname gpkg_table
#' @export
gpkg_collect <- function(x, table_name, query_string = FALSE, ...) {
  
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop('package `RSQLite` is required to get the `gpkg_extensions` table', call. = FALSE)
  }
  
  gpkg_table(x, table_name, ..., query_string = query_string, collect = TRUE)
}

#' @description `gpkg_tbl()`: Alias for `gpkg_table(..., collect=FALSE)`(default) that _always_ returns a _tbl_SQLiteConnection_ object.
#' @return `gpkg_tbl()`: An object of class _tbl_SQLiteConnection_
#' @rdname gpkg_table
#' @export
gpkg_tbl <- function(x, table_name, ...) {
  gpkg_table(x, table_name, ..., collect = FALSE)
}

#' @description `gpkg_rast()`: Get a _SpatRaster_ object corresponding to the specified `table_name`
#' @return `gpkg_rast()`: A 'terra' object of class _SpatRaster_
#' @export
#' @rdname gpkg_table
gpkg_rast <- function(x, table_name = NULL, ...) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("package 'terra' is required to create SpatVector objects from tables in a GeoPackage", call. = FALSE)
  if (is.null(table_name))
    table_name <- ""
  res <- try(terra::rast(x$dsn, subds = table_name, ...), silent = TRUE)
  if (inherits(res, 'try-error')) {
    stop(res[1], call. = FALSE)
  }
  res
}


#' @description `gpkg_vect()`: Get a _SpatVector_ object corresponding to the specified `table_name`
#' @return `gpkg_vect()`: A 'terra' object of class _SpatVector_ (may not contain geometry columns)
#' @export
#' @rdname gpkg_table
gpkg_vect <- function(x, table_name, ...) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("package 'terra' is required to create SpatVector objects from tables in a GeoPackage", call. = FALSE)
  res <- try(terra::vect(x$dsn, layer = table_name, ...), silent = TRUE)
  if (inherits(res, 'try-error')) {
    # create features, try again with layer not specified
    # gpkg_create_empty_features(x, table_name = "dummy_features", contents = FALSE)
    res2 <- try(terra::vect(x$dsn, query = paste("SELECT * FROM", table_name), ...), silent = TRUE)
    if (inherits(res2, 'try-error')) {
      stop(res2[1], call. = FALSE)
    }
    res <- res2
  }
  res
}

#' @description `gpkg_sf()`: Get a _sf-tibble_ object corresponding to the specified `table_name`
#' @return `gpkg_sf())`: An _sf-tibble_ object of class `"sf"`, `"tbl_df"`. If the table contains no geometry column the result is a `"tbl_df"`.
#' @export
#' @rdname gpkg_table
gpkg_sf <- function(x, table_name, ...) { 
  if (!requireNamespace("sf", quietly = TRUE))
    stop("package 'sf' is required to create 'sf' data.frame from tables in a GeoPackage", call. = FALSE)
  x <- .gpkg_connection_from_x(x)
  try(sf::read_sf(x$dsn, layer = table_name, ...), silent = TRUE)
}

gpkg_create_table <- function(x, table_name, fields, ...) {
  con <- .gpkg_connection_from_x(x)
  res <- try(RSQLite::dbCreateTable(con, name = table_name, fields = fields, ...))
  
  if (attr(con, 'disconnect')) {
    gpkg_disconnect(con) 
  }
  
  res
}

gpkg_append_table <- function(x, table_name, value, ...) {
  con <- .gpkg_connection_from_x(x)
  res <- try(RSQLite::dbAppendTable(con, name = table_name, value = value, ...))
  
  if (attr(con, 'disconnect')) {
    gpkg_disconnect(con) 
  }
  
  res
}