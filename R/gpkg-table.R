# lazy data.frame implementation for tables in a geopackage
#' @export
#' @rdname gpkg_table
gpkg_table_pragma <- function(x, table_name = NULL, ...)
  UseMethod("gpkg_table_pragma", x)

#' @rdname gpkg_table
#' @export
gpkg_table_pragma.character <- function(x, table_name = NULL, ...) {
  g <- geopackage(x, connect = TRUE)
  res <- gpkg_table_pragma(g, table_name = table_name, ...)
  gpkg_disconnect(g)
  res
}

#' @rdname gpkg_table
#' @export
gpkg_table_pragma.SQLiteConnection <- function(x, table_name, ...) {
  gpkg_table_pragma(geopackage(x), table_name, ...)
}

#' Lazy Access to Tables by Name
#' 
#' `gpkg_table_pragma()`: Get information on a table in a GeoPackage (without returning the whole table).
#' 
#' @param x A geopackage object or character path to GeoPackage
#' @param table_name One or more table names; for `gpkg_table_pragma()` if `table_name=NULL` returns a record for each table. `gpkg_table()` requires `table_name` be specified
#' @param collect Materialize a data.frame object in memory? Default: `FALSE` requires 'dbplyr' package. `TRUE` uses 'RSQLite'.
#' @param query_string logical. Return SQLite query rather than executing it? Default: `FALSE`
#' @param ... Additional arguments. In `gpkg_table()` arguments in `...` are passed to `dplyr::tbl()`. For `gpkg_table_pragma()`, `...` arguments are (currently) not used. For `gpkg_vect()` additional arguments are passed to `terra::vect()`.
#' @export
#' @rdname gpkg_table
#' @importFrom DBI dbGetQuery dbDisconnect
gpkg_table_pragma.geopackage <- function(x, table_name = NULL, ...) {
  con <- .gpkg_connection_from_x(x)
  tbls <- gpkg_list_tables(con)
  dsn <- gpkg_source(x)
  if (is.null(table_name)) {
    table_name <- tbls
  }
  
  if (!all(table_name %in% tbls)) stop("no table with name: '", paste0(table_name[!table_name %in% tbls], collapse = "', '"), "' in ", dsn)
  
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
#' @examplesIf !inherits(try(requireNamespace("RSQLite", quietly = TRUE)), 'try-error') &&!inherits(try(requireNamespace("dbplyr", quietly = TRUE)), 'try-error') && !inherits(try(requireNamespace("terra", quietly = TRUE)), 'try-error')
#' @description `gpkg_table()`: access a specific table (by name) and get a "lazy" `tibble` object referencing that table
#' @examples 
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
#' g <- geopackage(tf)
#' 
#' # inspect gpkg_contents table
#' gpkg_table(g, "gpkg_contents")
#' 
#' # materialize a data.frame from gpkg_2d_gridded_tile_ancillary
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' gpkg_table(g, "gpkg_2d_gridded_tile_ancillary") %>% 
#'   dplyr::filter(tpudt_name == "DEM2") %>% 
#'   dplyr::select(mean, std_dev) %>% 
#'   dplyr::collect()
gpkg_table <- function(x,
                       table_name,
                       collect = FALSE,
                       query_string = FALSE,
                       ...)
  UseMethod("gpkg_table", x)

#' @rdname gpkg_table
#' @export
gpkg_table.default <- function(x,
                               table_name,
                               collect = FALSE,
                               query_string = FALSE,
                               ...) {
    
  con <- .gpkg_connection_from_x(x)
  
  if (isTRUE(collect) || isTRUE(query_string)) {
    
    if (attr(con, 'disconnect')) {
      on.exit(DBI::dbDisconnect(con))
    }
    
    q <- sprintf("SELECT * FROM %s", table_name)
    if (query_string) {
      return(q)
    }
    
    return(gpkg_query(con, q))
  }
  
  stopifnot(requireNamespace("dbplyr", quietly = TRUE))
  
  tbls <- gpkg_list_tables(con)
  
  if (missing(table_name) || length(table_name) == 0) stop("table name should be one of:", paste0(tbls, collapse = ", "), call = FALSE)
  
  dplyr::tbl(con, table_name, ...)
}

#' @description `gpkg_collect()`: alias for `gpkg_table(..., collect=TRUE)`
#' @rdname gpkg_table
#' @export
gpkg_collect <- function(x, table_name, query_string = FALSE, ...) {
  gpkg_table(x, table_name, ..., query_string = query_string, collect = TRUE)
}

#' @return `gpkg_vect()`: a SpatVector object (may or may not contain geometry columns)
#' @export
#' @rdname gpkg_table
gpkg_vect <- function(x, table_name, ...) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("package 'terra' is required to create SpatVector objects from tables in a GeoPackage", call. = FALSE)
  res <- try(terra::vect(x$dsn, layer = table_name, ...), silent = TRUE)
  if (inherits(res, 'try-error')) {
    res2 <- try(terra::vect(x$dsn, query = paste("SELECT * FROM", table_name), ...), '')
    if (inherits(res2, 'try-error')) {
      message(res[1])
      stop(res2[1], call. = FALSE)
    }
    res <- res2
  }
  res
}
