# lazy data.frame implementation for tables in a geopackage
#' @export
#' @rdname lazy.frame
lazy.frame <- function(x, table_name = NULL, ...)
  UseMethod("lazy.frame", x)

#' @rdname lazy.frame
lazy.frame.character <- function(x, table_name = NULL, ...) {
  g <- geopackage(x, connect = TRUE)
  res <- lazy.frame(g, table_name = table_name, ...)
  gpkg_disconnect(g)
  res
}

#' Lazy Access to Table Information
#' 
#' `lazy.frame()`: Get information on a table in a GeoPackage (without returning the whole table).
#' 
#' @param x A geopackage object or character path to GeoPcakge
#' @param table_name One or more table names; for `lazy.frame()` if `table_name=NULL` returns a record for each table. `dplyr.frame()` requires `table_name` be specified
#' @param ... Additional arguments. In `dplyr.frame()` arguments in `...` are passed to `dplyr::tbl()`. For `lazy.frame()`, `...` arguments are (currently) not used. 
#' @export
#' @rdname lazy.frame
#' @importFrom DBI dbGetQuery dbDisconnect
lazy.frame.geopackage <- function(x, table_name = NULL, ...) {
  con <- .gpkg_connection_from_x(x)
  tbls <- gpkg_list_tables(con)
  dsn <- gpkg_source(x)
  if (is.null(table_name)) {
    table_name <- tbls
  }
  if (!all(table_name %in% tbls))
    stop("no table with name: '",
         paste0(table_name[!table_name %in% tbls], collapse = "', '"),
         "' in ",
         dsn)
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
  attr(res, 'class') <- c("lazy.frame", "data.frame")
  res
}

#' @export
#' @rdname lazy.frame
#' @examplesIf !inherits(try(requireNamespace("dbplyr", quietly = TRUE), 'try-error'))
#' @description `dplyr.frame()`: access a specific table (by name) and get a "lazy" `tibble` object referencing that table
#' @examples 
#' # inspect gpkg_contents table
#' dplyr.frame(g, "gpkg_contents")
#' 
#' # materialize a data.frame in memory by querying gpkg_2d_gridded_tile_ancillary
#' library(dplyr, warn.conflicts = FALSE)
#' dplyr.frame(g, "gpkg_2d_gridded_tile_ancillary") %>% 
#'   filter(tpudt_name == "DEM2") %>% 
#'   select(mean, std_dev) %>% 
#'   collect()
dplyr.frame <- function(x, table_name, ...)
  UseMethod("dplyr.frame", x)

#' @rdname lazy.frame
#' @export
dplyr.frame.character <- function(x, table_name, ...) {
  dplyr.frame(geopackage(x), table_name, ...)
}

#' @rdname lazy.frame
#' @export
dplyr.frame.geopackage <- function(x, table_name, ...) {
  stopifnot(requireNamespace("dbplyr"))
  
  con <- .gpkg_connection_from_x(x)
  tbls <- gpkg_list_tables(con)
  # dsn <- gpkg_source(x)
  if (missing(table_name) || length(table_name) == 0) {
    stop("table name should be one of:",
         paste0(tbls, collapse = ", "),
         call = FALSE)
  }
  
  dplyr::tbl(con, table_name, ...)
}