# lazy data.frame implementation for tables in a geopackage
#' @export
#' @rdname lazy.frame
lazy.frame <- function(x, ...)
  UseMethod("lazy.frame", x)

#' @rdname lazy.frame
lazy.frame.character <- function(x, ...) {
  g <- geopackage(x, connect = TRUE)
  res <- lazy.frame(g, ...)
  gpkg_disconnect(g)
  res
}

#' lazy data.frame
#' get information on a table in a GeoPackage (without returning the whole table)
#' @param x a geopackage object or character path to GeoPcakge
#' @param table_name one or more table names
#' @param ... additional arguments not used
#' @export
#' @rdname lazy.frame
#' @importFrom DBI dbGetQuery dbDisconnect
lazy.frame.geopackage <- function(x, table_name = NULL, ...) {
  con <- .gpkg_connection_from_x(x)
  tbls <- gpkg_list_tables(con)
  dsn <- gpkg_source(x)
  if (is.null(table_name)){
    table_name <- tbls
  }
  if (!all(table_name %in% tbls))
    stop("no table with name: '", paste0(table_name[!table_name %in% tbls], collapse="', '"), "' in ", dsn)
  res <- do.call('rbind', lapply(table_name, function(xx) {
    data.frame(
      dsn = dsn,
      table_name = xx,
      n_row = DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS nrow FROM", xx)[[1]]),
      table_info = I(list(DBI::dbGetQuery(con, paste0(
        "PRAGMA table_info(", xx, ")"
      )))) #TODO: custom print method for PRAGMA table_info?
    )
  }))
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  attr(res, 'class') <- c("lazy.frame", "data.frame")
  res
}
