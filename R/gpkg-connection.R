
#' Create SQLite Connection to GeoPackage
#'
#' @param x Path to GeoPackage
#'
#' @return A DBIConnection (SQLiteConnection) object. `NULL` on error.
#' @export
#' @rdname gpkg-connnection
gpkg_connect <- function(x)
  UseMethod("gpkg_connect", x)

#' @export
#' @rdname gpkg-connnection
gpkg_connect.geopackage <- function(x) {
  x$con <- gpkg_connect(x$dsn)$con
  x
}

#' @export
#' @importFrom DBI dbConnect
#' @rdname gpkg-connnection
gpkg_connect.character <- function(x) {
  
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    
    con <- try(DBI::dbConnect(RSQLite::SQLite(), x), silent = TRUE)
    
    if (inherits(con, 'try-error')) {
      message(con[1])
      return(NULL)
    }
    
  } else {
    stop('package `RSQLite` is required to open a connection to a GeoPackage', call. = FALSE)
  }
  geopackage(con)
}

#' @export
#' @rdname gpkg-connnection
gpkg_is_connected <- function(x)
  UseMethod("gpkg_is_connected", x)

#' @export
#' @rdname gpkg-connnection
gpkg_is_connected.geopackage <- function(x) {
  !is.null(x$dsn)
}

#' Create SQLite Connection to GeoPackage
#'
#' @param x A `geopackage` or `SQLiteConnection` object
#' @return Logical (invisible). `FALSE` if connection cannot be closed. 
#' @export
#' @importFrom DBI dbDisconnect
#' @rdname gpkg-connnection
gpkg_disconnect <- function(x)
  UseMethod("gpkg_disconnect", x)

#' @export
#' @rdname gpkg-connnection
gpkg_disconnect.geopackage <- function(x) {
  if (gpkg_is_connected(x)) {
    gpkg_disconnect(x$con)
  }
}

#' @export
#' @rdname gpkg-connnection
gpkg_disconnect.SQLiteConnection <- function(x) {
  if (requireNamespace("DBI", quietly = TRUE)) {
    if (inherits(x, 'SQLiteConnection')) {
      return(DBI::dbDisconnect(x))
    } else {
      warning("`x` is not an SQLiteConnection", call. = )
    }
  }
  invisible(FALSE)
}



#' .gpkg_connection_from_x
#' 
#' @param x A `geopackage` object, a path to a GeoPackage or an `SQLiteConnection`
#' @return An SQLiteConnection with logical attribute `"disconnect"` indicating whether it should be disconnected after use.
#' @noRd
#' @keywords internal
.gpkg_connection_from_x <- function(x) {
  disconnect <- TRUE
  if (is.character(x)) {
    con <- gpkg_connect(x)$con
  } else if (inherits(x, 'geopackage')) {
    if (!gpkg_is_connected(x)) {
      p <- x$dsn
      con <- gpkg_connect(p)$con
    } else {
      con <- x$con
      disconnect <- FALSE
    }
  } else if (inherits(x, 'SQLiteConnection')) {
    con <- x
    disconnect <- FALSE
  } else {
    stop('`x` should be a `geopackage` object, a path to a GeoPackage or an `SQLiteConnection`')
  }
  attr(con, 'disconnect') <- disconnect
  con
}