
#' Create SQLite Connection to GeoPackage
#'
#' Method for creating and connecting `SQLiteConnection` object stored within `geopackage` object.
#' 
#' @details The S3 method for `geopackage` objects does not require the use of assignment to create an object containing an active SQLiteConnection. e.g. `gpkg_connect(g)` connects the existing `geopackage` object `g`
#' @param x Path to GeoPackage
#' @param disconnect Set attribute `'disconnect'` on SQLiteConnection object to auto-disconnect? Default: `FALSE`
#'
#' @return A DBIConnection (SQLiteConnection) object. `NULL` on error.
#' @export
#' @rdname gpkg-connection
#' 
#' 
gpkg_connect <- function(x)
  UseMethod("gpkg_connect", x)

#' @export
#' @rdname gpkg-connection
gpkg_connect.geopackage <- function(x) {
  econ <- x$env$con
  if (is.null(econ) || !DBI::dbIsValid(econ))
    x$env$con <- gpkg_connect(x$dsn)$env$con
  x
}

#' @export
#' @importFrom DBI dbConnect
#' @rdname gpkg-connection
gpkg_connect.character <- function(x) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) 
    stop('package `RSQLite` is required to open a connection to a GeoPackage', call. = FALSE)

  con <- try(DBI::dbConnect(RSQLite::SQLite(), x), silent = TRUE)
  if (!inherits(con, 'try-error')) {
    geopackage(con) 
  } else message(con[1])
}

#' @export
#' @rdname gpkg-connection
gpkg_is_connected <- function(x)
  UseMethod("gpkg_is_connected", x)

#' @export
#' @rdname gpkg-connection
gpkg_is_connected.geopackage <- function(x) {
  !is.null(x$env$con)
}

#' Create SQLite Connection to GeoPackage
#'
#' @param x A _geopackage_ or _SQLiteConnection_ object
#' @return If `x` is _geopackage_, the disconnected object is returned. If x is a _SQLiteConnection_, logical (`TRUE` if successfully disconnected).
#' @export
#' @importFrom DBI dbDisconnect
#' @rdname gpkg-connection
gpkg_disconnect <- function(x)
  UseMethod("gpkg_disconnect", x)

#' @export
#' @rdname gpkg-connection
gpkg_disconnect.geopackage <- function(x) {
  if (gpkg_is_connected(x)) {
    gpkg_disconnect(x$env$con)
    x$env$con <- NULL
  }
  invisible(x)
}

#' @export
#' @rdname gpkg-connection
gpkg_disconnect.SQLiteConnection <- function(x) {
 return(DBI::dbDisconnect(x))
}

#' @export
#' @rdname gpkg-connection
gpkg_disconnect.tbl_SQLiteConnection <- function(x) {
  return(DBI::dbDisconnect(x$src$con))
}

#' @export
#' @rdname gpkg-connection
gpkg_disconnect.src_SQLiteConnection <- function(x) {
  return(DBI::dbDisconnect(x$con))
}

#' @export
#' @rdname gpkg-connection
gpkg_connection <- function(x, disconnect = FALSE)
  UseMethod("gpkg_connection", x)

#' @export
#' @rdname gpkg-connection
gpkg_connection.default <- function(x, disconnect = FALSE) {
  .gpkg_connection_from_x(x, disconnect = disconnect)
}

#' .gpkg_connection_from_x
#'
#' @param x A _geopackage_ object, a path to a GeoPackage or an _SQLiteConnection_
#' @param disconnect logical. Set attribute to automatically close connection? Default: `TRUE`
#' @return An SQLiteConnection with logical attribute `"disconnect"` indicating whether it should be disconnected after use.
#' @noRd
#' @importFrom DBI dbIsValid
#' @keywords internal
.gpkg_connection_from_x <- function(x, disconnect = TRUE) {
  
  if (!is.logical(disconnect)) {
    stop("Argument `disconnect` is not logical.", call. = FALSE)
  }
  
  if (inherits(x, "tbl_SQLiteConnection")) {
    x <- x$src
  }
  
  if (inherits(x, "src_SQLiteConnection")) {
    x <- x$con
  }
  
  if (is.character(x)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), x)
  } else if (inherits(x, 'geopackage')) {
    if (!gpkg_is_connected(x)) {
      p <- x$dsn
      con <- gpkg_connect(p)$env$con
    } else {
      con <- x$env$con
      disconnect <- FALSE
    }
  } else if (inherits(x, 'SQLiteConnection')) {
    con <- x
    disconnect <- FALSE
  } else stop('`x` should be `geopackage` object, a path to a GeoPackage, or an _SQLiteConnection_')
  
  if (!DBI::dbIsValid(con)) {
    attr(con, 'disconnect') <- TRUE
  } else if (!is.null(con)) { 
    attr(con, 'disconnect') <- disconnect
  }
  con
}

.gpkg_proxy_from_x <- function(x, table_name = NULL) {
  
  if (inherits(x, 'SpatVectorProxy')) {
    return(x)
  }
  
  if (is.character(x)) {
    con <- x
  } else if (inherits(x, 'geopackage')) {
    con <- x$dsn
  } else if (inherits(x, 'SQLiteConnection')) {
    con <- x$env$con@dbname
  } else stop('`x` should be `geopackage` object, a path to a GeoPackage, a `SpatVectorProxy`, or an _SQLiteConnection_')
  
  if (is.null(table_name)) {
    table_name <- ""
  }
  
  suppressWarnings(terra::vect(con, layer = table_name, proxy = TRUE))
}
