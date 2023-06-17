
#' Create SQLite Connection to GeoPackage
#'
#' Method for creating and connecting `SQLiteConnection` object stored within `geopackage` object.
#' 
#' @details The S3 method for `geopackage` objects uses in-place modification to update the parent object by name. That is, if you call `gpkg_connect()` on an object `g` then as a side-effect `g` is updated in the calling environment. This behavior is considered by many to be non-idiomatic for R, but it is useful to provide a simple way to connect an existing object without having to retain references to pointers to connection objects. To avoid replacement of object values in the parent frame, you can use the `character` method. That is, `g <- gpkg_connect(g$dsn)` is equivalent to `gpkg_connect(g)` when `g` is a `geopackage`.
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
  obj <- as.character(substitute(x))
  x$con <- gpkg_connect(x$dsn)$con
  # update object in parent frame
  try(assign(obj, x, envir = parent.frame()))
  x
}

#' @export
#' @importFrom DBI dbConnect
#' @rdname gpkg-connnection
gpkg_connect.character <- function(x) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) 
    stop('package `RSQLite` is required to open a connection to a GeoPackage', call. = FALSE)

  con <- try(DBI::dbConnect(RSQLite::SQLite(), x), silent = TRUE)
  if (!inherits(con, 'try-error')) {
    geopackage(con) 
  } else message(con[1])
}

#' @export
#' @rdname gpkg-connnection
gpkg_is_connected <- function(x)
  UseMethod("gpkg_is_connected", x)

#' @export
#' @rdname gpkg-connnection
gpkg_is_connected.geopackage <- function(x) {
  !is.null(x$con)
}

#' Create SQLite Connection to GeoPackage
#'
#' @param x A _geopackage_ or _SQLiteConnection_ object
#' @return If `x` is _geopackage_, the disconnected object is returned. If x is a _SQLiteConnection_, logical (`TRUE` if successfully disconnected).
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
    x$con <- NULL
  }
  invisible(x)
}

#' @export
#' @rdname gpkg-connnection
gpkg_disconnect.SQLiteConnection <- function(x) {
 return(DBI::dbDisconnect(x))
}
 
#' .gpkg_connection_from_x
#'
#' @param x A _geopackage_ object, a path to a GeoPackage or an _SQLiteConnection_
#' @return An SQLiteConnection with logical attribute `"disconnect"` indicating whether it should be disconnected after use.
#' @noRd
#' @importFrom DBI dbIsValid
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
  } else stop('`x` should be `geopackage` object, a path to a GeoPackage or an _SQLiteConnection_')
  
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
    con <- x$con@dbname
  } else stop('`x` should be `geopackage` object, a path to a GeoPackage, a `SpatVectorProxy`, or an _SQLiteConnection_')
  
  if (is.null(table_name)) {
    table_name <- ""
  }
  
  suppressWarnings(terra::vect(con, layer = table_name, proxy = TRUE))
}
