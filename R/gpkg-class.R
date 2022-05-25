# GeoPackage class

#' `geopackage` Constructors
#'
#' @param x list of SpatVectorProxy, SpatRaster, data.frame; or a character containing path to a GeoPackage file; or an SQLiteConnection to a GeoPackage
#' @param dsn Path to GeoPackage File (may not exist)
#' @param connect Connect to database and store connection in result? Default: `FALSE`
#' @param ... Additional arguments \[not currently used\]
#'
#' @return A `geopackage` Object
#' @rdname geopackage-class
#' @export
geopackage <- function(x, ...)
  UseMethod("geopackage", x)

#' @rdname geopackage-class
#' @export
geopackage.list <- function(x, dsn = NULL, connect = FALSE, ...) {
  obj <- .geopackage(dsn = dsn, connect = FALSE, ...)
  obj$tables <- x
  obj
}

#' @rdname geopackage-class
#' @export
geopackage.SQLiteConnection <- function(x, connect = FALSE, ...) {
  obj <- .geopackage(dsn = x, connect = FALSE, ...)
  obj$tables <- x
  obj
}

#' @rdname geopackage-class
#' @export
geopackage.geopackage <- function(x, ...) {
  message("`x` is already a `geopackage`")
  x
}

#' @rdname geopackage-class
#' @export
geopackage.character <- function(x, connect = FALSE, ...) {
  gpkg_read(x, connect = connect, ...)
}

# basic geopackage structure
.geopackage <- function(dsn = NULL, connect = FALSE, ...) {
  con <- NULL
  # existing sqliteconnection
  if (inherits(dsn, 'SQLiteConnection')) {
    con <- dsn
    dsn <- con@dbname
  # create a connection when geopackage object is constructed
  } else if (connect) {
    # TODO: connect to dsn, store in con
  }
  obj <- structure(list(
    tables = list(),
    con = con,
    dsn = dsn
  ), class = "geopackage")
}

#' @export
#' @importFrom methods show
print.geopackage <- function(x, ...) {
  cat("<geopackage>", sep = "\n")
  cat(paste0("# of Tables: ", length(x$tables)), sep = "\n")
  cat("\t", paste0(names(x$tables), collapse = ", "), sep = "\n")
  if (!is.null(x$con)) {
    show(x$con)
  }
}
