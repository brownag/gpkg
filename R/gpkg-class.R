# GeoPackage class

#' `geopackage` Constructors
#'
#' @param x list of SpatVectorProxy, SpatRaster, data.frame; or a character containing path to a GeoPackage file; or an SQLiteConnection to a GeoPackage. If missing, a temporary file with .gpkg extension is created in `tempdir`.
#' @param dsn Path to GeoPackage File (may not exist)
#' @param pattern used only when `x` is missing (creating temporary file GeoPackage), passed to `tempfile()`; default `"Rgpkg"`
#' @param tmpdir used only when `x` is missing (creating temporary file GeoPackage), passed to `tempfile()`; default `tempdir()`
#' @param connect Connect to database and store connection in result? Default: `FALSE`
#' @param ... Additional arguments \[not currently used\]
#'
#' @return A _geopackage_ object
#' @rdname geopackage-class
#' @export
geopackage <- function(x, ...)
  if (missing(x)) geopackage.missing(...) else UseMethod("geopackage", x)

#' @rdname geopackage-class
#' @export
geopackage.list <- function(x, dsn = NULL, connect = FALSE, ...) {
  if (is.null(dsn)) {
    dsn <- tempfile("Rgpkg", fileext = ".gpkg")
  }
  
  if (is.character(dsn) && !file.exists(dsn)) {
    gpkg_write(x, destfile = dsn, ...)
    dsn <- .gpkg_connection_from_x(dsn)
  } else {
    if (!all(names(x) %in% gpkg_list_tables(dsn))) {
      stop("File (", dsn, ") already exists! `geopackage(<list>)` should only be used when the GeoPackage `dsn` needs to be created. See the `geopackage(<character>)` and `geopackage(<SQLiteConnection>)` methods (without list input) to use existing databases.", call. = FALSE)
    }
  }
  obj <- .geopackage(dsn = dsn, connect = connect, ...)
  obj$tables <- x
  obj
}

#' @rdname geopackage-class
#' @export
geopackage.missing <- function(x, connect = FALSE, pattern = "Rgpkg", tmpdir = tempdir(), ...) {
  tf <- tempfile(pattern = pattern, tmpdir = tmpdir, fileext = ".gpkg")
  tft <- try(file.create(tf))
  if (inherits(tft, 'try-error')) stop('could not create temporary geopackage in ', tmpdir, call. = FALSE)
  obj <- .geopackage(dsn = tf, connect = connect, ...)
  obj$tables <- list()
  obj
}

#' @rdname geopackage-class
#' @export
geopackage.SQLiteConnection <- function(x, connect = FALSE, ...) {
  .geopackage(dsn = x, connect = connect, ...)
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
    if (requireNamespace("RSQLite", quietly = TRUE)) {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), dsn)
    } else stop('package `RSQLite` is required to connect to GeoPackages', call. = FALSE)
  }
  obj <- structure(list(
    tables = list(),
    env = list2env(list(con = con)),
    dsn = dsn
  ), class = "geopackage")
}

#' @export
#' @importFrom methods show
print.geopackage <- function(x, ...) {
  cat("<geopackage>", sep = "\n")
  xx <- gpkg_list_tables(x)
  y <- paste0(rep("-", getOption("width")), collapse = "")
  cat(y, sep = "\n")
  cat(paste0("# of Tables: ", length(xx)), "", sep = "\n\t")
  cat("\t")
  cat(strwrap(paste0(xx, collapse = ", ")), sep = "\n\t")
  cat(y, sep = "\n")
  if (!is.null(x$env$con)) {
    show(x$env$con)
  }
}
