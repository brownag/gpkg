#' Read a GeoPackage
#' @param x Path to GeoPackage
#' @param connect Connect to database and store connection in result? Default: `FALSE`
#' @param quiet Hide printing of gdalinfo description to stdout. Default: `TRUE`
#' @return A `geopackage` object (list containing tables, grids and vector data)
#' @export
#' @keywords io
gpkg_read <- function(x, connect = FALSE, quiet = TRUE) {
  res <- lapply(x, function(xx) {
    res <- list()
    contents <- gpkg_contents(x)
    # read grids
    if (any(contents$data_type != "features")) {
        r <- terra::rast(xx)
        # convert to list of single-layer SpatRaster
        grids <- as.list(r)
        # assign raster table names
        names(grids) <- names(r)
    } else grids <- list()

    # read vector layers (error if there aren't any)
    if (any(contents$data_type == "features")) {
        vects <- lapply(contents$table_name, function(xxx){
            # create SpatVectorProxy
            try(terra::vect(path.expand(xx), xxx, proxy = TRUE), silent = quiet)
          })
        names(vects) <- contents$table_name
        vects <- vects[!vapply(vects, FUN.VALUE = logical(1), inherits, 'try-error')]
    } else vects <- list()

    # TODO: get table references
    tables <- list()

    # spatial results (grid+vect+tabular) in `tables`
    res$tables <- c(grids, vects, tables)

    # descriptions in `gdalinfo`
    res$gdalinfo <- terra::describe(xx)

    # verbose gdalinfo output
    if (!quiet) {
      cat(res$gdalinfo, sep = "\n")
    }
    res
  })

  res2 <- do.call('c', lapply(res, function(x) x$tables))
  names(res2) <- do.call('c', lapply(res, function(x) names(x$tables)))
  g <- geopackage(res2, dsn = x)
  if (connect) {
    g <- gpkg_connect(g)
  }
  g
}

#' Write a GeoPackage
#' @param x Vector of source file path(s), or a list containing one or more SpatRaster, SpatRasterCollection, or SpatVectorProxy objects.
#' @param destfile Character. Path to output GeoPackage
#' @param datatype Data type. Defaults to `"FLT4S"` for GeoTIFF files, `"INT2U"` otherwise. See documentation for `terra::writeRaster()`.
#' @param append Append to existing data source? Default: `FALSE`. Setting `append=TRUE` overrides `overwrite=TRUE`
#' @param overwrite Overwrite existing data source? Default `FALSE`.
#' @param NoData Value to use as GDAL `NoData` Value
#' @param gdal_options Additional `gdal_options`, passed to `terra::writeRaster()`
#' @param ... Additional arguments are passed as GeoPackage "creation options." See Details.
#' @details Additional, non-default GeoPackage creation options can be specified as arguments to this function. The full list of creation options can be viewed [here](https://gdal.org/drivers/raster/gpkg.html#creation-options) or in the `gpkg_creation_options` dataset name of the argument is `creation_option` and the value is selected from one of the elements of `values` for that option.
#' @return Logical. `TRUE` on successful write of at least one grid.
#' @seealso [gpkg_creation_options]
#' @export
#' @keywords io
gpkg_write <- function(x,
                       destfile,
                       datatype = "FLT4S",
                       append = FALSE,
                       overwrite = FALSE,
                       NoData = NULL,
                       gdal_options = NULL,
                       ...) {
  if (!is.list(x) || is.character(x)) {
    x <- list(x)
  }
  if (is.list(x)) {
    x <- do.call('c', lapply(x, function(xx) {
      if (inherits(xx, 'SpatRasterCollection')) {
        return(sapply(xx, terra::sources))
      } else if (inherits(xx, 'SpatRaster')) {
        return(terra::sources(xx))
      } else if (inherits(xx, 'SpatVectorProxy')) {
        # TODO: is there an alternate way to do this?
        return(xx@ptr$v$source)
      } else {
        if (is.character(xx)) {
          return(xx)
        } else {
          return(character())
        }
      }
    }))
  }

  # classify source files -> (vector, grid, table)
  # TODO: extend types of sources
  ext <- gsub(".*\\.(.*$)", "\\1", x)
  ldsn <- split(x, factor(ext, levels = c('shp', 'tif', 'vrt', 'csv')), drop = FALSE)

  # if any grids are present, write them first
  grids <- c(ldsn[['tif']], ldsn[['vrt']])
  ngrd <- 0

  if (length(grids) > 0) {
    # first grid write with append=FALSE
    .gpkg_write_grid_subdataset_terra(x = grids[1],
                                      destfile = destfile,
                                      datatype = datatype,
                                      append = append,
                                      overwrite = overwrite,
                                      NoData = NoData,
                                      gdal_options = gdal_options,
                                      ...)
    ngrd <- 1
  }

  if (length(grids) > 1) {
    # subsequent grids append=TRUE
    sds <- lapply(grids[2:length(grids)], .gpkg_write_grid_subdataset_terra,
                                                  destfile = destfile,
                                                  datatype = datatype,
                                                  append = append,
                                                  overwrite = overwrite,
                                                  NoData = NoData,
                                                  gdal_options = gdal_options,
                                                  ...)
    ngrd <- ngrd + length(sds)
  }

  # iterate over vector/table sources and write to database
  vects <- ldsn[['shp']]
  if (is.null(names(vects)) && length(vects) > 0) {
    names(vects) <- as.character(1:length(vects))
  }
  nvct <- 0
  lapply(names(vects), function(vv) {
    # TODO: vector dataset open options -oo name=value <https://gdal.org/drivers/vector/gpkg.html#dataset-open-options>
    #                    creation options <https://gdal.org/drivers/vector/gpkg.html#dataset-creation-options>
    #                                     <https://gdal.org/drivers/vector/gpkg.html#layer-creation-options>
    .gpkg_write_vector_terra(vects[[vv]],
                             layername = vv,
                             destfile = destfile,
                             insert = TRUE,
                             overwrite = FALSE)
  })

  # post processing? validate?

  # return TRUE if at least one layer written
  invisible(ngrd > 0)
}

#' .gpkg_write_grid_subdataset_terra
#' @return A SpatRaster reference to grid written to GeoPackage, or `NULL` on error
#' @noRd
#' @keywords internal
.gpkg_write_grid_subdataset_terra <- function(x,
                                              destfile,
                                              datatype,
                                              append = FALSE,
                                              overwrite = FALSE,
                                              NoData = NULL,
                                              gdal_options = NULL,
                                              ...) {
  res <- NULL
  if (!requireNamespace('terra', quietly = TRUE)) {
    stop('the `terra` package is required to write gridded data to GeoPackage', call. = FALSE)
  }

  r <- terra::rast(x)

  if (!is.null(NoData)) {
    terra::NAflag(r) <- NoData
  }

  .lut_gpkg_creation <- function(...) {
    kv <- list(...)
    gpkg_creation_options <- get("gpkg_creation_options")
    kvn <- trimws(names(kv)[names(kv) %in% gpkg_creation_options$creation_option])
    kvn <- kvn[nchar(kvn) > 0]
    if (length(kvn) > 0) {
      return(paste0(kvn, "=", as.character(kv[kvn])))
    }
    character(0)
  }

  gdal_options <- c(gdal_options, "of=GPKG", .lut_gpkg_creation(...))

  if (getOption("gpkg.debug", default = FALSE)) {
    message("DEBUG:\n\n", paste0(gdal_options, collapse = "\n"))
  }

  if (append) {
    overwrite <- FALSE
    gdal_options <- c(gdal_options, "APPEND_SUBDATASET=YES")
  }
  res <- terra::writeRaster(
    x = r,
    filename = destfile,
    datatype = datatype,
    overwrite = overwrite,
    gdal = gdal_options
  )

  if (!is.null(NoData)) {
    # TODO: handle custom table name
    gpkg_tile_set_data_null(destfile, gsub("(.*?)\\..*", "\\1", basename(x)), NoData)
  }

  invisible(res)

}

#' .gpkg_write_vector_terra
#' @return A SpatVectorProxy reference to grid written to GeoPackage, or `NULL` on error
#' @noRd
#' @keywords internal
.gpkg_write_vector_terra <- function(x,
                                     destfile,
                                     layername,
                                     insert = FALSE,
                                     overwrite = FALSE,
                                     gdal_options = NULL) {

  # TODO: handle GDAL options
  if (insert) {
    overwrite <- FALSE
  }

  res <- terra::writeVector(terra::vect(x), destfile, layer = layername, insert = insert)
  invisible(res)
}

#' .gpkg_write_vector_terra
#' @return A SpatVectorProxy reference to grid written to GeoPackage, or `NULL` on error
#' @noRd
#' @keywords internal
.gpkg_write_table_RSQLite <- function(x,
                                      destfile,
                                      tablename) {

}
