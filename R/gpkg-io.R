#' Read data from a GeoPackage
#'
#' Experimental: This function is being evaluated for its scope compared to other more general functions that perform similar operations (i.e. `gpkg_tables()`).
#'
#' @param x Path to GeoPackage
#' @param connect Connect to database and store connection in result? Default: `FALSE`
#' @param quiet Hide printing of gdalinfo description to stdout. Default: `TRUE`
#' @return A _geopackage_ object (list containing tables, grids and vector data)
#' @export
#' @keywords io
gpkg_read <- function(x, connect = FALSE, quiet = TRUE) {
  if (inherits(x, 'geopackage')) {
    x <- x$dsn
  }
  res <- lapply(x, function(xx) {
    res <- list()
    contents <- gpkg_contents(x, create = TRUE)
    # read grids
    if (!any(contents$data_type %in%  c("attributes", "features"))) {
        r <- try(terra::rast(xx), silent = TRUE)
        if (inherits(r, 'try-error')) {
          grids <- list()
        } else {
          # convert to list of single-layer SpatRaster
          grids <- as.list(r)
          # assign raster table names
          names(grids) <- names(r)
        }
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
    if (!quiet) cat(res$gdalinfo, sep = "\n")
    res
  })

  res2 <- do.call('c', lapply(res, function(x) x$tables))
  names(res2) <- do.call('c', lapply(res, function(x) names(x$tables)))
  g <- geopackage(res2, dsn = x)
  if (connect) {
    gpkg_connect(g)
  }
  g
}

#' Write data to a GeoPackage
#' @param x Vector of source file path(s), or a list containing one or more SpatRaster, SpatRasterCollection, or SpatVectorProxy objects.
#' @param destfile Character. Path to output GeoPackage
#' @param table_name Character. Default `NULL` name is derived from source file. Required if `x` is a _data.frame_.
#' @param datatype Data type. Defaults to `"FLT4S"` for GeoTIFF files, `"INT2U"` otherwise. See documentation for `terra::writeRaster()`.
#' @param append Append to existing data source? Default: `FALSE`. Setting `append=TRUE` overrides `overwrite=TRUE`
#' @param overwrite Overwrite existing data source? Default `FALSE`.
#' @param NoData Value to use as GDAL `NoData` Value
#' @param gdal_options Additional `gdal_options`, passed to `terra::writeRaster()`
#' @param ... Additional arguments are passed as GeoPackage "creation options." See Details.
#' @details Additional, non-default GeoPackage creation options can be specified as arguments to this function. The full list of creation options can be viewed [here](https://gdal.org/drivers/raster/gpkg.html#creation-options) or in the `gpkg_creation_options` dataset. The name of the argument is `creation_option` and the value is selected from one of the elements of `values` for that option.
#' @return Logical. `TRUE` on successful write of at least one grid.
#' @seealso [gpkg_creation_options]
#' @export
#' @keywords io
gpkg_write <- function(x,
                       destfile,
                       table_name = NULL,
                       datatype = "FLT4S",
                       append = FALSE,
                       overwrite = FALSE,
                       NoData = NULL,
                       gdal_options = NULL,
                       ...) {
  res <- .gpkg_process_sources(x, 
                               destfile,
                               table_name = table_name,
                               datatype = datatype,
                               append = append,
                               overwrite = overwrite,
                               NoData = NoData,
                               gdal_options = gdal_options,
                               ...)
  invisible(.gpkg_postprocess_sources(res))
}

.is.file <- function(y, pattern) {
  is.character(y) && grepl(sprintf("\\.(%s)$", pattern), y, ignore.case = TRUE)
}

.gpkg_process_sources <- function(x, ...) {
  if (!is.list(x) || is.data.frame(x)) {
    x <- list(x)
  }
  
  # TODO: extend this; only intended for prototyping before general sln
  
  # objects with a file-based
  src_raster <- vapply(x, inherits, logical(1), c('SpatRaster', 'SpatRasterCollection'))
  src_vector <- vapply(x, inherits, logical(1), 'SpatVectorProxy')
  obj_vector <- vapply(x, inherits, logical(1), c('sf', 'SpatVector'))
  obj_attrib <- vapply(x, inherits, logical(1), 'data.frame')
  pth_raster <- vapply(x, .is.file, logical(1),  "tif+|vrt|grd|png")
  pth_vector <- vapply(x, .is.file, logical(1),  "shp|gpkg")
  pth_attrib <- vapply(x, .is.file, logical(1),  "csv")
  
  # classify list of object input  grid, features, attributes
  #  - each processing function handles local objects and/or file paths
  list(
    grids = .gpkg_process_grids(c(x[src_raster], x[pth_raster]), ...),
    features = .gpkg_process_features(c(x[obj_vector], x[src_vector], x[pth_vector]), ...),
    attributes = .gpkg_process_attributes(c(x[obj_attrib], x[pth_attrib]), ...)
  )
}

#' @importFrom utils read.csv
.gpkg_process_attributes <- function(ldsn, destfile, table_name, overwrite=FALSE, append=TRUE, ...) {
  sapply(names(ldsn), function(x) {
    # attribute tables
    if (!inherits(ldsn[[x]], 'data.frame')) {
      ldsn[[x]] <- utils::read.csv(ldsn[[x]])
    }
    g <- geopackage(destfile, connect = TRUE)
    on.exit(gpkg_disconnect(g))
    x <- gpkg_write_attributes(
      g,
      ldsn[[x]],
      table_name = x,
      overwrite = overwrite,
      append = append
    )
    return(inherits(x, 'try-error'))
  })
}
    
.gpkg_process_grids <- function(ldsn, destfile, datatype, append = TRUE, overwrite = FALSE, NoData, gdal_options, ...) {
  res <- list()
  if (length(ldsn) > 0) {
    gdal_options_sub <- .gpkg_gdaloptions_add(gdal_options, 
                                              key = "RASTER_TABLE", 
                                              value = names(ldsn)[[1]])
    res <- list(.gpkg_write_grid_subdataset_terra(
      x = ldsn[[1]],
      destfile = destfile,
      datatype = datatype,
      append = append,
      overwrite = overwrite,
      NoData = NoData,
      gdal_options = gdal_options_sub,
      ...
    ))
    names(res) <- names(ldsn)[[1]]
  }
  sds <- list()
  if (length(ldsn) > 1) {
    # subsequent grids append=TRUE
    sds <- sapply(names(ldsn[2:length(ldsn)]), function(n) {
      gdal_options_sub <- .gpkg_gdaloptions_add(gdal_options, 
                                                key = "RASTER_TABLE", 
                                                value = n)
      .gpkg_write_grid_subdataset_terra(
        ldsn[[n]],
        destfile = destfile,
        datatype = datatype,
        append = TRUE,
        overwrite = overwrite,
        NoData = NoData,
        gdal_options = gdal_options_sub,
        ...
      )
    })
  }
  c(res, sds)
}

.gpkg_process_features <- function(ldsn, destfile, insert = TRUE, overwrite = FALSE, ...) {
  if (is.null(names(ldsn)) && length(ldsn) > 0) {
    names(ldsn) <- paste0("layer", 1:length(ldsn))
  }
  sapply(names(ldsn), function(vv) {
    .gpkg_write_vector_terra(
      ldsn[[vv]],
      layername = vv,
      destfile = destfile,
      insert = insert,
      overwrite = overwrite
    )
  })
}

.gpkg_postprocess_sources <- function(result, ...) {
  result
 # TODO gpkg_validate? 
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

.gpkg_debug <- function(x, ...) if (getOption("gpkg.debug", default = FALSE)) message("DEBUG:\n\n", x, ...)

.gpkg_gdaloptions_add <- function(gdal_options, key, value, force = FALSE) {
  # add a gdaloption (if 'key' not already defined)
  if (force || !any(grepl(key, gdal_options, ignore.case = TRUE))) 
    return(c(gdal_options, ifelse(nchar(value) > 0, paste0(key, "=", value), "")))
  else ""
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
  if (!requireNamespace('terra', quietly = TRUE)) stop('the `terra` package is required to write gridded data to GeoPackage', call. = FALSE)

  if (!inherits(x, 'SpatRaster')) {
    r <- terra::rast(x)
  } else {
    r <- x
  }

  gdal_options <- unique(c(gdal_options, .lut_gpkg_creation(...)))

  .gpkg_debug(paste0(gdal_options, collapse = "\n"))

  if (append) {
    if (!any(grepl("APPEND_SUBDATASET", gdal_options, ignore.case = TRUE))) {
      gdal_options <- c(gdal_options, "APPEND_SUBDATASET=YES")
    }
  }
  
  res <- terra::writeRaster(
    x = r,
    filename = destfile,
    datatype = datatype,
    overwrite = overwrite,
    gdal = gdal_options
  )

  if (!is.null(NoData)) {
    ctn <- gsub("RASTER_TABLE=(.*)|.*", "\\1", gdal_options)
    ctn <- ctn[nchar(ctn) > 0]
    if (length(ctn) > 0) {
      gpkg_tile_set_data_null(destfile, ctn, NoData)
    } else message("NOTE: NoData specified without setting RASTER_TABLE")
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
                                     gdal_options = NULL,
                                     ...) {

  gdal_options <- unique(c(gdal_options, .lut_gpkg_creation(...)))
  
  if (!inherits(x, 'SpatVector')) {
    x <- terra::vect(x)
  }
  
  res <- terra::writeVector(x,
            destfile,
            layer = layername,
            insert = insert,
            overwrite = overwrite,
            options = gdal_options
          )
  invisible(res)
}
