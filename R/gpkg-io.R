#' Read data from a GeoPackage
#'
#' This function creates a _geopackage_ object with references to all tables
#' from the GeoPackage source specified in `x`. For a simple list of tables see
#' `gpkg_tables()`.
#'
#' @param x Path to GeoPackage
#' @param connect Connect to database and store connection in result? Default:
#'   `FALSE`
#' @param quiet Hide printing of gdalinfo description to stdout. Default: `TRUE`
#' @return A _geopackage_ object (list containing tables, grids and vector data)
#' @export
#' @seealso [gpkg_tables()]
#' @keywords io
gpkg_read <- function(x, connect = FALSE, quiet = TRUE) {
  
  stopifnot(length(x) == 1)
  
  con <- .gpkg_connection_from_x(x)
  
  dsn <- path.expand(con@dbname)
  res <- list()
  contents <- gpkg_contents(con, create = TRUE)
  
  # read grids
  if (!all(contents$data_type %in% c("attributes", "features"))) {
    r <- try(terra::rast(dsn), silent = TRUE)
    if (inherits(r, 'try-error')) {
      grids <- list()
    } else {
      # convert to list of single-layer SpatRaster
      grids <- as.list(r)
      # assign raster table names
      names(grids) <- names(r)
    }
  } else {
    grids <- list()
  }
  
  # read vector layers (error if there aren't any)
  if (any(contents$data_type == "features")) {
    vects <- lapply(contents$table_name, function(xxx) {
      # create SpatVectorProxy
      try(terra::vect(dsn, xxx, proxy = TRUE), silent = quiet)
    })
    names(vects) <- contents$table_name
    vects <- vects[!vapply(vects, FUN.VALUE = logical(1), inherits, 'try-error')]
  } else {
    vects <- list()
  }
  
  # get attribute tables
  tables <- list()
  lattr <- contents$data_type == "attributes"
  if (any(lattr)) {
    tables <- lapply(contents$table_name[lattr], function(y) {
      gpkg_table_pragma(con, y)
    })
  }
  
  # spatial results (grid+vect+tabular) in `tables`
  res$tables <- c(grids, vects, tables)
  
  # descriptions in `gdalinfo`
  res$gdalinfo <- terra::describe(x)
  
  # verbose gdalinfo output
  if (!quiet)
    cat(res$gdalinfo, sep = "\n")
  
  if (attr(con, 'disconnect')) {
    gpkg_disconnect(con)
  }

  obj <- .geopackage(dsn = dsn, connect = connect)
  obj$tables <- res$tables
  obj  
}

#' Write data to a GeoPackage
#' @param x Vector of source file path(s), or a list containing one or more
#'   SpatRaster, SpatRasterCollection, SpatVectorProxy, or data.frame objects.
#' @param y _character_, _geopackage_, or _DBIConnection_. 
#' @param destfile _character_. Path to output GeoPackage
#' @param table_name _character_. Default `NULL` name is derived from source file.
#'   Required if `x` is a _data.frame_.
#' @param datatype _character_. Data type. Defaults to `"FLT4S"` for GeoTIFF files, `"INT2U"`
#'   otherwise. See documentation for `terra::writeRaster()`.
#' @param append _logical_. Append to existing data source? Default: `FALSE`. Setting
#'   `append=TRUE` overrides `overwrite=TRUE`
#' @param overwrite _logical_. Overwrite existing data source? Default `FALSE`.
#' @param NoData _numeric_. Value to use as GDAL `NoData` Value
#' @param auto_nodata logical. If TRUE (default), automatically select a datatype-appropriate
#'   default NoData value when `NoData = NULL`, determined by [gpkg_default_nodata()].
#'   Set to FALSE to use terra's default behavior (NaN for floats), which may trigger GDAL
#'   warnings.  Ignored if `NoData` is explicitly specified.
#' @param gdal_options _character_. Additional `gdal_options`, passed to
#'   `terra::writeRaster()`
#' @param ... Additional arguments are passed as GeoPackage creation options.
#'   See Details.
#' @details Additional, non-default GeoPackage creation options can be specified
#'   as arguments to this function. The full list of creation options can be
#'   viewed
#'   [here](https://gdal.org/en/stable/drivers/raster/gpkg.html#creation-options)
#'   or in the `gpkg_creation_options` dataset. The name of the argument is
#'   `creation_option` and the value is selected from one of the elements of
#'   `values` for that option.
#'
#'   If `x` contains source file paths, any comma-separated value (CSV) files
#'   are treated as attribute data--even if they contain a geometry column.
#'   GeoPackage source file paths are always treated as vector data sources, and
#'   only one layer will be read from the source and written to the target. If
#'   you need to read raster data from a GeoPackage first create a `SpatRaster`
#'   from the layer of interest (see `gpkg_rast()`) before passing to
#'   `gpkg_write()`. If you need to read multiple layers from any multi-layer
#'   source read them individually into suitable objects. For a source
#'   GeoPackage containing multiple layers you can use `gpkg_read()` (returns a
#'   _geopackage_ object) or `gpkg_tables()` (returns a _list_ object).
#'
#' @return Logical. `TRUE` on successful write of at least one grid.
#' @seealso [gpkg_creation_options] [gpkg_default_nodata()]
#' @export
#' @keywords io
gpkg_write <- function(x,
                       y = NULL,
                       table_name = NULL,
                       datatype = "FLT4S",
                       append = FALSE,
                       overwrite = FALSE,
                       NoData = NULL,
                       gdal_options = NULL,
                       auto_nodata = TRUE,
                       destfile = NULL,
                       ...) {
  
  if (!missing(destfile)) {
    .Deprecated(msg = "Argument `destfile` is deprecated; use `y`")
    y <- destfile
  }
  
  if (inherits(y, 'geopackage')) {
    y <- gpkg_connection(y)
  }
  
  if (inherits(y, 'DBIConnection')) {
    y <- DBI::dbGetInfo(y)$dbname
  }
  
  res <- .gpkg_process_sources(x, y,
                               table_name = table_name,
                               datatype = datatype,
                               append = append,
                               overwrite = overwrite,
                               NoData = NoData,
                               gdal_options = gdal_options,
                               auto_nodata = auto_nodata,
                               ...)
  invisible(.gpkg_postprocess_sources(res))
}

.is.file <- function(y, pattern) {
  is.character(y) && grepl(sprintf("\\.(%s)$", pattern), y, ignore.case = TRUE)
}

.gpkg_process_sources <- function(x, ...) {
  
  args <- list(...)
  
  if (!is.list(x) || is.data.frame(x)) {
    x <- list(x)
    if (!is.null(args$table_name) && nchar(args$table_name) > 0) {
      names(x) <- args$table_name
    }
  } else if (length(x) == 1 && is.null(names(x)) && 
             !is.null(args$table_name) && nchar(args$table_name) > 0) {
    names(x) <- args$table_name
  }
  
  # objects with a file source
  src_raster <- vapply(x, inherits, logical(1), c('SpatRaster', 'SpatRasterCollection'))
  src_vector <- vapply(x, inherits, logical(1), 'SpatVectorProxy')
  obj_vector <- vapply(x, inherits, logical(1), c('sf', 'SpatVector'))
  obj_attrib <- vapply(x, inherits, logical(1), 'data.frame')
  
  # pth_raster <- vapply(x, .is.file, logical(1),  "tif+|vrt|grd|png")
  # pth_vector <- vapply(x, .is.file, logical(1),  "shp|gpkg")
  # pth_attrib <- vapply(x, .is.file, logical(1),  "csv")
  pth_file <- vapply(x, .is.file, logical(1), ".*")
  
  # TODO: gdal is not used to read attributes,
  #       provide support for some other tabular data formats?
  #       arrow? openxlsx? 
  pth_attrib <- pth_file & vapply(x, .is.file, logical(1),  "csv")
  pth_raster <- rep(FALSE, length(x))  
  pth_vector <- rep(FALSE, length(x))
  
  if (any(pth_file)) {
    if (!requireNamespace("gdalraster")) {
      stop("package 'gdalraster' is required to auto-detect GDAL drivers needed to read from arbitrary file paths", call. = FALSE)
    }
    
    gdal_drv <- vapply(x, function(y) {
      if (!is.character(y)) {
        res <- ""
      } else {
        res <- gdalraster::identifyDriver(y)
        # message(res, ":", y)
        if (length(res) == 0) {
          res <- ""
        }
        res
      }
    }, character(1))
    
    drv <- gdalraster::gdal_formats()
    drm <- match(gdal_drv, drv$short_name)
    
    pth_raster <- pth_file & drv$raster[drm]
    
    # TODO: use gdalraster::inspectDataset()
    # TODO: how to handle GPKG as a raster and vector source?
    pth_raster[gdal_drv == "GPKG"] <- FALSE
    
    pth_vector <- pth_file & drv$vector[drm]
    
    # TODO: handling of CSV files as attributes/without GDAL
    #       filter drivers to subset that terra can readwrite
    pth_vector[gdal_drv == "CSV"] <- FALSE
  }
  
  
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
    y <- gpkg_write_attributes(
      destfile,
      ldsn[[x]],
      table_name = x,
      overwrite = overwrite,
      append = append
    )
    return(inherits(y, 'try-error'))
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
                                              auto_nodata = TRUE,
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
  
  nodata_unset <- missing(NoData) || is.null(NoData)
  if (is.nan(terra::NAflag(r)) && is.null(NoData) && isTRUE(auto_nodata)) {
    NoData <- gpkg_default_nodata(terra::datatype(r))
  }
  
  # warn about setting data_null
  terra_naflag <- terra::NAflag(r)
  if (((!is.null(NoData) && is.finite(NoData)) ||
        is.nan(terra_naflag)) && 
      isTRUE(auto_nodata)) {
    if (nodata_unset && is.nan(terra_naflag)) {
      message(
        "gpkg_write: setting NoData to ",
        NoData,
        "; specify NoData argument explicitly to override, or auto_nodata=FALSE to explicitly use SQL NULL in gpkg_2d_gridded_coverage_ancillary.data_null"
      )
    }
    terra_naflag <- NoData
  }
  
  res <- terra::writeRaster(
    x = r,
    filename = destfile,
    datatype = datatype,
    overwrite = overwrite,
    gdal = gdal_options,
    NAflag = terra_naflag
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
  
  if (inherits(x, 'SpatVectorProxy')) {
    x <- terra::query(x)
  }
  
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
