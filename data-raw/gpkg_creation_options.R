## code to prepare `gpkg_creation_options` dataset goes here
## source: https://gdal.org/drivers/raster/gpkg.html#creation-options
x <- read.table(sep = "\n", text =
stringi::stri_enc_toascii("RASTER_TABLE=string. Name of tile user table. By default, based on the filename (i.e. if filename is foo.gpkg, the table will be called 'foo').
APPEND_SUBDATASET=YES/NO: If set to YES, an existing GeoPackage will not be priorly destroyed, such as to be able to add new content to it. Defaults to NO.
RASTER_IDENTIFIER=string. Human-readable identifier (e.g. short name), put in the identifier column of the gpkg_contents table.
RASTER_DESCRIPTION=string. Human-readable description, put in the description column of the gpkg_contents table.
BLOCKSIZE=integer. Block size in width and height in pixels. Defaults to 256. Maximum supported is 4096. Should not be set when using a non-custom TILING_SCHEME.
BLOCKXSIZE=integer. Block width in pixels. Defaults to 256. Maximum supported is 4096.
BLOCKYSIZE=integer. Block height in pixels. Defaults to 256. Maximum supported is 4096.
TILE_FORMAT=PNG_JPEG/PNG/PNG8/JPEG/WEBP/TIFF/AUTO: Format used to store tiles. See Tile formats. Defaults to AUTO.
QUALITY=1-100: Quality setting for JPEG and WEBP compression. Default to 75.
ZLEVEL=1-9: DEFLATE compression level for PNG tiles. Default to 6.
DITHER=YES/NO: Whether to use Floyd-Steinberg dithering (for TILE_FORMAT=PNG8). Defaults to NO.
TILING_SCHEME=CUSTOM/GoogleCRS84Quad/GoogleMapsCompatible/InspireCRS84Quad/PseudoTMS_GlobalGeodetic/PseudoTMS_GlobalMercator/other. See Tiling schemes. Defaults to CUSTOM. Starting with GDAL 3.2, the value of TILING_SCHEME can also be the filename of a JSON file according to the OGC Two Dimensional Tile Matrix Set standard, a URL to such file, the radical of a definition file in the GDAL data directory (e.g. FOO for a file named tms_FOO.json) or the inline JSON definition. Note: the TILING_SCHEME option with a non-CUSTOM value is best used with the gdal_translate utility / CreateCopy() API operation. If used with gdalwarp, it requires setting the -tr switch to the exact value expected by one zoom level of the tiling scheme.
ZOOM_LEVEL_STRATEGY=AUTO/LOWER/UPPER. Strategy to determine zoom level. Only used by CreateCopy() for TILING_SCHEME different from CUSTOM. LOWER will select the zoom level immediately below the theoretical computed non-integral zoom level, leading to subsampling. On the contrary, UPPER will select the immediately above zoom level, leading to oversampling. Defaults to AUTO which selects the closest zoom level.
RESAMPLING=NEAREST/BILINEAR/CUBIC/CUBICSPLINE/LANCZOS/MODE/AVERAGE. Resampling algorithm. Only used by CreateCopy() for TILING_SCHEME different from CUSTOM. Defaults to BILINEAR.
PRECISION=floating_point_value_in_vertical_units: Smallest significant value. Only used for tile gridded coverage datasets. Defaults to 1.
UOM=string: (GDAL >= 2.3) Unit of Measurement. Only used for tiled gridded coverage datasets. Also set through SetUnitType()
FIELD_NAME=string: (GDAL >= 2.3) Field name. Only used for tiled gridded coverage datasets. Defaults to Height.
QUANTITY_DEFINITION=string: (GDAL >= 2.3) Description of the field. Only used for tiled gridded coverage datasets. Defaults to Height.
GRID_CELL_ENCODING=grid-value-is-center/grid-value-is-area/ grid-value-is-corner: (GDAL >= 2.3) Grid cell encoding. Only used for tiled gridded coverage datasets. Defaults to grid-value-is-center, when AREA_OR_POINT metadata item is not set.
VERSION=AUTO/1.0/1.1/1.2/1.3: (GDAL >= 2.2) Set GeoPackage version (for application_id and user_version fields). In AUTO mode, this will be equivalent to 1.2 starting with GDAL 2.3. 1.3 is available starting with GDAL 3.3
ADD_GPKG_OGR_CONTENTS=YES/NO: (GDAL >= 2.2) Defines whether to add a gpkg_ogr_contents table to keep feature count for vector layers, and associated triggers. Defaults to YES."
))$V1

d <- as.data.frame(do.call('rbind', lapply(strsplit(gsub("(.*?)[\\:\\.] (.*)", "\\1;;;;;\\2", x), split = ";;;;;", fixed = TRUE), trimws)))
p <- as.data.frame(do.call('rbind', lapply(strsplit(gsub("([A-Z_]+)=(.*)", "\\1;;;;;\\2", d$V1), split = ";;;;;", fixed = TRUE), trimws))) 
a <- lapply(strsplit(p$V2, split = "/", fixed = TRUE), trimws)

gpkg_creation_options <- cbind(p, I(a), x)
colnames(gpkg_creation_options) <- c("creation_option", "value_string", "values", "description")

usethis::use_data(gpkg_creation_options, overwrite = TRUE)
