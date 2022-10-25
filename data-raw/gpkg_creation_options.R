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
ADD_GPKG_OGR_CONTENTS=YES/NO: (GDAL >= 2.2) Defines whether to add a gpkg_ogr_contents table to keep feature count for vector layers, and associated triggers. Defaults to YES.
DATETIME_FORMAT=WITH_TZ/UTC: (GDAL >= 3.2) Defines whether to keep the DateTime values in the time zones as used in the data source (WITH_TZ), or to convert the date and time expressions to UTC (Coordinated Universal Time). Defaults to WITH_TZ. Pedantically, non-UTC time zones are not currently supported by GeoPackage v1.3 (see https://github.com/opengeospatial/geopackage/issues/530). When using UTC format, with a unspecified timezone, UTC will be assumed.
GEOMETRY_NAME: Column to use for the geometry column. Default to “geom”. Note: This option was called GEOMETRY_COLUMN in releases before GDAL 2
GEOMETRY_NULLABLE: Whether the values of the geometry column can be NULL. Can be set to NO so that geometry is required. Default to “YES”
FID: Column name to use for the OGR FID (primary key in the SQLite database). Default to “fid”
OVERWRITE: If set to “YES” will delete any existing layers that have the same name as the layer being created. Default to NO
SPATIAL_INDEX: If set to “YES” will create a spatial index for this layer. Default to YES
PRECISION: This may be “YES” to force new fields created on this layer to try and represent the width of text fields (in terms of UTF-8 characters, not bytes), if available using TEXT(width) types. If “NO” then the type TEXT will be used instead. The default is “YES”.
TRUNCATE_FIELDS: This may be “YES” to force truncated of field values that exceed the maximum allowed width of text fields, and also to “fix” the passed string if needed to make it a valid UTF-8 string. If “NO” then the value is not truncated nor modified. The default is “NO”.
IDENTIFIER=string: Identifier of the layer, as put in the contents table.
DESCRIPTION=string: Description of the layer, as put in the contents table.
ASPATIAL_VARIANT=GPKG_ATTRIBUTES/NOT_REGISTERED: (GDAL >=2.2) How to register non spatial tables. Defaults to GPKG_ATTRIBUTES in GDAL 2.2 or later (behavior in previous version was equivalent to OGR_ASPATIAL). Starting with GeoPackage 1.2, non spatial tables are part of the specification. They are recorded with data_type=”attributes” in the gpkg_contents table. This is only compatible of GDAL 2.2 or later. It is also possible to use the NOT_REGISTERED option, in which case the non spatial table is not registered at all in any GeoPackage system tables. Priorly, in OGR 2.0 and 2.1, the “aspatial” extension had been developed for similar purposes, so if selecting OGR_ASPATIAL, non spatial tables will be recorded with data_type=”aspatial” and the “aspatial” extension was declared in the gpkg_extensions table. Starting with GDAL 3.3, OGR_ASPATIAL is no longer available on creation."
))$V1

d <- as.data.frame(do.call('rbind', lapply(strsplit(gsub("(.*?)[\\:\\.] (.*)", "\\1;;;;;\\2", x), split = ";;;;;", fixed = TRUE), trimws)))
p <- as.data.frame(do.call('rbind', lapply(strsplit(gsub("([A-Z_]+)=(.*)", "\\1;;;;;\\2", d$V1), split = ";;;;;", fixed = TRUE), trimws))) 
a <- lapply(strsplit(p$V2, split = "/", fixed = TRUE), trimws)

gpkg_creation_options <- cbind(p, I(a), x)
colnames(gpkg_creation_options) <- c("creation_option", "value_string", "values", "description")

usethis::use_data(gpkg_creation_options, overwrite = TRUE)
