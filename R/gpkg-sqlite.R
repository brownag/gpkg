# gpkg sqlite tools

#' general function for table `tablename` set column X to scalar A where column Y is in vector B 
#' @param x A a `geopackage` object, path to a GeoPackage or an `SQLiteConnection`
#' @param tablename character. table name
#' @param updatecol character. column to update
#' @param updatevalue scalar value to set
#' @param wherecol character. column to constrain update
#' @param wherevector vector of values where update should be made
#' @param query_string logical. Return SQLite query rather than executing it? Default: `FALSE`
#' @noRd
#' @keywords internal
.gpkg_update_table <- function(x, tablename, updatecol, updatevalue, wherecol, wherevector, query_string = FALSE) {
  con <- .gpkg_connection_from_x(x)
  q <- sprintf("UPDATE %s SET %s = %s WHERE %s IN %s", 
               tablename, updatecol, updatevalue, wherecol, 
               paste0("(", paste0(paste0("'", wherevector, "'"), collapse = ","), ")"))
  if (query_string) return(q)
  res <- RSQLite::dbExecute(con, q)
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  res
}

#' general function to get table by table name 
#' @param x A a `geopackage` object, path to a GeoPackage or an `SQLiteConnection`
#' @param tablename character. table name
#' @param query_string logical. Return SQLite query rather than executing it? Default: `FALSE`
#' @noRd
#' @keywords internal
.gpkg_get_table <- function(x, tablename, query_string = FALSE) {
  con <- .gpkg_connection_from_x(x)
  res <- RSQLite::dbGetQuery(con, sprintf("SELECT * FROM %s", tablename))
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  res
}