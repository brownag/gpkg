# gpkg sqlite tools

#' Update a Table by Name
#' 
#' For a given table, set column `updatecol` to scalar `updatevalue` where column `wherecol` is in vector `wherevector`.
#' 
#' @param x A _geopackage_ object, path to a GeoPackage or an `SQLiteConnection`.
#' @param table_name _character_. Table name.
#' @param updatecol _character_. Column to update.
#' @param updatevalue _character_, _numeric_, etc.; A scalar value to set.
#' @param wherecol _character_. Column used to constrain update.
#' @param wherevector _character_, _numeric_, etc.; Vector of values where update should be made.
#' @param query_string _logical_. Return SQLite query rather than executing it? Default: `FALSE`
#' @export
gpkg_update_table <- function(x, table_name, 
                              updatecol, updatevalue, 
                              wherecol = NULL, 
                              wherevector = NULL, 
                              query_string = FALSE) {
  con <- .gpkg_connection_from_x(x)
  q <- sprintf("UPDATE %s SET %s = %s %s",
               table_name, updatecol, updatevalue, 
               ifelse(!is.null(wherecol), sprintf("WHERE %s IN %s", wherecol,
                                                  paste0("(", paste0(paste0("'", wherevector, "'"), collapse = ","), ")")), ""))
  if (query_string) return(q)
  res <- RSQLite::dbExecute(con, q)
  if (attr(con, 'disconnect')) {
    DBI::dbDisconnect(con)
  }
  res
}
