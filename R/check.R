# copied from readwritesqlite
check_sqlite_connection <- function(x, connected = NA, x_name = substitute(x), error = TRUE) {
  x_name <- chk_deparse(x_name)
  check_scalar(connected, values = c(TRUE, NA))
  check_flag(error)
  check_inherits(x, "SQLiteConnection", x_name = x_name)
  if(isTRUE(connected) && !dbIsValid(x)) {
    chk_fail(x_name, " must be connected", error = error)
  } else if(isFALSE(connected) && dbIsValid(x))
    chk_fail(x_name, " must be disconnected", error = error)
  invisible(x)
}

count_space <- function(x) sapply(gregexpr(" ", x), function(y) {sum(y >= 0)})


check_column_name <- function(x, table_name, conn){
  if(is.null(x) || x == "")
    return(FALSE)
  if(count_space(x) > 0)
    return(FALSE)
  if(column_exists(x, table_name, conn))
    return(FALSE)
  TRUE
}

check_db_extension <- function(x){
  x <- tolower(tools::file_ext(x))
  if(!(x %in% c("sqlite", "db")))
    return(FALSE)
  TRUE
}

