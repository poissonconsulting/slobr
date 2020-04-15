# copied from readwritesqlite
check_sqlite_connection <- function(x, connected = NA, x_name = substitute(x), error = TRUE) {
  x_name <- chk_deparse(x_name)
  chk_lgl(connected)
  chk_flag(error)
  chk_s4_class(x, "SQLiteConnection", x_name = x_name)
  if (vld_true(connected) && !dbIsValid(x)) {
    chk_fail(x_name, " must be connected", error = error)
  }
  else if (vld_false(connected) && dbIsValid(x)) {
    chk_fail(x_name, " must be disconnected", error = error)
  }
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

chk_deparse <- function(x){
  if (!is.character(x)) {
    x <- deparse(x)
  }
  if (vld_true(is.na(x))) {
    x <- "NA"
  }
  if (!is_string(x)) {
    err(substitute(x), " must be a string")
  }
  x
}

is_string <- function(x){
    (is.character(x) || is.factor(x)) && length(x) == 1 && !is.na(x)
}

chk_fail <- function (..., error) {
  if (missing(error) || vld_true(error)) {
    err(...)
  }
  wrn(...)
}

