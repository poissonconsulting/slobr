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

check_column_name <- function(x){
  y <- FALSE
  if(count_space(x) > 0)
    y <- TRUE
  y
}

write_modal <- function(x, path){
  msg1 <- "Please select only one cell"
  msg2 <- "Please select at least one cell"
  msg3 <- "Please click 'Browse...' to find the file you'd like to write"
  
  if(nrow(x) == 0){
    return(modal(msg2))
  }
  if(nrow(x) > 1){
    return(modal(msg1))
  }
  if(is.null(path)){
    return(modal(msg3))
  }
  TRUE
}

read_modal <- function(x){
  msg1 <- "Please select at least one cell"
  if(nrow(x) == 0){
    return(modal(msg1))
  }
  TRUE
}

blob_modal <- function(x){
  msg1 <- "Column name should not have spaces"
  if(check_column_name(x)){
    return(modal(msg1))
  }
  TRUE
}

delete_modal <- function(x){
  msg1 <- "Please select at least one cell"
  if(nrow(x) == 0){
    return(modal(msg1))
  }
  TRUE
}





