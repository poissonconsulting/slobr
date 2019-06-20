flob_ext <- function(x){
  class(x) <- c("flob", "blob")
  y <- try(flobr::check_flob(x), silent = TRUE)
  
  if(is_err(y)){
    return("empty")
  }
  flobr::flob_ext(y)
}

flob_exts <- function(x){
  y <- character(length(x))
  for(i in seq_along(x)){
    y[i] <- flob_ext(x[i])
  }
  y
}

column_matrix <- function(x, table_name, conn){
  columns <- unique(x[,2])
  y <- nrow(table_read(table_name, conn))
  do.call(rbind, lapply(columns, function(x){
    matrix(c(1:y, rep(x, y)), ncol = 2, byrow = FALSE)
  }))
}

key_matrix <- function(x, table_name, conn){
  if(length(x) == 0) return()
  table <- table_read(table_name, conn)
  blob_cols <- blob_columns(table_name, conn)
  lapply(1:nrow(x), function(y){
    z <- x[y,]
    row <- z[1]
    list(
      column_name = names(table)[z[2] + 1],
      key = table[row, -blob_cols]
    )
  })
}

# colname_matrix <- function(x, table_name, conn){
#   
# }

get_flobs <- function(x, table_name, conn, by_column = FALSE){
  if(by_column){
    x <- column_matrix(x, table_name, conn)
  }
  key <- key_matrix(x, table_name, conn)
  y <- lapply(key, function(x) {
    key <- x$key
    column_name <- x$column_name
    y <- try(dbflobr::read_flob(column_name, table_name, key, conn), silent = TRUE)
    if(is_err(y))
      return(NULL)
    y
  }) 
  rm_null(y)
}

get_files <- function(x, table_name, conn, by_column = FALSE){
  flobs <- get_flobs(x, table_name, conn, by_column)
  sapply(seq_along(flobs), function(x) {
    y <- flobs[[x]]
    name <- flobr::flob_name(y)
    ext <- flobr::flob_ext(y)
    flobr::unflob(y, paste0(name, ".", ext))
  }, USE.NAMES = FALSE)
}

get_files_table <- function(table_name, conn){
  blob_cols <- blob_columns(table_name, conn)
  x <- matrix(c(rep(1, length(blob_cols)), blob_cols -1), 
                ncol = 2, byrow = FALSE)
  get_files(x, table_name, conn, by_column = TRUE)
}

send_flob <- function(x, table_name, conn, path){
  if(nrow(x) > 1) return()
  key <- key_matrix(x, table_name, conn)[[1]]
  flob <- flobr::flob(path)
  try(dbflobr::write_flob(flob, column_name = key$column_name, 
                      table_name = table_name, 
                      key = key$key, 
                      conn = conn, exists = TRUE), silent = TRUE)
}

delete_flob <- function(x, table_name, conn, by_column = FALSE){
  if(by_column){
    x <- column_matrix(x, table_name, conn)
  }
  key <- key_matrix(x, table_name, conn)
  y <- lapply(key, function(x) {
    key <- x$key
    column_name <- x$column_name
    try(dbflobr::delete_flob(column_name, table_name, key, conn), silent = TRUE)
  }) 
}

file_name <- function(x, table_name, conn, by_column = FALSE){
  flobs <- get_flobs(x, table_name, conn, by_column)
  if(length(flobs) > 1)
    return(glue("{table_name}-.zip"))
  flob <- flobs[[1]]
  ext <- flobr::flob_ext(flob)
  name <- flobr::flob_name(flob)
  glue("{name}.{ext}")
}

download_file <- function(x, table_name, conn, path, by_column = FALSE){
  flobs <- get_flobs(x, table_name, conn, by_column)
  if(length(flobs) > 1)
    return({
      files <- get_files(x, table_name, conn, by_column = FALSE)
      zip(path, files)
      unlink(files)
      })
  flob <- flobs[[1]]         
  flobr::unflob(flob, path)
}

add_column <- function(column_name, table_name, conn){
  dbflobr::add_blob_column(column_name,
                           table_name,
                           conn)
}

