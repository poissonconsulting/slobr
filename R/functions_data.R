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

table_matrix <- function(table_name, conn){
  table <- table_read(table_name, conn)
  columns <- blob_columns(table_name, conn)
  y <- nrow(table)
  z <- do.call(rbind, lapply(columns, function(x){
    matrix(c(1:y, rep(x, y)), ncol = 2, byrow = FALSE)
  }))
  z
}

get_matrix <- function(x, table_name, conn, by = FALSE){
  switch(by,
         column = column_matrix(x, table_name, conn),
         table =  table_matrix(table_name, conn),
         x)
}

key_matrix <- function(x, table_name, conn){
  if(length(x) == 0) return()
  table <- table_read(table_name, conn)
  blob_cols <- blob_columns(table_name, conn)
  lapply(1:nrow(x), function(y){
    z <- x[y,]
    row <- z[1]
    list(
      column_name = names(table)[z[2]],
      key = table[row, -blob_cols]
    )
  })
}

get_flobs <- function(x, table_name, conn, by = "cell"){
  x <- get_matrix(x, table_name, conn, by)
  key <- key_matrix(x, table_name, conn)
  print(key)
  y <- lapply(key, function(x) {
    key <- x$key
    column_name <- x$column_name
    y <- try(dbflobr::read_flob(column_name, table_name, key, conn), silent = TRUE)
    if(is_err(y))
      return(NULL)
    y
  }) 
  names(y) <- sapply(key, function(x) x$column_name)
  rm_null(y)
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

delete_flob <- function(x, table_name, conn, by = "cell"){
  x <- get_matrix(x, table_name, conn, by)
  key <- key_matrix(x, table_name, conn)
  y <- lapply(key, function(x) {
    key <- x$key
    column_name <- x$column_name
    try(dbflobr::delete_flob(column_name, table_name, key, conn), silent = TRUE)
  }) 
}

file_name <- function(x, table_name, conn, by = "cell"){
  flobs <- get_flobs(x, table_name, conn, by)
  if(length(flobs) > 1)
    return(glue("{table_name}.zip"))
  flob <- flobs[[1]]
  ext <- flobr::flob_ext(flob)
  name <- flobr::flob_name(flob)
  glue("{name}.{ext}")
}

download_file <- function(x, table_name, conn, path, by = "cell"){
  flobs <- get_flobs(x, table_name, conn, by)
  if(length(flobs) > 1)
    return({
      for(i in unique(names(flobs))){
        dir.create(i)
      }
      for(i in flobs){
        flobr::unflob(i, file.path(names(i), flobr::flob_name(i)))
      }
      zip(path, names(flobs))
      for(i in names(flobs)){
        unlink(i, recursive = TRUE)
      }
      })
  flob <- flobs[[1]]         
  flobr::unflob(flob, path)
}

add_column <- function(column_name, table_name, conn){
  dbflobr::add_blob_column(column_name,
                           table_name,
                           conn)
}

