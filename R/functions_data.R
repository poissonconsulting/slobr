# path = system.file("extdata", "demo_db.sqlite", package = "slobr")
# conn <- pool_open(path)
# table_name = "Table2"
# table <- table_read(table_name, conn)
# x <- table$flob

# foo is a reactiveValue used to trigger a rebuild
flob_datatable <- function(table, table_name, conn, ns){
  table <- table
  blob_cols <- blob_column_names(table_name, conn)
  for(i in blob_cols){
    flobs <- table[[i]]
    ext <- flob_exts(flobs)
    table[i] <- cell_display(ext)
  }
  DT::datatable(table, escape = FALSE, selection = list(mode = "multiple",
                                                        target = 'cell'),
                rownames = FALSE,  class = 'cell-border compact', 
                options=list(ordering = TRUE, autowidth = FALSE, scrollX = TRUE,
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
                ))
}

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

# x is matrix output of input$table_cells_selected
get_key <- function(x, table_name, conn){
  if(length(x) == 0) return()
  table <- table_read(table_name, conn)
  blob_cols <- blob_column_names(table_name, conn)
  lapply(1:nrow(x), function(y){
    z <- x[y,]
    row <- z[1]
    list(
      column_name = names(table)[z[2] + 1],
      row = row,
      key = table[row, -which(names(table) %in% blob_cols)]
    )
  })
}

get_flobs <- function(x, table_name, conn){
  key <- get_key(x, table_name, conn)
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

get_unflobs <- function(flobs){
  sapply(seq_along(flobs), function(x) {
    y <- flobs[[x]]
    ext <- flobr::flob_ext(y)
    flobr::unflob(y, paste0("file_", x,".", ext))
  }, USE.NAMES = FALSE)
}

send_flob <- function(path, x, table_name, conn){
  key <- get_key(x, table_name, conn)[[1]]
  flob <- flobr::flob(path)
  dbflobr::write_flob(flob, column_name = key$column_name, 
                      table_name = table_name, 
                      key = key$key, 
                      conn = conn, exists = TRUE)
}

delete_flobs <- function(x, table_name, conn){
  key <- get_key(x, table_name, conn)
  y <- lapply(key, function(x) {
    key <- x$key
    column_name <- x$column_name
    try(dbflobr::delete_flob(column_name, table_name, key, conn), silent = TRUE)
  }) 
}

