# path = system.file("extdata", "demo_db.sqlite", package = "slobr")
# conn <- pool_open(path)
# table_name = "Table2"
# table <- table_read(table_name, conn)
# x <- table$flob


flob_datatable <- function(table_name, conn, ns){
  table <- table_read(table_name, conn)
  blob_cols <- blob_column_names(table_name, conn)
  
  for(i in blob_cols){
    flobs <- table[[i]]
    ext <- flob_exts(flobs)
    table[i] <- checkboxes(col = i, 
                        ext = ext,
                        row = seq_len(nrow(table)),
                        ns = ns)
  }
  DT::datatable(table, escape = FALSE, selection = 'none', options = list(
    preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')
  ))
}

flob_ext <- function(x){
  class(x) <- c("flob", "blob")
  y <- try(flobr::check_flob(x), silent = TRUE)
  
  if(is_err(y)){
    return("< empty >")
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

checked_ids <- function(input){
  rows <- names(input)[grepl(pattern = "checkrows_",names(input))]
  paste(unlist(lapply(rows,function(i){
    if(input[[i]] == TRUE){
      return(substr(i, gregexpr(pattern = "_", i)[[1]]+1, nchar(i)))
    }
  })))
}

parse_ids <- function(x){
  lapply(x, function(y){
    y <- strsplit(y, "_")[[1]]
    list(
      column_name = y[2],
      row = y[3])
  })
}

get_key <- function(x, table_name, conn){
  table <- table_read(table_name, conn)
  blob_cols <- blob_column_names(table_name, conn)
  y <- parse_ids(x)
  for(i in seq_along(y)){
    y[[i]]$key = table[y[[i]]$row, -which(names(table) %in% blob_cols)]
  }
  y
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

send_flob <- function(path, id, table_name, conn){
  key <- get_key(id, table_name, conn)[[1]]
  flob <- flobr::flob(path)
  dbflobr::write_flob(flob, column_name = key$column_name, 
                      table_name = table_name, key = key$key, 
                      conn = conn, exists = TRUE)
}

