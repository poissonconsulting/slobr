# path = system.file("extdata", "demo_db.sqlite", package = "slobr")
# conn <- pool_open(path)
# table_name = "Table2"
# table <- table_read(table_name, conn)
# x <- table$flob


flob_datatable <- function(table_name, conn, mode, ns){
  table <- table_read(table_name, conn)
  blob_cols <- blob_column_names(table_name, conn)
  
  for(i in blob_cols){
    flobs <- table[[i]]
    ext <- flob_exts(flobs)
    table[i] <- buttons(col = i, 
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
  tmp <- try(flobr::check_flob(x), silent = TRUE)
  
  is_err <- inherits(tmp, "try-error")
  if(is_err){
    return("< empty >")
  }
  flobr::flob_ext(tmp)
}

flob_exts <- function(x){
  out <- character(length(x))
  for(i in seq_along(x)){
    out[i] <- flob_ext(x[i])
  }
  out
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
  lapply(key, function(x) {
    key <- x$key
    column_name <- x$column_name
    dbflobr::read_flob(column_name, table_name, key, conn)
  }) 
}

get_unflobs <- function(flobs){
  sapply(seq_along(flobs), function(x) {
    y <- flobs[[x]]
    ext <- flobr::flob_ext(y)
    flobr::unflob(y, paste0("file_", x,".", ext))
  }, USE.NAMES = FALSE)
}


