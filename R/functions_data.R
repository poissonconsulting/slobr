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
    type <- button_types(flobs, mode = mode)
    ext <- flob_exts(flobs)
    table[i] <- buttons(col = i, 
                        type = type, 
                        ext = ext,
                        row = seq_len(nrow(table)),
                        ns = ns)
  }
  DT::datatable(table, escape = FALSE, selection = "none")
}

button_type <- function(x, mode){
  class(x) <- c("flob", "blob")
  tmp <- try(flobr::check_flob(x), silent = TRUE)
  
  is_err <- inherits(tmp, "try-error")
  if(is_err & mode == "write"){
    return("Upload")
  }
  if(is_err & mode == "read"){
    return("Empty")
  }
  if(!is_err & mode == "write"){
    return("Replace")
  }
  "Download"
}

button_types <- function(x, mode){
  out <- character(length(x))
  for(i in seq_along(x)){
    out[i] <- button_type(x[i], mode = mode)
  }
  out
}

flob_ext <- function(x){
  class(x) <- c("flob", "blob")
  tmp <- try(flobr::check_flob(x), silent = TRUE)
  
  is_err <- inherits(tmp, "try-error")
  if(is_err){
    return("")
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



