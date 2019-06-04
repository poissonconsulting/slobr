button <- function(col, type, row, ns, ...){
  as.character(actionButton(ns(glue("{col}_{type}_{row}")), 
                            label = type, ...))
}

buttons <- function(col, type, row, ns, ...){
  col <- rep(col, length(row))
  sapply(seq_along(row), function(x){
    button(col[x], type = type[x], row = row[x], ns = ns, ...)
  })
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


