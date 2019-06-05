button <- function(col, row, ext, ns, ...){
  # if(type == "Empty")
  #   return("< no file available >")
  as.character(checkboxInput(ns(glue("_checkrows_{col}_{row}")), 
                            label = ext, value = FALSE, ...))
}

buttons <- function(col, ext, row, ns, ...){
  col <- rep(col, length(row))
  sapply(seq_along(row), function(x){
    button(col[x], row = row[x], ext = ext[x], ns = ns, ...)
  })
}



