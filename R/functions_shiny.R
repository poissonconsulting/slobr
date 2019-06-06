checkbox <- function(col, row, ext, ns, ...){
  as.character(checkboxInput(ns(glue("_checkrows_{col}_{row}")), 
                            label = ext, value = FALSE, ...))
}

checkboxes <- function(col, ext, row, ns, ...){
  col <- rep(col, length(row))
  sapply(seq_along(row), function(x){
    checkbox(col[x], row = row[x], ext = ext[x], ns = ns, ...)
  })
}



