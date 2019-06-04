button <- function(col, type, ext, row, ns, ...){
  label <- glue("{type} {ext}")
  if(type == "Empty")
    return("< no flob available >")
  as.character(actionButton(ns(glue("{col}_{type}_{row}")), 
                            label = label, ...))
}

buttons <- function(col, type, ext, row, ns, ...){
  col <- rep(col, length(row))
  sapply(seq_along(row), function(x){
    button(col[x], type = type[x], ext = ext[x], row = row[x], ns = ns, ...)
  })
}

