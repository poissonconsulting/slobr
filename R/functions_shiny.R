button <- function(col, type, ext, row, ns, ...){
  if(type == "Empty")
    return("< no file available >")
  
  label <- glue("{type} {ext}")
  onclick <- glue('Shiny.onInputChange(\"{ns(\"lastClick\")}\",  this.id)')
  
  as.character(actionButton(ns(glue("_{col}_{type}_{row}")), 
                            label = label,
                            onclick = onclick, ...))
}

buttons <- function(col, type, ext, row, ns, ...){
  col <- rep(col, length(row))
  sapply(seq_along(row), function(x){
    button(col[x], type = type[x], ext = ext[x], row = row[x], ns = ns, ...)
  })
}



