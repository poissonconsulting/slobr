buttons <- function(.fun, length, id, ns, ...){
  sapply(seq_len(length), function(x){
    as.character(.fun(ns(glue("{id}{x}")),
                 onclick = glue('Shiny.onInputChange(\"{ns(\"lastClick\")}\",  this.id)'), ...))
  })
}

download_buttons <- function(length, id, ns, ...){
  buttons(.fun = actionButton, length = length, id = id, ns = ns, ...)
}

upload_buttons <- function(length, id, ns, ...){
  buttons(.fun = uploadButton, length = length, id = id, ns = ns, ...)
}


