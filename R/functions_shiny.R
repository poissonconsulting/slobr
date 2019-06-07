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

info_tooltip <- function(input, x){
  input %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_tooltip(
          x
        ))
}

label_container <- function(x){
  tags$div(tags$label(x), class = "form-group shiny-input-container")
}

modal <- function(x){
  modalDialog(
    title = "oops...",
    x,
    easyClose = TRUE,
    footer = NULL
  )
}

