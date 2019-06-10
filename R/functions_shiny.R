cell_display <- function(ext){
  sapply(ext, function(x){
    if(x == "empty"){
      return("< ... >")
    }
    glue("< {x} file >")
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

