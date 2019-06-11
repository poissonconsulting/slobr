cell_display <- function(ext){
  sapply(ext, function(x){
    if(x == "empty"){
      return("< ... >")
    }
    paste(as.character(icon(ext_icon(x))), x)
  })
}

ext_icon <- function(ext){
  y <- rm_null(sapply(file_types, function(x){
    x$icon[ext %in% x$ext]
  }))
  if(length(y))
    return(paste0(y, "-o"))
  "file-o"
}

info_popover <- function(input, content, placement = c("bottom"), 
                         title = "How to use table"){
  input %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(
          title = title,
          content = content,
          placement = placement
        ))
}

label_container <- function(x){
  tags$div(tags$label(x), class = "form-group shiny-input-container")
}

modal <- function(x, footer = NULL, title = "oops"){
  modalDialog(
    title = title,
    x,
    easyClose = TRUE,
    footer = footer
  )
}

write_modal <- function(x, ns){
  msg1 <- "Please select only one cell"
  msg2 <- "Please select at least one cell"
  if(nrow(x) == 0){
    return(modal(msg2))
  }
  if(nrow(x) > 1){
    return(modal(msg1))
  }
  modal(fileInput(ns("file"), label = NULL), 
        title = "Select file",
        footer = actionButton(ns("write"), label = "write", 
                              icon = icon("upload")))
  
}

read_modal <- function(x){
  msg1 <- "Please select at least one cell"
  if(nrow(x) == 0){
    return(modal(msg1))
  }
  TRUE
}

delete_modal <- function(x){
  msg1 <- "Please select at least one cell"
  if(nrow(x) == 0){
    return(modal(msg1))
  }
  TRUE
}

read_column_modal <- function(ns){
  modal(
    uiOutput(ns("ui_column_name")),
    title = "Select column",
    footer = downloadButton(ns("read_column"),
                            label = "read", 
                            icon = icon("download")))
}

add_column_modal <- function(ns){
  modal(
    textInput(ns("add_column_name"), label = NULL, placeholder = "NewColumn"),
    title = "Enter new column name",
    actionButton(ns("add_column"), label = "add column", 
                 icon = icon("table"))
  )
}


