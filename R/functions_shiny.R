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

info_modal <- function(input, modal_id){
  input %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_attach_modal(
          modal_id
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

read_modal <- function(x, table_name, conn){
  msg1 <- "Please select at least one cell"
  msg2 <- "There isn't a file there!"
  if(nrow(x) == 0){
    return(modal(msg1))
  }
  y <- get_column_flobs(x, table_name, conn)
  if(!length(y))
    return(modal(msg2))
  TRUE
}

delete_modal <- function(x, table_name, conn){
  msg1 <- "Please select at least one cell"
  msg2 <- "There isn't a file there!"
  
  if(nrow(x) == 0){
    return(modal(msg1))
  }
  y <- get_column_flobs(x, table_name, conn)
  if(!length(y))
    return(modal(msg2))
  TRUE
}

add_column_modal <- function(ns){
  modal(
    textInput(ns("add_column_name"), label = NULL, placeholder = "NewColumn"),
    title = "Enter new column name",
    actionButton(ns("add_column"), label = "add column", 
                 icon = icon("table"))
  )
}


