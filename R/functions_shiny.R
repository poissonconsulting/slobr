flob_datatable <- function(table, table_name, conn, ns){
  blob_cols <- blob_columns(table_name, conn)
  for(i in blob_cols){
    flobs <- table[[i]]
    ext <- flob_exts(flobs)
    table[i] <- cell_display(ext)
  }
  
  sfc_cols <- sfc_columns(table_name, conn)
  column_names <- column_names(table_name, conn)
  cols <- which(toupper(column_names) %in% sfc_cols)
  table[,cols] <- "< GEOMETRY >"
  
  DT::datatable(table, escape = FALSE, selection = list(mode = "multiple",
                                                        target = 'cell'),
                rownames = FALSE,  class = 'cell-border compact', 
                options = list(ordering = TRUE, 
                               autowidth = FALSE, scrollX = TRUE, 
                               columnDefs = list(list(className = 'dt-center', 
                                                      targets = "_all"))
                ))
}

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

info_tooltip <- function(input, x){
  input %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_tooltip(
          title = x, placement = "bottom"
        ))
}

label_container <- function(x){
  tags$div(tags$label(x), class = "form-group shiny-input-container")
}

modal <- function(x, footer = NULL, title = "oops..."){
  modalDialog(
    title = title,
    x,
    easyClose = TRUE,
    footer = footer
  )
}

write_modal <- function(x, table_name, conn, ns){
  msg1 <- "Please select only one cell"
  msg2 <- "Please select at least one cell"
  msg3 <- "Please select a cell from a BLOB column"
  
  column_name <- column_names(table_name, conn)[x[2] + 1]
  is_blob <- is_column_blob(column_name = column_name, 
                      table_name = table_name, 
                      conn = conn)
  if(!is_blob){
    return(modal(msg3))
  }
  if(nrow(x) == 0){
    return(modal(msg2))
  }
  if(nrow(x) > 1){
    return(modal(msg1))
  }
  modal(
    fileInput(ns("file"), label = NULL), 
        title = "Select file",
        footer = actionButton(ns("write"), label = "write", 
                              icon = icon("upload")))
  
}

read_modal <- function(x, table_name, conn, by = "cell"){
  msg1 <- "Please select at least one cell."
  msg2 <- "There is either no file there, or we are unable to read it from the 
  database."
  if(nrow(x) == 0){
    return(modal(msg1))
  }
  y <- get_flobs(x, table_name, conn, by)
  if(!length(y))
    return(modal(msg2))
  TRUE
}

delete_modal <- function(x, table_name, conn, by = "cell"){
  msg1 <- "Please select at least one cell"
  msg2 <- "There isn't a file there!"
  
  if(nrow(x) == 0){
    return(modal(msg1))
  }
  y <- get_flobs(x, table_name, conn, by)
  if(!length(y))
    return(modal(msg2))
  TRUE
}

add_column_modal <- function(ns){
  tagList(
    bsplus::use_bs_tooltip(),
    modal(
      textInput(ns("add_column_name"), label = "New column name", placeholder = "NewColumn") %>%
        info_tooltip("Column name cannot include spaces and cannot already exist in the table (case-insensitive)."),
      title = NULL,
      actionButton(ns("add_column"), label = "add column", 
                   icon = icon("table"))
    )
  )
}


