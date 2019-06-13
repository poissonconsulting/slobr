# Module UI

#' @title   mod_readwrite_ui and mod_readwrite_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_readwrite
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
mod_readwrite_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("dbpath"), label = "Select database"),
        uiOutput(ns("ui_table_name")),
        downloadButton(ns("read_handler"), label = NULL,
                       style = "visibility: hidden;"),
        downloadButton(ns("read_column_handler"), label = NULL,
                       style = "visibility: hidden;"),
        ########## ---------- Read files ---------- ##########
        br(),
        shinyjs::hidden(
          div(id = ns("div_input"),
              fluidRow(align = "center",
                       actionButton(ns("read"), "read cell(s)", icon = icon("download")),
                       actionButton(ns("read_column"), label = "read column(s)", 
                                    icon = icon("download")),
                       br2(),
                       actionButton(ns("init_write"), label = "write cell", 
                                    icon = icon("upload"))
              ),
              br2(),
              actionLink("other_link", label =  "Other options") %>%
                bs_attach_collapse("other"),
              bs_collapse(
                id = "other",
                content = fluidRow(align = "center",
                                   br(),
                                   actionButton(ns("delete"), "delete cell(s)", icon = icon("trash")),
                                   actionButton(ns("delete_column"), "delete column(s)", icon = icon("trash")),
                                   br2(),
                                   downloadButton(ns("read_table"), label = "read table", 
                                                  icon = icon("download")),
                                   br2(),
                                   actionButton(ns("init_add_column"), label = "add column", 
                                                icon = icon("table")) 
                ))))),
      mainPanel(
        bs_modal(ns("modal_info"),
                 title = "How to use this table",
                 body = instructions,
                 footer = bs_modal_closebutton("Got it")),
        shinyjs::hidden(
          div(id = ns("div_output"),
            label_container("Select cell(s) to read/write/delete file(s)") %>%
              info_modal(ns("modal_info")),
            wellPanel(style = "overflow-y:scroll; max-height: 600px",
                      DT::DTOutput(ns("table")))
          )
        )
      ))
  )
}

# Module Server

#' @rdname mod_readwrite
#' @keywords internal

mod_readwrite_server <- function(input, output, session){
  ns <- session$ns
  
  path <- getShinyOption("path")
  
  rv <- reactiveValues(conn = pool_open(path),
                       tbnames = table_names(pool_open(path)),
                       table = table_read(table_names(pool_open(path))[1], 
                                          pool_open(path)))
  
  observe({
    if(!is.null(rv$table)){
      shinyjs::show("div_output")
      shinyjs::show("div_input")
    } 
  })
  
  observeEvent(input$dbpath, {
    conn <- pool_open(input$dbpath$datapath)
    tbnames <- table_names(conn)
    rv$conn <- conn
    rv$tbnames <- tbnames
    rv$table <- table_read(tbnames[1], conn)
  })
  
  observeEvent(input$table_name, {
    rv$table <- table_read(input$table_name, rv$conn)
  })
  
  output$ui_table_name <- renderUI({
    req(rv$tbnames)
    selectInput(ns("table_name"), label = "Select table",
                choices = rv$tbnames)
  })
  
  data_table <- reactive({
    req(input$table_name)
    flob_datatable(rv$table, input$table_name, rv$conn, ns = ns)
  })
  
  output$table <- DT::renderDT({data_table()})
  
  observeEvent(input$read, {
    x <- input$table_cells_selected
    y <- read_modal(x, input$table_name, rv$conn$fetch())
    if(!isTRUE(y)){
      return(showModal(y))
    }
    js <- glue("document.getElementById('{ns('read_handler')}').click();")
    shinyjs::runjs(js)
  })

  output$read_handler <- downloadHandler(
    filename = function(){
      file_name(input$table_cells_selected,
                input$table_name, rv$conn$fetch())
    },
    content = function(path){
      download_file(input$table_cells_selected,
                    input$table_name, rv$conn$fetch(),
                    path)
    },
    contentType = "application/zip"
  )

  observeEvent(input$read_column, {
    x <- input$table_cells_selected
    y <- read_modal(x, input$table_name, rv$conn$fetch())
    if(!isTRUE(y)){
      return(showModal(y))
    }
    js <- glue("document.getElementById('{ns('read_column_handler')}').click();")
    shinyjs::runjs(js)
  })

  output$read_column_handler <- downloadHandler(
    filename = function(){
      file_name(input$table_cells_selected,
                input$table_name, rv$conn$fetch(), column = TRUE)
    },
    content = function(path){
      download_file(input$table_cells_selected,
                    input$table_name, rv$conn$fetch(),
                    path, column = TRUE)
    },
    contentType = "application/zip"
  )

  observeEvent(input$delete, {
    x <- input$table_cells_selected
    y <- delete_modal(x, input$table_name, rv$conn$fetch())
    if(isTRUE(y)){
      return({
        delete_flobs(x, input$table_name, rv$conn$fetch())
        rv$table <- table_read(input$table_name, rv$conn)
      })
    }
    showModal(y)
  })

  observeEvent(input$delete_column, {
    x <- input$table_cells_selected
    y <- delete_modal(x, input$table_name, rv$conn$fetch())
    
    if(isTRUE(y)){
      return({
        delete_flob_column(x, input$table_name, rv$conn$fetch())
        rv$table <- table_read(input$table_name, rv$conn)
      })
    }
    showModal(y)
  })

  output$read_table <- downloadHandler(
    filename = function(){
      glue("slobr-files_{Sys.Date()}.zip")
    },
    content = function(path){
      table_name <- input$table_name
      blob_cols <- blob_column_names(table_name, rv$conn)
      files <- get_column_files(x, table_name, rv$conn, blob_cols)
      zip(path, unlist(files))
    },
    contentType = "application/zip"
  )

  observeEvent(input$init_write, {
    x <- input$table_cells_selected
    showModal(write_modal(x, ns = ns))
  })

  observeEvent(input$write, {
    x <- input$table_cells_selected
    path <- input$file$datapath
    send_flob(path, x, input$table_name, rv$conn$fetch())
    rv$table <- table_read(input$table_name, rv$conn)
    reset('file')
  })

  observeEvent(input$init_add_column, {
    showModal(add_column_modal(ns = ns))
  })

  observeEvent(input$add_column, {
    column_name <- input$add_column_name
    dbflobr::add_blob_column(column_name,
                             input$table_name,
                             rv$conn$fetch())
    rv$table <- table_read(input$table_name, rv$conn)
    reset('add_column_name')
  })
  
}

