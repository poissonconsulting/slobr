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
        fileInput(ns("dbpath"), label = "Select database",
                  accept = c("sqlite", "db")) %>%
          info_tooltip("Must be a SQLite database"),
        shinyjs::hidden(div(id = ns("div_dbhelp"), 
                            helpText("This is not a SQLite database"))),
        shinyjs::hidden(
          div(id = ns("div_input"),
              uiOutput(ns("ui_table_name")),
              br(),
              fluidRow(align = "center",
                       button(ns("read"), "read cell(s)", "download", "primary"),
                       button(ns("read_column"), "read column(s)", 
                              "download", "primary"),
                       br2(),
                       button(ns("init_write"), "write cell", 
                              "upload", "success")),
              br2(),
              actionLink("other_link", label =  "Other options") %>%
                bs_attach_collapse("other"),
              bs_collapse(
                id = "other",
                content = fluidRow(align = "center",
                                   br(),
                                   button(ns("delete"), "delete cell(s)",
                                          "trash", "danger"),
                                   button(ns("delete_column"), "delete column(s)", 
                                          "trash", "danger"),
                                   br2(),
                                   button(ns("read_table"), "read table",
                                          "download", "primary"),
                                   br2(),
                                   button(ns("init_add_column"), "add column", 
                                          "table", "success") 
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
        ),
        downloadButton(ns("read_handler"), label = NULL,
                       style = "visibility: hidden;"),
        downloadButton(ns("read_column_handler"), label = NULL,
                       style = "visibility: hidden;"),
        downloadButton(ns("read_table_handler"), label = NULL,
                       style = "visibility: hidden;")
      ))
  )
}

# Module Server

#' @rdname mod_readwrite
#' @keywords internal

mod_readwrite_server <- function(input, output, session){
  
  options(shiny.maxRequestSize = 3000*1024^2)
  ns <- session$ns 
  
  path <- getShinyOption("path")
  
  rv <- reactiveValues(conn = db_connect(path),
                       tbnames = table_names(db_connect(path)),
                       table = table_read(table_names(db_connect(path))[1], 
                                          db_connect(path)))
  
  observe({
    if(!is.null(rv$table)){
      shinyjs::show("div_output")
      shinyjs::show("div_input")
    } else {
      shinyjs::hide("div_output")
      shinyjs::hide("div_input")
    }
  })
  
  observe({
    if(is.null(rv$table) && !is.null(input$dbpath)){
      shinyjs::show("div_dbhelp")
    } else {
      shinyjs::hide("div_dbhelp")
    }
  })
  
  observe({
    if(is.null(input$file$datapath)){
      shinyjs::disable("write")
    } else {
      shinyjs::enable("write")
    }
  })
  
  observe({
    req(input$table_name)
    if(check_column_name(input$add_column_name, 
                          input$table_name, rv$conn)){
      shinyjs::enable("add_column")
    } else {
      shinyjs::disable("add_column")
    }
  })
  
  observeEvent(input$dbpath, {
    path <- input$dbpath$datapath
    if(!check_db_extension(path)){
      rv$table <- NULL
      return()
    }
    conn <- db_connect(path)
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
  
  output$table <- DT::renderDT({
    data_table()
    })
  
  observeEvent(input$read, {
    x <- input$table_cells_selected
    y <- read_modal(x, input$table_name, rv$conn)
    if(!isTRUE(y)){
      return(showModal(y))
    }
    js <- glue("document.getElementById('{ns('read_handler')}').click();")
    shinyjs::runjs(js)
  })

  output$read_handler <- downloadHandler(
    filename = function(){
      file_name(input$table_cells_selected,
                input$table_name, rv$conn)
    },
    content = function(path){
      download_file(input$table_cells_selected,
                    input$table_name, rv$conn,
                    path)
    },
    contentType = "application/zip"
  )

  observeEvent(input$read_column, {
    x <- input$table_cells_selected
    y <- read_modal(x, input$table_name, 
                    conn = rv$conn, by = "column")
    if(!isTRUE(y)){
      return(showModal(y))
    }
    js <- glue("document.getElementById('{ns('read_column_handler')}').click();")
    shinyjs::runjs(js)
  })

  output$read_column_handler <- downloadHandler(
    filename = function(){
      file_name(input$table_cells_selected,
                input$table_name, rv$conn, by = "column")
    },
    content = function(path){
      download_file(input$table_cells_selected,
                    input$table_name, rv$conn,
                    path, by = "column")
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$read_table, {
    js <- glue("document.getElementById('{ns('read_table_handler')}').click();")
    shinyjs::runjs(js)
  })
  
  output$read_table_handler <- downloadHandler(
    filename = function(){
      file_name(input$table_cells_selected,
                input$table_name, rv$conn, by = "table")
    },
    content = function(path){
      download_file(input$table_cells_selected,
                    input$table_name, rv$conn,
                    path, by = "table")
    },
    contentType = "application/zip"
  )

  observeEvent(input$delete, {
    x <- input$table_cells_selected
    y <- delete_modal(x, input$table_name, rv$conn)
    if(isTRUE(y)){
      return({
        delete_flob(x, input$table_name, rv$conn)
        rv$table <- table_read(input$table_name, rv$conn)
      })
    }
    showModal(y)
  })

  observeEvent(input$delete_column, {
    x <- input$table_cells_selected
    y <- delete_modal(x, input$table_name, rv$conn)
    if(isTRUE(y)){
      return({
        delete_flob(x, input$table_name, rv$conn, by = "column")
        rv$table <- table_read(input$table_name, rv$conn)
      })
    }
    showModal(y)
  })

  observeEvent(input$init_write, {
    x <- input$table_cells_selected
    showModal(write_modal(x, input$table_name, rv$conn, ns = ns))
  })

  observeEvent(input$write, {
    x <- input$table_cells_selected
    path <- input$file$datapath
    name <- rm_ext(input$file$name)
    send_flob(x, input$table_name, rv$conn, path, name)
    rv$table <- table_read(input$table_name, rv$conn)
    reset('file')
  })

  observeEvent(input$init_add_column, {
    showModal(add_column_modal(ns = ns))
  })

  observeEvent(input$add_column, {
    column_name <- input$add_column_name
    add_column(column_name, input$table_name,
                             rv$conn)
    rv$table <- table_read(input$table_name, rv$conn)
    reset('add_column_name')
  })
  
}

