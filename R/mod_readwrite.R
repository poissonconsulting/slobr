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
mod_readwrite_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
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
        shinyjs::hidden(
          div(id = ns("div_output"),
            tags$label("Select cell(s) to read/write/delete file(s)") %>%
              shinyhelper::helper(content = "read_write", colour = "#377bb5"),
            wellPanel(style = "overflow-y:scroll; max-height: 600px",
                      shinycssloaders::withSpinner(DT::DTOutput(ns("table"))))
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
  
  conn <- getShinyOption("conn")
  
  rv <- reactiveValues(conn = conn,
                       tbnames = table_names(conn),
                       table = table_read(table_names(conn)[1], conn))
  
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
    req(input$table_name)
    if(check_column_name(input$add_column_name, 
                          input$table_name, rv$conn)){
      shinyjs::enable("add_column")
    } else {
      shinyjs::disable("add_column")
    }
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
    shinyjs::runjs(click_js(ns('read_handler')))
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
    shinyjs::runjs(click_js(ns('read_column_handler')))
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
    x <- input$table_cells_selected
    y <- read_modal(x, input$table_name, 
                    conn = rv$conn, by = "table")
    if(!isTRUE(y)){
      return(showModal(y))
    }
    shinyjs::runjs(click_js(ns('read_table_handler')))
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
    y <- delete_modal(x, input$table_name, rv$conn, by = "column")
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
    tmp <- send_flob(x, input$table_name, rv$conn, path, name)
    if(!is_try_error(tmp)){
      rv$table <- table_read(input$table_name, rv$conn)
      reset('file')
      removeModal()
    } else {
      showModal(modal(tags$p("There was a problem writing that file to the database.")))
    }
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
    removeModal()
  })
  
  session$onSessionEnded(function() {
    conn <- isolate(rv$conn)
    DBI::dbDisconnect(conn)
  })
  
}

