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
#' @export 
#' @importFrom shiny NS tagList 
mod_readwrite_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("ui_table_name")),
        downloadButton(ns("read_handler"), label = NULL,
                       style = "visibility: hidden;"),
        ########## ---------- Read files ---------- ##########
        br(),
        fluidRow(align = "center",
                 actionButton(ns("read"), "read", icon = icon("download")),
                 
                 actionButton(ns("init_write"), label = "write", icon = icon("upload")),
                 actionButton(ns("delete"), "delete", icon = icon("trash"))),
        br2(),
        actionLink("other_link", label =  "Other options") %>%
            bs_attach_collapse("other"),
            bs_collapse(
              id = "other",
              content = fluidRow(align = "center",
                br(),
                actionButton(ns("init_read_column"), 
                             label = "read column", 
                             icon = icon("download")),
              downloadButton(ns("read_table"), label = "read table", 
                             icon = icon("download")),
              actionButton(ns("init_add_column"), label = "add column", 
                           icon = icon("table")) 
              ))),
      mainPanel(
        label_container("Select cell(s) to read/write/delete file(s)") %>%
          info_popover("Files appear in the table as < ext file >, 
          where 'ext' is the file extension. 
          Any empty cell appears as < ... >. 
                       Files may be written to empty cells
                       or may replace existing files. 
                       You may read and delete files from multiple cells at once. 
                       You may write files to only one cell at a time."),
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
                  DT::DTOutput(ns("table")))
        
      ))
    )
}
    
# Module Server
    
#' @rdname mod_readwrite
#' @export
#' @keywords internal
    
mod_readwrite_server <- function(input, output, session){
  ns <- session$ns
  
  pool <- getShinyOption("pool")
  
  output$ui_table_name <- renderUI({
    selectInput(ns("table_name"), label = "Select table", 
                choices = table_names(pool))
  })
  
  output$ui_column_name <- renderUI({
    selectInput(ns("read_column_name"), label = NULL, 
                choices = blob_column_names(table_name = input$table_name, 
                                       pool))
  })
  
  rv <- reactiveValues(table = table_read(table_names(pool)[1], conn = pool))
  
  data_table <- reactive({
    req(input$table_name)
    flob_datatable(rv$table, input$table_name, pool, ns = ns)
  })

  output$table <- DT::renderDT({data_table()})
  
  observeEvent(input$read, {
    x <- input$table_cells_selected
    if(!isTRUE(read_modal(x))){
      return(showModal(read_modal(x)))
    }
    js <- glue("document.getElementById('{ns('read_handler')}').click();")
    shinyjs::runjs(js)
  })
  
  output$read_handler <- downloadHandler(
    filename = function(){
      glue("slobr-files_{Sys.Date()}.zip")
    },
    content = function(path){
      x <- input$table_cells_selected
      flobs <- get_flobs(x, input$table_name, pool$fetch())
      files <- get_unflobs(flobs)
      zip(path, files)
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
    send_flob(path, x, input$table_name, pool$fetch())
    rv$table <- table_read(input$table_name, pool)
    reset('file')
  })
  
  observeEvent(input$delete, {
    x <- input$table_cells_selected
    if(isTRUE(delete_modal(x))){
      return({
        delete_flobs(x, input$table_name, pool$fetch())
        rv$table <- table_read(input$table_name, pool)
      })
    }
    showModal(delete_modal(x))
  })
  
  observeEvent(input$init_add_column, {
    showModal(add_column_modal(ns = ns))
  })
  
  observeEvent(input$add_column, {
    column_name <- input$add_column_name
    dbflobr::add_blob_column(column_name, 
                             input$table_name, 
                             pool$fetch())
    rv$table <- table_read(input$table_name, pool)
    reset('add_column_name')
  })
  
  observeEvent(input$init_read_column, {
    showModal(read_column_modal(ns = ns))
  })
  
  output$read_column <- downloadHandler(
    filename = function(){
      glue("slobr-files_{Sys.Date()}.zip")
    },
    content = function(path){
      column_name <- input$read_column_name
      x <- column_matrix(column_name, input$table_name, pool)
      flobs <- get_flobs(x, input$table_name, pool$fetch())
      files <- get_unflobs(flobs)
      zip(path, files)
    },
    contentType = "application/zip"
  )
  
  output$read_table <- downloadHandler(
    filename = function(){
      glue("slobr-files_{Sys.Date()}.zip")
    },
    content = function(path){
      table_name <- input$table_name
      blob_cols <- blob_column_names(table_name, pool)
      print(blob_cols)
      files <- rm_null(sapply(blob_cols, function(y){
        z <- column_matrix(y, table_name, pool$fetch())
        flob <- get_flobs(z, input$table_name, pool$fetch())
        get_unflobs(flob)
      }, USE.NAMES = FALSE))
      zip(path, unlist(files))
    },
    contentType = "application/zip"
  )
}
 
