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
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("ui_table_name")),
        downloadButton(ns("download"), label = "Download from selected"),
        fileInput(ns("upload"), label = "Upload to selected")
      ),
      mainPanel(
        DT::DTOutput(ns("table"))
      )
    )
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
  
  data_table <- reactive({
    req(input$table_name)
    flob_datatable(input$table_name, pool, ns = ns)
  })
  
  checked <- reactive({
    checked_ids(input = input)
  })
  
  output$table <- DT::renderDT({data_table()})
  
  output$download <- downloadHandler(
    filename = function(){
      glue("slobr-files_{Sys.Date()}.zip")
    },
    content = function(path){
      flobs <- get_flobs(checked(), input$table_name, pool$fetch())
      flobs <- rm_null(flobs)
      files <- get_unflobs(flobs)
      zip(path, files)
    },
    contentType = "application/zip"
  )
}
 
