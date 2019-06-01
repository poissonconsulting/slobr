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
        br(),
        radioButtons(ns("mode"), label = "Mode", 
                     choices = c("Write", "Read"), 
                     selected = "Read"),
        uiOutput(ns("ui_table_name"))
      ),
      mainPanel(
        DT::dataTableOutput(ns("table"))
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
  
  table <- reactive({
    req(input$table_name)
    data_table(pool, input$table_name)
  })
  
  output$table <- DT::renderDataTable({table()})
}
    
## To be copied in the UI
# mod_readwrite_ui("readwrite_ui_1")
    
## To be copied in the server
# callModule(mod_readwrite_server, "readwrite_ui_1")
 
