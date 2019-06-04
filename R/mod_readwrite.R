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
        uiOutput(ns("ui_table_name")),
        downloadLink(ns("downloadData"),label="")
      ),
      mainPanel(
        DT::DTOutput(ns("table")),
        wellPanel(
          verbatimTextOutput(ns('printMsg'))
        )
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
  
  table <- reactive({
    req(input$table_name)
    table_read(input$table_name, pool)
  })
  
  output$table <- DT::renderDT({data_table()})
  
  printText <- reactiveValues(brands = '')
  # vals <- reactiveValues(data = table())

  clicked <- reactiveValues()
  observeEvent(input$lastClick, {
    table <- table()
    row <- as.numeric(strsplit(input$lastClick, "_")[[1]][5])
    col <- strsplit(input$lastClick, "_")[[1]][4]
    blob_cols <- blob_column_names(table_name = input$table_name, pool)
    key <- table[row, -which(names(table) %in% blob_cols)]
    clicked$key <- key
    clicked$row <- row
    clicked$col <- col
    clicked$id <- input$lastClick
    flob <- try(dbflobr::read_flob(column_name = col, table_name = input$table_name,
                                                  key = key, conn = pool$fetch()), silent = TRUE)
    if(inherits(flob, "try-error")){return(print("hi"))}
    print(key)
    output$downloadData <<- downloadHandler(filename = "flob.pdf",
                                            content = function(file) flobr::unflob(flob, file))
    jsinject <- "setTimeout(function(){window.open($('#readwrite_ui_1-downloadData').attr('href'))}, 100);"
    session$sendCustomMessage(type = 'jsCode', list(value = jsinject)) 
  })
  
  output$flob_1 <- downloadHandler(
    filename = function(){
      "flob.pdf"
    },
    content = function(con){
      flobr::unflob(dbflobr::read_flob(column_name = clicked$col,
                                       table_name = input$table_name,
                                       key = clicked$key, 
                                       conn = pool$fetch()))
    }
  )
  
  
}
    
## To be copied in the UI
# mod_readwrite_ui("readwrite_ui_1")
    
## To be copied in the server
# callModule(mod_readwrite_server, "readwrite_ui_1")
 
