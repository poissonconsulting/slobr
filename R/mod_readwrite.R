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
                     choices = c("Write" = "write", 
                                 "Read" = "read"), 
                     selected = "read"),
        uiOutput(ns("ui_table_name")),
        downloadLink(ns("downloadData"),label="")
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
    req(input$mode)
    flob_datatable(input$table_name, pool, 
                   mode = input$mode, ns = ns)
  })
  
  table <- reactive({
    req(input$table_name)
    table_read(input$table_name, pool)
  })
  
  output$table <- DT::renderDT({data_table()})
  
  download_flob <- function(column_name, table_name, key, conn, ns){
    flob <- dbflobr::read_flob(column_name, 
                               table_name,
                               key, 
                               conn)
    ext <- flobr::flob_ext(flob)
    
    output$downloadData <<- downloadHandler(filename = glue("flob.{ext}"),
                                            content = function(file) flobr::unflob(flob, file))
    id <- glue("{ns('downloadData')}")
    jsinject <- paste0("setTimeout(function(){window.open($('#", id, "').attr('href'))}, 100);")
    session$sendCustomMessage(type = 'jsCode', list(value = jsinject)) 
  }

  observeEvent(input$lastClick, {
    table <- table()
    table_name <- input$table_name
    id <- strsplit(input$lastClick, "_")[[1]]
    col <- id[4]
    type <- id[5]
    row <- as.numeric(id[6])
    
    blob_cols <- blob_column_names(table_name = input$table_name, pool)
    key <- table[row, -which(names(table) %in% blob_cols)]

    if(type == "Download"){
      return({
        download_flob(column_name = col, 
                      table_name = table_name,
                      key = key, 
                      conn = pool$fetch(), 
                      ns = ns)
      })
    }
    
  })
}
    
## To be copied in the UI
# mod_readwrite_ui("readwrite_ui_1")
    
## To be copied in the server
# callModule(mod_readwrite_server, "readwrite_ui_1")
 
