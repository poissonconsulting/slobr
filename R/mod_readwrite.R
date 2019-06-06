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
        br(),
        actionLink("read_link", label =  "Read files from database") %>%
          bs_attach_collapse("read"),
        bs_collapse(
          show = TRUE,
          id = "read", 
          content = tagList(
            br(),
            label_container("Download files from checked boxes") %>%
              info_tooltip("Check multiple boxes in table to download files into a .zip folder."),
            downloadButton(ns("download"), label = ".zip"),
            br()
          )),
        br(),
        actionLink("write_link", label =  "Write files to database") %>%
          bs_attach_collapse("write"),
        
        bs_collapse(
          id = "write", 
          content = tagList(
            br(),
            fileInput(ns("file"), label = "Select file and write to checked box") %>%
              info_tooltip("Select only one file and checkbox at a time"),
            actionButton(ns("write"), label = "write", icon = icon("upload"))
          )
        ),
        br(),
        actionLink("blob_link", label =  "Add new column") %>%
          bs_attach_collapse("blob"),
        bs_collapse(
          id = "blob", 
          content = tagList(
            br(),
            textInput(ns("blob_column_name"), label = "Column name", 
                      placeholder = "NameExample") %>%
              info_tooltip("Column name must not have spaces"),
            fix_ui("column name should not have spaces", 
                   condition = "check_column_name", 
                   ns = ns),
            actionButton(ns("add_blob"), label = "Add (no undo)")
          )
        )
        # hr(),
        # tags$label("Delete files"),
        # helpText("Check any number of boxes in table"),
        # actionButton(ns("delete"), "Delete", icon = icon("trash"))
      ),
      mainPanel(
        # actionButton(ns("refresh"), label = "Refresh", icon = icon("refresh")),
        # br(),
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
  
  observeEvent(input$add_blob, {
    req(input$blob_column_name)
    req(input$table_name)
    column_name <- input$blob_column_name
    table_name <- input$table_name
    conn <- pool$fetch()
    
    dbflobr::add_blob_column(column_name, table_name, conn)
  })
  
  observeEvent(input$write, {
    path <- input$file$datapath
    id <- checked()
    showModal(file_modal(id, path))
    if(is.null(file_modal(id, path))){
      send_flob(path, id, input$table_name, pool$fetch())
      reset('file')
    }
  })
  
  observeEvent(input$refresh, {
    data_table()
  })
  
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
  
  fix_server(output, "check_column_name", 
             reactive(check_column_name(input$blob_column_name)))
  
  
  
}
 
