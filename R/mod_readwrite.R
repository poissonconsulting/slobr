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
            # two buttons necessary here so downloadHandler can exit gracefully
            actionButton(ns("init_download"), ".zip", icon = icon("download")),
            downloadButton(ns("download"), label = NULL, style = "visibility: hidden;"),
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

    column_name <- input$blob_column_name
    if(isTRUE(blob_modal(column_name))){
      return({
        dbflobr::add_blob_column(column_name, 
                                 input$table_name, 
                                 pool$fetch())
      })
    }
    showModal(blob_modal(column_name))
  })
  
  observeEvent(input$write, {
    path <- input$file$datapath
    id <- checked()
    if(isTRUE(write_modal(id, path))){
      return({
        send_flob(path, id, input$table_name, pool$fetch())
        reset('file')
      })
    }
    showModal(write_modal(id, path))
  })
  
  observeEvent(input$refresh, {
    data_table()
  })
  
  observeEvent(input$init_download, {
    id <- checked()
    if(!isTRUE(read_modal(id))){
      return(showModal(read_modal(id)))
    }
    js <- glue("document.getElementById('{ns('download')}').click();")
    shinyjs::runjs(js)
  })
  
  output$download <- downloadHandler(
    filename = function(){
      glue("slobr-files_{Sys.Date()}.zip")
    },
    content = function(path){
      id <- checked()
      flobs <- get_flobs(id, input$table_name, pool$fetch())
      files <- get_unflobs(flobs)
      zip(path, files)
    },
    contentType = "application/zip"
  )
  
  fix_server(output, "check_column_name", 
             reactive(check_column_name(input$blob_column_name)))
  
}
 
