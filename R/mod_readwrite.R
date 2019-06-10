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
        downloadButton(ns("download"), label = NULL,
                       style = "visibility: hidden;"),
        ########## ---------- Read files ---------- ##########
        br(),
        fluidRow(align = "center",
                 actionButton(ns("init_download"), "read", icon = icon("download")),
                 bs_modal(
                   id = "modal_write", 
                   title = "Select a file and click 'write'",
                   body = tagList(fileInput(ns("file"), label = NULL)),
                   footer = tags$span(
                     actionButton(ns("write"), label = "write", icon = icon("upload"))
                   )
                 ),
                 actionButton(ns("init_write"), label = "write", icon = icon("upload")) %>%
                   bs_attach_modal("modal_write"),
                 actionButton(ns("delete"), "delete", icon = icon("trash"))),
        br2(),
        actionLink("other_link", label =  "Other options") %>%
            bs_attach_collapse("other"),
            bs_collapse(
              id = "other",
              content = fluidRow(align = "center",
                br(),
                bs_modal(
                  id = "modal_read_column", 
                  title = "Select a column and click 'read'",
                  body = tagList(uiOutput(ns("ui_column_name"))),
                  footer = tags$span(
                    actionButton(ns("read_column"), label = "read", icon = icon("download"))
                  )
                ),
                actionButton(ns("init_read_column"), 
                             label = "read column", 
                             icon = icon("download"))  %>%
                  bs_attach_modal("modal_read_column"),
              actionButton(ns("read_table"), label = "read table", icon = icon("download")),
              bs_modal(
                id = "modal_add_column", 
                title = "Type a column name (no spaces) and click 'add column'",
                body = tagList(textInput(ns("add_column_name"), label = NULL)),
                footer = tags$span(
                  actionButton(ns("add_column"), label = "add column", icon = icon("upload"))
                )
              ),
              actionButton(ns("init_add_column"), label = "add column", icon = icon("table")) %>%
                bs_attach_modal("modal_add_column")
              ))),
    #     actionLink("read_link", label =  "Read files") %>%
    #       bs_attach_collapse("read"),
    #     bs_collapse(
    #       show = TRUE,
    #       id = "read", 
    #       content = tagList(
    #         br(),
    #         label_container("Download from selected checkboxes") %>%
    #           info_tooltip("Select multiple checkboxes in table to download files into a .zip folder."),
    #         # two buttons necessary here so downloadHandler can exit gracefully
    #         actionButton(ns("init_download"), ".zip", icon = icon("download")),
    #         downloadButton(ns("download"), label = NULL, 
    #                        style = "visibility: hidden;"),
    #         br()
    #       )),
    #     ########## ---------- Write files ---------- ##########
    #     br(),
    #     actionLink("write_link", label =  "Write files") %>%
    #       bs_attach_collapse("write"),
    #     bs_collapse(
    #       id = "write", 
    #       content = tagList(
    #         br(),
    #         fileInput(ns("file"), label = "Write to a selected checkbox") %>%
    #           info_tooltip("Click 'Browse...' to search for a file. 
    #                        Select only one file and checkbox at a time. 
    #                        If a file already exists, it will be replaced."),
    #         actionButton(ns("write"), label = "write", icon = icon("upload"))
    #       )
    #     ),
    #     ########## ---------- Delete files ---------- ##########
    #     br(),
    #     actionLink("delete_link", label =  "Delete files") %>%
    #       bs_attach_collapse("delete"),
    #     bs_collapse(
    #       id = "delete", 
    #       content = tagList(
    #         br(),
    #         label_container("Delete from selected checkboxes") %>%
    #           info_tooltip("Select multiple checkboxes in table to delete files from database. 
    #                        There is no undo!"),
    #         actionButton(ns("delete"), "Delete", icon = icon("trash"))
    #       )
    #     ),
    #   ########## ---------- Add column ---------- ##########
    #   br(),
    #   actionLink("blob_link", label =  "Add column") %>%
    #     bs_attach_collapse("blob"),
    #   bs_collapse(
    #     id = "blob", 
    #     content = tagList(
    #       br(),
    #       textInput(ns("blob_column_name"), label = "Column name", 
    #                 placeholder = "NameExample") %>%
    #         info_tooltip("New column name must not have spaces. There is no undo!"),
    #       fix_ui("column name should not have spaces", 
    #              condition = "check_column_name", 
    #              ns = ns),
    #       actionButton(ns("add_blob"), label = "Add (no undo)")
    #     )
    #   )),
      mainPanel(
        # actionButton(ns("refresh"), label = "Refresh", icon = icon("refresh")),
        # br(),
        DT::DTOutput(ns("table"))
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
    selectInput(ns("column_name"), label = NULL, 
                choices = column_names(table_name = input$table_name, 
                                       pool))
  })
  
  data_table <- reactive({
    req(input$table_name)
    flob_datatable(rv$table, input$table_name, pool, ns = ns)
  })

  output$table <- DT::renderDT({data_table()})
  
  rv <- reactiveValues(table = table_read(table_name = table_names(pool)[1],
                                             conn = pool))
  
  observeEvent(input$add_blob, {
    column_name <- input$blob_column_name
    if(isTRUE(blob_modal(column_name))){
      return({
        dbflobr::add_blob_column(column_name, 
                                 input$table_name, 
                                 pool$fetch())
        reset("blob_column_name")
        rv$bar <- rv$bar + 1
      })
    }
    showModal(blob_modal(column_name))
  })
  
  # observeEvent(input$write, {
  #   path <- input$file$datapath
  #   x <- input$table_cells_selected
  #   if(isTRUE(write_modal(x, path))){
  #     return({
  #       send_flob(path, x, input$table_name, pool$fetch())
  #       rv$table <- table_read(input$table_name, pool)
  #       reset('file')
  #     })
  #   }
  #   showModal(write_modal(x, path))
  # })
  
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
  
  observeEvent(input$init_download, {
    x <- input$table_cells_selected
    if(!isTRUE(read_modal(x))){
      return(showModal(read_modal(x)))
    }
    js <- glue("document.getElementById('{ns('download')}').click();")
    shinyjs::runjs(js)
  })
  
  output$download <- downloadHandler(
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
}
 
