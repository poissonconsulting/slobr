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
        ########## ---------- Read files ---------- ##########
        br(),
        actionLink("read_link", label =  "Read files") %>%
          bs_attach_collapse("read"),
        bs_collapse(
          show = TRUE,
          id = "read", 
          content = tagList(
            br(),
            label_container("Download from selected checkboxes") %>%
              info_tooltip("Select multiple checkboxes in table to download files into a .zip folder."),
            # two buttons necessary here so downloadHandler can exit gracefully
            actionButton(ns("init_download"), ".zip", icon = icon("download")),
            downloadButton(ns("download"), label = NULL, style = "visibility: hidden;"),
            br()
          )),
        ########## ---------- Write files ---------- ##########
        br(),
        actionLink("write_link", label =  "Write files") %>%
          bs_attach_collapse("write"),
        bs_collapse(
          id = "write", 
          content = tagList(
            br(),
            fileInput(ns("file"), label = "Write to a selected checkbox") %>%
              info_tooltip("Click 'Browse...' to search for a file. 
                           Select only one file and checkbox at a time. 
                           If a file already exists, it will be replaced."),
            actionButton(ns("write"), label = "write", icon = icon("upload"))
          )
        ),
        ########## ---------- Delete files ---------- ##########
        br(),
        actionLink("delete_link", label =  "Delete files") %>%
          bs_attach_collapse("delete"),
        bs_collapse(
          id = "delete", 
          content = tagList(
            br(),
            label_container("Delete from selected checkboxes") %>%
              info_tooltip("Select multiple checkboxes in table to delete files from database. 
                           There is no undo!"),
            actionButton(ns("delete"), "Delete", icon = icon("trash"))
          )
        ),
      ########## ---------- Add column ---------- ##########
      br(),
      actionLink("blob_link", label =  "Add column") %>%
        bs_attach_collapse("blob"),
      bs_collapse(
        id = "blob", 
        content = tagList(
          br(),
          textInput(ns("blob_column_name"), label = "Column name", 
                    placeholder = "NameExample") %>%
            info_tooltip("New column name must not have spaces. There is no undo!"),
          fix_ui("column name should not have spaces", 
                 condition = "check_column_name", 
                 ns = ns),
          actionButton(ns("add_blob"), label = "Add (no undo)")
        )
      )),
      mainPanel(
        # actionButton(ns("refresh"), label = "Refresh", icon = icon("refresh")),
        # br(),
        DT::DTOutput(ns("table")),
        tags$script(HTML('$(document).on("click", "input", function () {
                       var checkboxes = document.getElementsByName("selected");
                       var checkboxesChecked = [];
                       for (var i=0; i<checkboxes.length; i++) {
                       if (checkboxes[i].checked) {
                       checkboxesChecked.push(checkboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("checked_rows",checkboxesChecked);  })'))
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
                choices = table_names(pool), selected = "Table2")
  })
  
  table <- reactiveValues(table = NULL)
  observe({print(input$table_cells_selected)})
  
  data_table <- reactive({
    req(input$table_name)
    flob_datatable(input$table_name, pool, ns = ns)
  })
  
  checked <- reactive({
    checked_ids(input = input)
  })
  
  # observe({print(input[["_checkrows_flob_1"]])})

  output$table <- DT::renderDT({data_table()})
  
  rv <- reactiveValues(table = table_read(table_name = table_names(pool)[1],
                                             conn = pool))
  
  # observe(print(rv$table))
  # observeEvent(input$refresh, {
  #   data_table()
  # })
  
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
  
  observeEvent(input$write, {
    path <- input$file$datapath
    id <- checked()
    if(isTRUE(write_modal(id, path))){
      return({
        send_flob(path, id, input$table_name, pool$fetch())
        rv$table <- table_read(input$table_name, pool)
        reset('file')
      })
    }
    showModal(write_modal(id, path))
  })
  
  observeEvent(input$delete, {
    id <- checked()
    if(isTRUE(delete_modal(id))){
      return({
        delete_flobs(id, input$table_name, pool$fetch())
        rv$table <- table_read(input$table_name, pool)
      })
    }
    showModal(delete_modal(id))
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
}
 
