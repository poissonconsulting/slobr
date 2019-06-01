#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(mod_readwrite_server, "readwrite_ui_1")
  callModule(shinyutils::mod_about_server, "about_ui_1")
  
}
