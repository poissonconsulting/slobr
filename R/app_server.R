#' @import shiny
app_server <- function(input, output,session) {
  callModule(shinyutils::mod_about_server, "about_ui_1")
  callModule(mod_slobr_server, "slobr_ui_1")
}
