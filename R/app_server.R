app_server <- function(input, output,session) {
  # List the first level callModules here
  shinyhelper::observe_helpers(help_dir = system.file("helpers", package = "slobr"))
  callModule(mod_readwrite_server, "readwrite_ui_1")
  callModule(mod_about_server, "about_ui_1")
  
}
