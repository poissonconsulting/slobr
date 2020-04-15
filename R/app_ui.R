app_ui <- function() {
  tagList(
    css_navbar(text_selected_color = "#5bc0de"),
    css_hide_errors(),
    use_bs_tooltip(),
    tags$head(tags$script(HTML('
                           Shiny.addCustomMessageHandler("jsCode",
                           function(message) {
                           eval(message.value);
                           });'
    ))),
    navbarPage(title = "slobr", 
               selected = "Read and Write Files", 
               tabPanel(title = "Read and Write Files",
                        mod_readwrite_ui("readwrite_ui_1")),
               tabPanel(title = "About",
                        mod_about_ui("about_ui_1"))
               )
  )
}
