#' @import shiny
app_ui <- function() {
  tagList(
    # shinyutils::css_body(),
    shinyutils::css_navbar(),
    use_bs_tooltip(),
    use_bs_popover(),
    tags$head(tags$script(HTML('
                           Shiny.addCustomMessageHandler("jsCode",
                           function(message) {
                           eval(message.value);
                           });'
    ))),
    # shinyutils::css_hide_errors(),
    navbarPage(title = "slobr", 
               selected = "Read and Write Files", 
               tabPanel(title = "Read and Write Files",
                        mod_readwrite_ui("readwrite_ui_1")),
               tabPanel(title = "About",
                        shinyutils::mod_about_ui("about_ui_1", citation = tagList(p(""))))
               )
  )
}
