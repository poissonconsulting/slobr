#' @import shiny
app_ui <- function() {
  tagList(
    shinyutils::css_navbar(),
    navbarPage(title =  "Slobr", selected = 'Slob',
               tabPanel(title = "Slob",
                        mod_slobr_ui("ui_slobr_1")),
               tabPanel(title = 'About',
                        shinyutils::mod_about_ui("ui_about_1")))
  )
}
