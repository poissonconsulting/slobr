#' @title   mod_about_ui and mod_about_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about
#'
#' @keywords internal
mod_about_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(1,
           HTML("")),
    column(11,
           h4(paste("Welcome!")),
           br(),
           h5("This app is part of a suite of R packages to read and write files to SQLite database. For more information, see the ",
              a("flobr,", href = "https://github.com/poissonconsulting/flobr"),
              a("dbflobr", href = "https://github.com/poissonconsulting/dbflobr"),
              "and",
              a("slobr", href = "https://github.com/poissonconsulting/slobr"),
              "GitHub pages."),
           actionLink(ns('info_citation'), "Citation info"),
           shinyjs::hidden(div(id = ns("div_citation"),
                               h6(HTML(paste("To cite package 'slobr' in publications use:<br><br>
                                    Seb Dalgarno (2020). slobr: An app to read and write files to SQLite databases.<br><br>
                                    "
                               ))))),
           hr(),
           h6('Developed by Poisson Consulting.')),
    tags$footer(actionLink(inputId = 'poisson',
                           label = img(src = 'https://www.poissonconsulting.ca/assets/logos/poisson.png',
                                       height = 177/5,
                                       width = 739/5,
                                       onclick = "window.open('http://www.poissonconsulting.ca', '_blank')")),
                align = "center",
                style = "position: relative;
                bottom:1;
                width:100%;
                height:50px; /* Height of the footer */
                color: #2f4f4f;
                padding: 10px;
                background-color: white;
                z-index: -1000;
                font-size: 12px"))
}

# Module Server

#' @rdname mod_about
#' @keywords internal

mod_about_server <- function(input, output, session){
  ns <- session$ns
  observeEvent(input$info_citation, {
    shinyjs::toggle("div_citation", anim = TRUE, animType = "slide", time = 0.2)
  })
}


