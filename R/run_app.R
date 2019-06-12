#' Run the Shiny Application
#'
#' @param path A path to SQLite database. Use "demo" to load demo database.
#' @export
#' @importFrom shiny runApp
run_app <- function(path = NULL) {
  
  checkor(check_string(path), check_null(path))
  
  if(isTRUE(path == "demo"))
    path <- system.file("extdata", "demo_db.sqlite", package = "slobr")

  shinyOptions(path = path)
  
  shiny::runApp(system.file("app", package = "slobr"), launch.browser = TRUE)
}
