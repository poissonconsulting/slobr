#' Run the Shiny Application
#'
#' @param path A path to SQLite database.
#' @export
#' @importFrom shiny runApp
run_app <- function(path = system.file("extdata", "demo_db.sqlite", package = "slobr")) {
  
  check_string(path)

  pool <- pool_open(path)
  shinyOptions(pool = pool)
  
  # close the pool onExit
  pool_close(pool)
  shiny::runApp(system.file("app", package = "slobr"))
}
