#' Run the Shiny Application
#'
#' @param conn A [SQLiteConnection-class] to a database or 'demo' to open with slobr demo database.
#' @export
run_slobr <- function(conn = "demo") {
  
  chk::chkor(check_sqlite_connection(conn),
          chk::check_values(conn, c("demo", "demo", "demo")))
  
  if(is.character(conn)){
    if(conn == "demo")
      conn <- db_connect(system.file("extdata", "demo_db.sqlite", package = "slobr"))
  }
 
  shinyOptions(conn = conn)
  
  shiny::runApp(system.file("app", package = "slobr"), launch.browser = TRUE)
}
