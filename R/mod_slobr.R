# Module UI
  
#' @title   mod_slobr_ui and mod_slobr_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_slobr
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_slobr_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_slobr
#' @export
#' @keywords internal
    
mod_slobr_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_slobr_ui("slobr_ui_1")
    
## To be copied in the server
# callModule(mod_slobr_server, "slobr_ui_1")
 
