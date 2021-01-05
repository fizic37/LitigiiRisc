#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  sidebar_selected <- c()
  load("data/litigii_update.rda")
  load("data/litigii_curente.rda")
  
  vals <- reactiveValues(sidebar_selected = sidebar_selected, litigii_update = litigii_update,
                         litigii_curente = litigii_curente)
  
  callModule(mod_sidebar_server, "sidebar_ui_1",vals)
  
  observeEvent(vals$sidebar_selected,{
    
    if (sum("litigii" == vals$sidebar_selected)==1) {
      callModule(mod_litigii_server, "litigii_ui_1", vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"litigii")
    }
    
    if (sum("litigii_current" == vals$sidebar_selected)==1) {
      
      callModule(mod_litigii_current_server, "litigii_current_ui_1", vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"litigii_current")
    }
    
    if (sum("litigii_inghetate" == vals$sidebar_selected)==1) {
      callModule(mod_litigii_inghetate_server, "litigii_inghetate_ui_1")
      vals$sidebar_selected <- c(vals$sidebar_selected,"litigii_inghetate")
    }
    
    
    if (sum("actualizare_automata" == vals$sidebar_selected)==1) {
      callModule(mod_actualizare_automata_server, "actualizare_automata_ui_1", vals)
      callModule(mod_litigii_platite_server, "litigii_platite_ui_1",vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"actualizare_automata")
    }
    
    if (sum("litigii_noi" == vals$sidebar_selected)==1) {
      callModule(mod_litigii_noi_server, "litigii_noi_ui_1", vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"litigii_noi")
    }
    
    
    if (sum("actualizare_manuala" == vals$sidebar_selected)==1) {
      callModule(mod_actualizare_manuala_server, "actualizare_manuala_ui_1")
      vals$sidebar_selected <- c(vals$sidebar_selected,"actualizare_manuala")
    }
    
    if (sum("home" == vals$sidebar_selected)==1) {
      callModule(mod_home_server, "home_ui_1")
      vals$sidebar_selected <- c(vals$sidebar_selected,"home")
    }
  })
}
