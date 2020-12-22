#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(tags$style(HTML(' h1, h2, h3, h4, h5, h6 {color: black;}')),
   h2("Aceasta aplicatie este destinata doar Directiei risc in vederea gestiunii litigiilor 
             care genereaza provizioanele aferente"),
  h3("Fluxul de lucru al aplicatiei este urmatorul:"),
  
  h4(tags$div(
    tags$ul(
      tags$li("Se pleaca de la un sold inghetat al litigiilor. Acesta se gaseste ", a("aici")),
      tags$li("Sentintele se updateaza automat")
    )
  ))
  
  )
}
    
#' home Server Function
#'
#' @noRd 
mod_home_server <- function(input, output, session){
    ns <- session$ns
 
 
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# callModule(mod_home_server, "home_ui_1")
 
