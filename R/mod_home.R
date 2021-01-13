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
  tagList(tags$style(HTML('h3, h4, h5, h6 {color: black;}'), HTML('h2 {color: #bc3c4b;}')),
          
   h2("Aceasta aplicatie este destinata doar Directiei risc in vederea gestiunii litigiilor 
             care genereaza provizioanele aferente"),
   hr(),
    
   h3("Fluxul de lucru al aplicatiei este urmatorul:"), br(),
 
  h4(tags$div(
    tags$ul(
      tags$li("Se pleaca de la un sold inghetat al litigiilor. Acesta se gaseste ", 
              actionLink(inputId = ns("access_sold_inghetat"),label = "aici")), br(),
      tags$li("Numarul unui litigiu se actualizeaza de ", actionLink(inputId = ns("access_actualizare_numar_litigiu"),
                                        label = "aici")), br(),
      tags$li("Litigiile noi care sunt gasite pe portal.just.ro se actualizeaza de ",
              actionLink(inputId = ns("access_litigii_noi_automate"), label = "aici")), br(),
      tags$li("Sentintele litigiilor in derulare se actualizeaza de",
          actionLink(inputId = ns("access_actualizare_sentinte"), label = "aici")), br(),
      
      tags$li("Litigiile platite se actualizeaza de ",
              actionLink(inputId = ns("access_actualizare_litigii_platite"), label = "aici")), br(),
      
      
      tags$li("Litigiile noi care nu sunt gasite pe portal.just.ro se introduc de ",
              actionLink(inputId = ns("access_actualizare_litigii_noi_manuale"), label = "aici")), hr(),
      
      tags$li("Litigiile la zi sunt disponibile ",
              actionLink(inputId = ns("access_litigii_la_zi"), label = "aici"))
    )
  ))
  
  )
}
    
#' home Server Function
#'
#' @noRd 
mod_home_server <- function(input, output, session, parrent){
    ns <- session$ns
 
    observeEvent(input$access_sold_inghetat,{
      
      shinydashboard::updateTabItems(session = parrent,inputId = "sidebar_ui_1-tabs",
                                    selected = "litigii_inghetate")
    })
    
    observeEvent(input$access_actualizare_numar_litigiu,{
      
      shinydashboard::updateTabItems(session = parrent,inputId = "sidebar_ui_1-tabs",
                                     selected = "actualizare_automata")
    })
    
    observeEvent(input$access_actualizare_sentinte,{
      
      shinydashboard::updateTabItems(session = parrent,inputId = "sidebar_ui_1-tabs",
                                     selected = "actualizare_automata")
    })
    
    observeEvent(input$access_litigii_noi_automate,{
      
      shinydashboard::updateTabItems(session = parrent,inputId = "sidebar_ui_1-tabs",
                                     selected = "actualizare_automata")
    })
    
    observeEvent(input$access_actualizare_litigii_platite,{
      
      shinydashboard::updateTabItems(session = parrent,inputId = "sidebar_ui_1-tabs",
                                     selected = "actualizare_automata")
    })
    
    observeEvent(input$access_actualizare_litigii_noi_manuale,{
      
      shinydashboard::updateTabItems(session = parrent,inputId = "sidebar_ui_1-tabs",
                                     selected = "litigii_noi")
    })
    
    observeEvent(input$access_litigii_la_zi,{
      
      shinydashboard::updateTabItems(session = parrent,inputId = "sidebar_ui_1-tabs",
                                     selected = "litigii_current")
    })
    
    
 
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# callModule(mod_home_server, "home_ui_1")
 
