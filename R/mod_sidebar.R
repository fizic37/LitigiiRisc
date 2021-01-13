#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  
 shinydashboard::sidebarMenuOutput(outputId = ns("sidebar"))

 }
    
#' sidebar Server Function
#'
#' @noRd 
mod_sidebar_server <- function(input, output, session,vals){
  ns <- session$ns
 
  risk_user_sidebar <- shinydashboard::sidebarMenu(id = ns("tabs"),
                            shinydashboard::menuItem(tabName = "home",text = "Home",icon = icon("home"),selected = F),
                            shinydashboard::menuItem(tabName = "litigii",  
                                text = "Litigii actualizate",icon=icon("balance-scale"),selected = F,
                                shinydashboard::menuSubItem(text = "Litigii la zi ",tabName = "litigii_current",selected = T,
                                                          icon = icon("calendar-alt")),
                                #shinydashboard::menuSubItem(text = "Litigiile actualizate",icon = icon("pen-fancy"),
                                    #                        tabName = "litigii",selected = FALSE),
                                shinydashboard::menuSubItem(text = "Ultimul sold inghetat ",icon = icon("camera"),
                                                            tabName = "litigii_inghetate",selected = FALSE)),
                            shinydashboard::menuItem(tabName = "actualizare_automata",  
                                  text = "Actualizare automata",icon=icon("laptop-code"),selected = F),
                            shinydashboard::menuItem(tabName = "litigii_noi",  
                                                     text = "Litigii noi manuale",icon=icon("highlighter"),selected = F)
                            #,shinydashboard::menuItem(tabName = "actualizare_manuala",  
                             #     text = "Actualizare manuala",icon = icon("highlighter"),selected = F)
                            )
                            
  
  guest_user_sidebar <- shinydashboard::sidebarMenu(id = ns("tabs"),
                          shinydashboard::menuItem(tabName = "home",text = "Home"),
                          shinydashboard::menuItem(tabName = "plati",  text = "Provizioane plati",selected = FALSE),
                          shinydashboard::menuItem(tabName = "portofoliu", text = "Portofoliu",selected = FALSE))
  
  
  
  output$sidebar <- shinydashboard::renderMenu({
    
  risk_user_sidebar
    
  })
  
  observeEvent(input$tabs,{ 
        # I use this in order to have a selection of all inputs in sidebar. This way, I don`t have to call modules
    # every time a sidebar is selected, I only call modules ones.`
    vals$sidebar_selected <- c(vals$sidebar_selected,input$tabs)
    
    })
  
 
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# callModule(mod_sidebar_server, "sidebar_ui_1")
 
