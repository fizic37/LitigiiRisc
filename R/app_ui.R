#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      shinydashboard::dashboardPage(skin = "blue",
       
        header = shinydashboardPlus::dashboardHeaderPlus(title = "Litigii Risc"),
          sidebar = shinydashboard::dashboardSidebar(tags$style(HTML('.skin-blue .sidebar-menu>li>.treeview-menu {
    background: #47d9c8;} .skin-blue .treeview-menu>li>a {color: #bc3c4b;}')),
            mod_sidebar_ui("sidebar_ui_1"),collapsed = FALSE),
        body = shinydashboard::dashboardBody(shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "litigii", mod_litigii_ui("litigii_ui_1")),
          shinydashboard::tabItem(tabName = "home", mod_home_ui("home_ui_1")),
          shinydashboard::tabItem(tabName = "litigii_current", mod_litigii_current_ui("litigii_current_ui_1")),
          shinydashboard::tabItem(tabName = "litigii_inghetate", mod_litigii_inghetate_ui("litigii_inghetate_ui_1")),
          shinydashboard::tabItem(tabName = "actualizare_manuala", mod_actualizare_manuala_ui("actualizare_manuala_ui_1")),
          shinydashboard::tabItem(tabName = "actualizare_automata", 
                              mod_actualizare_automata_ui("actualizare_automata_ui_1")),
          shinydashboard::tabItem(tabName = "litigii_noi", 
                                  mod_litigii_noi_ui("litigii_noi_ui_1")))))
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'LitigiiRisc'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

