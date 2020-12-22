#' litigii UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#  Handles litigii la zi sidebar menu

mod_litigii_ui <- function(id){
  ns <- NS(id)
  tagList(#tags$style('.nav-tabs-custom .nav-tabs li.active {border-top-color: #3cbcad;}'),
  
  shinydashboard::box(title = "Litigii a caror solutie s-a actualizat",width = 12, status = "success",
                          collapsible = T, collapsed = T, 
            downloadButton(outputId = ns("down_update_sentinte"),label = "Download sentinte actualizate"),br(),
                      
            DT::dataTableOutput(outputId = ns("litigii_update_sentinte"))),
    
  shinydashboard::box(width = 12, title = "Litigii platite",status = "success", collapsible = TRUE, collapsed = T,
                      downloadButton(outputId = ns("down_litigii_platite"),label = "Download litigii platite"),br(),
                      
                      DT::dataTableOutput(outputId = ns("litigii_update_plati"))),
  
  shinydashboard::box(width = 12, title = "Litigii noi inlocuiesc litigii existente",status = "success", 
                      collapsible = TRUE, collapsed = FALSE,
                      DT::dataTableOutput(outputId = ns("litigii_noi_updated"))),
  )
}
    
#' litigii Server Function
#'
#' @noRd 
mod_litigii_server <- function(input, output, session, vals){
  ns <- session$ns
  
  output$down_update_sentinte <- downloadHandler(filename = function() {"sentinte_updatate.csv"},
        content = function(file) {readr::write_csv(vals$litigii_update %>% 
                          dplyr::filter(updated_by != "update_litigiu"),path = file) })
 
  output$down_litigii_platite <- downloadHandler(filename = function() {"litigii_platite.csv"},
              content = function(file) {readr::write_csv(x = vals$litigii_update %>% 
                                dplyr::filter(updated_by == "automated_contract_platit"),path = file) })
  
  observeEvent(vals$litigii_update,{     
    output$litigii_update_sentinte <-  DT::renderDataTable({ DT::datatable( rownames = FALSE, class = "nowrap",
        options = list(scrollX = TRUE, dom = "ftp", pageLength = 5),
        data = vals$litigii_update %>% dplyr::filter(updated_by != "update_litigiu") %>%
          dplyr::select(-Nr_crt,-data_plata) %>% dplyr::filter(!is.na(update_sentinta)) %>%
          dplyr::select(1, 2, 6, 7, 9, 12, 11, 14,15, 13, 10, 3:5, 8),
        caption = "Litigii cu stadiu modificat dupa 30 septembrie 2020:") })
  
    output$litigii_update_plati <- DT::renderDataTable({ DT::datatable( rownames = FALSE, class = "nowrap",
        options = list(scrollX = TRUE, dom = "ftp", pageLength = 5),
        data = vals$litigii_update %>% dplyr::filter(updated_by == "automated_contract_platit") %>%
          dplyr::select(-Nr_crt,-data_actualizare_sentinta) %>% dplyr::select(1, 2, 6, 7, 9:17,3:5, 8),
        caption = "Litigii platite:") })
    
    output$litigii_noi_updated <- DT::renderDataTable({ DT::datatable(rownames = FALSE,
          class = "nowrap",  options = list(scrollX = TRUE,  dom = "ftp", pageLength = 5),
          data = vals$litigii_update %>% dplyr::filter(updated_by == "update_litigiu") %>%
            dplyr::select(-Nr_crt,-data_plata),
          caption = "Litigii noi care au actualizat litigii existente:" ) })
    
  })
    
}
    
## To be copied in the UI
# mod_litigii_ui("litigii_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_server, "litigii_ui_1")
 
