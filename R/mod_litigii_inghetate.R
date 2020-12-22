#' litigii_inghetate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_litigii_inghetate_ui <- function(id){
  ns <- NS(id)
  tagList(
      DT::dataTableOutput(ns("sinteza_sold_litigii")), hr(),
      
      downloadButton(ns("down_litigii_inghetate"),label = "Download litigii inghetate"),br(),
  
      DT::dataTableOutput(ns("sold_litigii_inghetat"))
  )
}
    
#' litigii_inghetate Server Function
#'
#' @noRd 
mod_litigii_inghetate_server <- function(input, output, session){
  ns <- session$ns
  load('data/litigii_sep.rda')
  
  output$sinteza_sold_litigii <- DT::renderDataTable({ DT::datatable(class = "compact",
                  caption = "Sinteza sold litigii 30 septembrie 2020:",extensions = "Buttons",
                  options = list(dom = "Bt", buttons = c("excel", "csv")),  rownames = FALSE,
    data = litigii_sep %>% dplyr::group_by(Tip_litigiu) %>% dplyr::summarise(Suma_litigiu = sum(`Suma litigiu - echiv lei`),
            Provizion_septembrie = sum(`Necesar provizion septembrie 2020`),
            Numar_litigii = dplyr::n()) %>% dplyr::arrange(desc(Suma_litigiu)) %>% 
            janitor::adorn_totals()) %>%   
            DT::formatRound(columns = 2:4, digits = 0)  })
  
  output$sold_litigii_inghetat <- DT::renderDataTable({ DT::datatable(class = "nowrap",
     caption = "Sold detaliat la 30 septembrie 2020:",
     options = list(dom = "ftp", scrollX = TRUE, pageLength = 7),  rownames = FALSE,
      data = litigii_sep %>% dplyr::select(-Nr_crt))  })
  
  output$down_litigii_inghetate <- downloadHandler(filename = function() {"litigii_inghetate.csv"},
      content = function(file) {readr::write_csv(x = litigii_sep, path = file)})
  
}
    
## To be copied in the UI
# mod_litigii_inghetate_ui("litigii_inghetate_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_inghetate_server, "litigii_inghetate_ui_1")
 
