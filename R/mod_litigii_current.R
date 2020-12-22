#' litigii_current UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_litigii_current_ui <- function(id){
  ns <- NS(id)
  tagList(tags$style(HTML("#litigii_current_ui_1-titlu_litigii_current {color: #bc3c4b; position: pull-right; font-size: 20px}")),
    fluidRow(
  column(width = 4,br(),downloadButton(outputId = ns('down_litigii_current'),label = "Download csv litigii curente")),
  column(width = 4, br(),div(class = "pull-right",textOutput(outputId = ns("titlu_litigii_current")))),
  column(width = 4, div(class = "pull-right",
      dateInput(inputId = ns("data_litigii_current"),value = Sys.Date(),label = "Schimba data raportului",width = "190px")))),
  hr(),
  DT::dataTableOutput(ns("litigii_current")), hr(),
  DT::dataTableOutput(ns("sinteza_litigii_curent")),
  verbatimTextOutput(ns("diverse"))
  )
}
    
#' litigii_current Server Function
#'
#' @noRd 
mod_litigii_current_server <- function(input, output, session, vals){
  ns <- session$ns
  vals_current <- reactiveValues()
  load("data/litigii_sep.rda")
  
  observeEvent(vals$litigii_update,{req(input$data_litigii_current)
  
    vals_current$plati_update <- vals$litigii_update %>% dplyr::filter(updated_by == "automated_contract_platit") %>%
      dplyr::filter(data_plata <= input$data_litigii_current)
    
    vals_current$sentinte_update <- vals$litigii_update %>% dplyr::filter(updated_by != "update_litigiu") %>%
        dplyr::filter(!is.na(data_actualizare_sentinta) & data_actualizare_sentinta <=  input$data_litigii_current) %>%
          dplyr::filter(!Nr_crt %in% vals_current$plati_update$Nr_crt)
    
    vals_current$litigii_noi_manual <- vals$litigii_update %>% dplyr::filter(updated_by == "litigiu_nou_manual") %>%
      dplyr::filter(data_actualizare_sentinta <=  input$data_litigii_current)
      
    vals_current$litigii_noi_automated <- vals$litigii_update %>% dplyr::filter(updated_by == "litigiu_nou_automated") %>%
      dplyr::filter(data_actualizare_sentinta <=  input$data_litigii_current)
        
    vals_current$litigii_noi_updated <-  vals$litigii_update %>% dplyr::filter(updated_by == "update_litigiu")   
    
    vals_current$litigii_final_update <- dplyr::bind_rows(vals_current$litigii_noi_automated,vals_current$litigii_noi_manual ,
              vals_current$plati_update,vals_current$sentinte_update, vals_current$litigii_noi_updated)
    
    vals_current$litigii_current <- dplyr::bind_rows(vals_current$litigii_final_update,
                      litigii_sep %>% dplyr::filter(!Nr_crt %in% vals_current$litigii_final_update$Nr_crt))
    
    vals_current$litigii_curent_prelucrat <- vals_current$litigii_current %>% 
      dplyr::mutate(`Stadiul procesului` = ifelse(is.na(`Stadiul procesului`),update_sentinta,
                      paste(`Stadiul procesului`,update_sentinta,sep = "\n"))) %>%
      dplyr::mutate(Necesar_provizion_curent = ifelse(is.na(update_coef_proviz),`Necesar provizion septembrie 2020`,
                      update_coef_proviz * `Suma litigiu - echiv lei`)) %>%
      dplyr::mutate(Regularizare_provizion = Necesar_provizion_curent - ifelse(is.na(`Necesar provizion septembrie 2020`),0,
        `Necesar provizion septembrie 2020`)) %>%
      
      dplyr::select(`Nr dosar instanta`, updated_by, `Beneficiar_Juridic`, `Nr contract`,`Necesar provizion septembrie 2020`,
                    Necesar_provizion_curent, Regularizare_provizion,`Suma litigiu - echiv lei`,
                    `Coeficient provizionare`,update_coef_proviz, `Suma litigiu - echiv lei`, Tip_litigiu,
                    data_actualizare_sentinta,  `Stadiul procesului`,Nr_crt)
 

    
    output$litigii_current <- DT::renderDataTable({DT::datatable(rownames = FALSE, 
              options = list(dom = "ftp", scrollX = TRUE,pageLength = 5),class = "nowrap",
                              data = vals_current$litigii_curent_prelucrat) %>% 
        DT::formatRound(columns = 5:8,digits = 0)})
    
    output$titlu_litigii_current <- renderText({paste0("Situatia litigiilor la data de ", input$data_litigii_current)})
    
    output$sinteza_litigii_curent <- DT::renderDataTable({ DT::datatable(rownames = FALSE, class = "compact",
      extensions = "Buttons", options = list(dom = "Bt",buttons = c("excel")),
      caption = "Sinteza regularizare provizioane curente:",
      data = vals_current$litigii_curent_prelucrat %>% dplyr::group_by(Tip_litigiu) %>% 
        dplyr::summarise(dplyr::across(.cols = c(`Suma litigiu - echiv lei`,Regularizare_provizion,
                Necesar_provizion_curent,`Necesar provizion septembrie 2020`),.fns = ~sum(.x,na.rm = TRUE))) %>%
                 janitor::adorn_totals(where = "row",na.rm = TRUE)) %>%
        DT::formatRound(columns = 2:5,digits=0)})
    
   # output$diverse <- renderPrint({str( vals_current$litigii_curent_prelucrat)})
  
  })
  
  output$down_litigii_current <- downloadHandler(filename = function(){"litigii_zi.csv"}, 
      content = function(file) { readr::write_csv(x = vals_current$litigii_curent_prelucrat,path = file)})
 
}
    
## To be copied in the UI
# mod_litigii_current_ui("litigii_current_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_current_server, "litigii_current_ui_1")
 
