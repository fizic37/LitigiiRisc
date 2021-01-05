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
  shinydashboard::tabBox(title = "",width=NULL,
                         tabPanel(title = "Litigii la zi",
            tags$style(HTML('.nav-tabs-custom>.nav-tabs>li.active {border-top-color: #3cbcad;}')),
  fluidRow(
  shinydashboard::box(width = 12,collapsible = FALSE, title = NULL, status = "success",
                        tags$style(HTML("#litigii_current_ui_1-titlu_litigii_current {color: #bc3c4b; position: pull-right; font-size: 20px;}
                          .box-footer {color: black;}")),              
    footer = "litigiu_nou_automated - litigii noi generate prin interogare
        automata; automated - litigiu cu sentinta actualizata; automated_contract_platit - litigiu platit prelucrare automata;
    update_litigiu - litigiu al carui numar s-a actualizat (este posibil sa se fi actualizat si sentinta); neactualizat - litigiu 
    neactualizat fata de sep 2020",
          fluidRow(
  #column(width = 4,br(),downloadButton(outputId = ns('down_litigii_current'),label = "Download csv litigii curente")),
  column(width = 6, br(),div(class = "pull-left",textOutput(outputId = ns("titlu_litigii_current")))),
  column(width = 6, div(class = "pull-right",
      dateInput(inputId = ns("data_litigii_current"),value = as.Date("2020-12-31"),label = "Schimba data raportului",width = "190px")))),
  hr(),
  radioButtons(inputId = ns("filter_litigii_curente"),label = "Filter for updated_by",inline = TRUE,
               choices = c(1,2)),
  downloadButton(outputId = ns("down_baza_filtrata"),label = "Download litigii curente filtrate"),  
  DT::dataTableOutput(ns("litigii_current")),
  )) ) ,
  tabPanel(title = "Sinteza litigii curente",
  DT::dataTableOutput(ns("sinteza_litigii_curent")) )  )  
}
    
#' litigii_current Server Function
#'
#' @noRd 
mod_litigii_current_server <- function(input, output, session, vals){
  ns <- session$ns
  
  vals_current <- reactiveValues()
  
  observeEvent(vals$litigii_curente,{req(input$data_litigii_current)
  
    vals_current$plati_update <- vals$litigii_curente %>% dplyr::filter(updated_by == "automated_contract_platit") %>%
      dplyr::filter(data_plata <= input$data_litigii_current)
    
    vals_current$sentinte_update <- vals$litigii_curente %>% dplyr::filter(updated_by != "update_litigiu") %>%
        dplyr::filter(!is.na(data_actualizare_sentinta) & data_actualizare_sentinta <=  input$data_litigii_current) %>%
          dplyr::filter(!Nr_crt %in% vals_current$plati_update$Nr_crt)
    
    vals_current$litigii_noi_manual <- vals$litigii_curente %>% dplyr::filter(updated_by == "litigiu_nou_manual") %>%
      dplyr::filter(data_actualizare_sentinta <=  input$data_litigii_current)
      
    vals_current$litigii_noi_automated <- vals$litigii_curente %>% dplyr::filter(updated_by == "litigiu_nou_automated") %>%
      dplyr::filter(data_actualizare_sentinta <=  input$data_litigii_current)
        
    vals_current$litigii_noi_updated <-  vals$litigii_curente %>% dplyr::filter(updated_by == "update_litigiu")   
    
    vals_current$litigii_final_update <- dplyr::bind_rows(vals_current$litigii_noi_automated,vals_current$litigii_noi_manual ,
              vals_current$plati_update,vals_current$sentinte_update, vals_current$litigii_noi_updated)
    
    vals_current$litigii_current <- dplyr::bind_rows(vals_current$litigii_final_update,
                      vals$litigii_curente %>% dplyr::filter(!Nr_crt %in% vals_current$litigii_final_update$Nr_crt))
    
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
    
    output$titlu_litigii_current <- renderText({paste0("Situatia litigiilor la data de ", input$data_litigii_current)})
    
    updateRadioButtons(session = session, inputId = "filter_litigii_curente",inline = TRUE,
                       choices = c("all", unique(vals_current$litigii_curent_prelucrat$updated_by[
                         !is.na(vals_current$litigii_curent_prelucrat$updated_by)]),"neactualizat"),selected = "all")
    
    output$down_litigii_current <- downloadHandler(filename = function(){"litigii_zi.csv"}, 
             content = function(file) { readr::write_csv(x = vals_current$litigii_curent_prelucrat,path = file)})
  })
  
  observeEvent(input$filter_litigii_curente,{
 
    output$litigii_current <- DT::renderDataTable({
      DT::datatable(rownames = FALSE, 
          class = "nowrap",
          options = list(dom = "ftp", scrollX = TRUE,pageLength = 5),
          data = if (input$filter_litigii_curente == "all") {vals_current$litigii_curent_prelucrat}
          
          else if (input$filter_litigii_curente == "neactualizat") {vals_current$litigii_curent_prelucrat %>% 
              dplyr::filter(is.na(updated_by))}
          else {vals_current$litigii_curent_prelucrat %>% 
              dplyr::filter(updated_by == input$filter_litigii_curente) }) %>% 
        DT::formatRound(columns = 5:8,digits = 0)   })
    
   
    
    
    
    output$sinteza_litigii_curent <- DT::renderDataTable({  DT::datatable(  rownames = FALSE,
          class = "compact",  extensions = "Buttons",  options = list(dom = "Bt", buttons = c("excel")),
          caption = paste0("Sinteza regularizare provizioane curente la data de ",
            input$data_litigii_current,   "_filter_",  input$filter_litigii_curente),
          data = if (input$filter_litigii_curente == "all") {
            vals_current$litigii_curent_prelucrat %>% dplyr::group_by(Tip_litigiu) %>%
              dplyr::summarise(dplyr::across(.cols = c(`Suma litigiu - echiv lei`,
                  Regularizare_provizion,   Necesar_provizion_curent,`Necesar provizion septembrie 2020`),
                .fns = ~ sum(.x, na.rm = TRUE))) %>%
              janitor::adorn_totals(where = "row", na.rm = TRUE)     }
          else if (input$filter_litigii_curente == "neactualizat") {
            vals_current$litigii_curent_prelucrat %>%
              dplyr::filter(is.na(updated_by)) %>% dplyr::group_by(Tip_litigiu) %>%
              dplyr::summarise(dplyr::across( .cols = c(`Suma litigiu - echiv lei`,
                  Regularizare_provizion,  Necesar_provizion_curent,`Necesar provizion septembrie 2020`),
                .fns = ~ sum(.x, na.rm = TRUE))) %>%
              janitor::adorn_totals(where = "row", na.rm = TRUE)   }
          else {
            vals_current$litigii_curent_prelucrat %>%
              dplyr::filter(updated_by == input$filter_litigii_curente) %>% dplyr::group_by(Tip_litigiu) %>%
              dplyr::summarise(dplyr::across( .cols = c(`Suma litigiu - echiv lei`,
                  Regularizare_provizion, Necesar_provizion_curent, `Necesar provizion septembrie 2020`),
                .fns = ~ sum(.x, na.rm = TRUE))) %>%
              janitor::adorn_totals(where = "row", na.rm = TRUE)   }
    ) %>% DT::formatRound(columns = 2:5, digits = 0)  })
    
    output$down_baza_filtrata <- downloadHandler(filename = function(){paste0("Litigii_curente_",input$filter_litigii_curente,".csv")}, 
                                                 content = function(file) { readr::write_csv(path = file,
                                                                                             x = if (input$filter_litigii_curente == "all") {vals_current$litigii_curent_prelucrat}
                                                                                             
                                                                                             else if (input$filter_litigii_curente == "neactualizat") {vals_current$litigii_curent_prelucrat %>% 
                                                                                                 dplyr::filter(is.na(updated_by))}
                                                                                             else {vals_current$litigii_curent_prelucrat %>% 
                                                                                                 dplyr::filter(updated_by == input$filter_litigii_curente) })})
    
  })
      
    
      
  
 
  
  
 
}
    
## To be copied in the UI
# mod_litigii_current_ui("litigii_current_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_current_server, "litigii_current_ui_1")
 
