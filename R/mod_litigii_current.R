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
  
            column(width = 6, br(),div(class = "pull-left",textOutput(outputId = ns("titlu_litigii_current")))),
  
            column(width = 6, div(class = "pull-right", dateInput(inputId = ns("data_litigii_current"), width = "190px",
                                        value = as.Date("2020-12-31"),label = "Schimba data raportului")))),
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
  
    # produces just plati
    vals_current$plati_update <- vals$litigii_curente %>% dplyr::filter(updated_by == "automated_contract_platit") %>%
      dplyr::filter(data_plata <= input$data_litigii_current)
    
    vals_current$litigii_noi_manual <- vals$litigii_curente %>% dplyr::filter(updated_by == "litigiu_nou_manual") %>%
      dplyr::filter(data_actualizare_sentinta <=  input$data_litigii_current)
    
    vals_current$litigii_noi_automated <- vals$litigii_curente %>% dplyr::filter(updated_by == "litigiu_nou_automated") %>%
      dplyr::filter(data_actualizare_sentinta <=  input$data_litigii_current)
    
    # produces sentinte actualizate. Majoritar acestea sunt in updated_by = automated dar se pot regasi si in update_litigiu
    
    vals_current$sentinte_update <- vals$litigii_curente %>% dplyr::filter(updated_by %in% c("update_litigiu", "automated")) %>%
        dplyr::filter(!is.na(data_actualizare_sentinta) & data_actualizare_sentinta <=  input$data_litigii_current) 
    
   
     # produces doar litigii actualizate dar elimina si litigiile actualizate care au avut sentinta actualizata   
    vals_current$litigii_noi_updated <-  vals$litigii_curente %>% dplyr::filter(updated_by == "update_litigiu") %>%
                  dplyr::filter(!Nr_crt %in% vals_current$sentinte_update$Nr_crt)   
    
    vals_current$litigii_final_update <- dplyr::bind_rows(vals_current$litigii_noi_automated,vals_current$litigii_noi_manual ,
              vals_current$plati_update,vals_current$sentinte_update, vals_current$litigii_noi_updated)
    
    # Litigiile curente sunt formate din cele atualizate si din cele neactualizate, avand in vedere ca vals$litigii_curente
    #        s-a format pornind de la litigiile inghetate
    vals_current$litigii_current <- dplyr::bind_rows(vals_current$litigii_final_update,
                      vals$litigii_curente %>% dplyr::filter(is.na(updated_by)))
                        #dplyr::filter(!Nr_crt %in% vals_current$litigii_final_update$Nr_crt))
    
    vals_current$litigii_curent_prelucrat <- vals_current$litigii_current %>% 
      #dplyr::mutate(`Stadiul procesului` = ifelse(is.na(`Stadiul procesului`),update_sentinta,
          #            paste(`Stadiul procesului`,update_sentinta,sep = "\n"))) %>%
      dplyr::mutate(Necesar_provizion_curent = ifelse(is.na(update_coef_proviz),`Necesar provizion septembrie 2020`,
                      update_coef_proviz * `Suma litigiu - echiv lei`)) %>%
      dplyr::mutate(Regularizare_provizion = Necesar_provizion_curent - ifelse(is.na(`Necesar provizion septembrie 2020`),0,
        `Necesar provizion septembrie 2020`)) %>%
      
      dplyr::select(`Nr dosar instanta`, updated_by, `Beneficiar_Juridic`, `Nr contract`,`Necesar provizion septembrie 2020`,
                    Necesar_provizion_curent, Regularizare_provizion,`Suma litigiu - echiv lei`,
                    `Coeficient provizionare`,update_coef_proviz, `Suma litigiu - echiv lei`, Tip_litigiu,
                    data_actualizare_sentinta,  `Stadiul procesului`,Nr_crt, update_sentinta) %>%
                      dplyr::arrange(updated_by)
    
    output$titlu_litigii_current <- renderText({paste0("Situatia litigiilor la data de ", input$data_litigii_current)})
    
    updateRadioButtons(session = session, inputId = "filter_litigii_curente",inline = TRUE,
                       choices = c("all", unique(vals_current$litigii_curent_prelucrat$updated_by[
                         !is.na(vals_current$litigii_curent_prelucrat$updated_by)]),"neactualizat"),selected = "all")
    
    output$down_litigii_current <- downloadHandler(filename = function(){"litigii_zi.csv"}, 
             content = function(file) { readr::write_csv(x = vals_current$litigii_curent_prelucrat,path = file)})
  })
  
  
  observeEvent(input$filter_litigii_curente,{
  
    if (input$filter_litigii_curente == "all") {
      vals_current$litigii_current_tabel <- vals_current$litigii_curent_prelucrat}
    
    else if (input$filter_litigii_curente == "neactualizat") {
      vals_current$litigii_current_tabel <- vals_current$litigii_curent_prelucrat %>% 
        dplyr::filter(is.na(updated_by))  }
    else {
      vals_current$litigii_current_tabel <- vals_current$litigii_curent_prelucrat %>% 
        dplyr::filter(updated_by == input$filter_litigii_curente) }
    
    output$litigii_current <- DT::renderDataTable({
      DT::datatable(rownames = FALSE, 
        selection = list(mode = "single",selected = NULL, target = "row"),
          class = "nowrap",caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: left;',
            "Click pe litigiu pentru a vizualiza"),
          options = list(dom = "ftp", scrollX = TRUE,pageLength = 5),
          data = vals_current$litigii_current_tabel) %>% 
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
      
    
      
  observeEvent(input$litigii_current_rows_selected,{
    
    vals_current$litigiu_selectat <- vals_current$litigii_current_tabel %>%  dplyr::slice(input$litigii_current_rows_selected)
    
    showModal(modalDialog(title = paste0("View litigiul ",vals_current$litigiu_selectat$`Nr dosar instanta`),
                              size = "l",footer = modalButton(label = "Close"),
              fluidRow(column(width = 6,
                              textInput(inputId = session$ns("beneficiar_litigiu"),label = "Beneficiar litigiu",
                                        value = vals_current$litigiu_selectat$Beneficiar_Juridic),
                              textInput(inputId = session$ns("numar_contract_litigiu"), label = "Numar contract litigiu",
                                        value = vals_current$litigiu_selectat$`Nr contract`),
                              selectInput(inputId = session$ns("coef_proviz_vechi"),label = "Coeficient provizionare vechi",
                                           selected = vals_current$litigiu_selectat$`Coeficient provizionare`,
                                          choices = c(0,0.25,0.65,1)),
                              textInput(session$ns("provizion_vechi"),label = "Provizion contabil anterior",
                                           value = vals_current$litigiu_selectat$`Necesar provizion septembrie 2020` %>%
                                              formatC(digits = 2,big.mark = ",",format = "f",mode = "double")),
                              textAreaInput(session$ns("sentinta_veche"),label = "Sentinta veche",
                                            height = "250px",width = "400px",
                                            value = vals_current$litigiu_selectat$`Stadiul procesului`)),
                       column(width = 6,
                              textInput(inputId = session$ns("updated_by"),label = "Tip update",
                                        value = vals_current$litigiu_selectat$updated_by),
                              textInput(inputId = session$ns("suma_litigiu"),label = "Suma litigiu",
                                        value = vals_current$litigiu_selectat$`Suma litigiu - echiv lei` %>%
                                          formatC(digits = 2,big.mark = ",",format = "f",mode = "double")),
                              selectInput(inputId = session$ns("update_coef"), label = "Coeficient de provizionare updatat",
                                          selected = vals_current$litigiu_selectat$update_coef_proviz,
                                          choices = c(0,0.25,0.65,1)),
                              textInput(session$ns("regularizare_provizion"),label = "Regularizare provizion",
                                           value = vals_current$litigiu_selectat$Regularizare_provizion %>% 
                                              formatC(digits = 2,big.mark = ",",format = "f",mode = "double")),
                              textAreaInput(session$ns("sentinta_actualizata"),label = "Sentinta actualizata",
                                            height = "250px",width = "400px",
                                            value = vals_current$litigiu_selectat$update_sentinta),
                              verbatimTextOutput(ns("diverse"))))
    ))
    
    #output$diverse <- renderPrint({str(vals_current$litigiu_selectat)})
  })
 
  
  
 
}
    
## To be copied in the UI
# mod_litigii_current_ui("litigii_current_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_current_server, "litigii_current_ui_1")
 
