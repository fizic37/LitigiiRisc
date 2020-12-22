#' actualizare_automata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_actualizare_automata_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    mod_litigii_platite_ui("litigii_platite_ui_1"),
    shinydashboard::box(title = "Actualizare automata solutii dosare existente",
                      status = "success",width = 12,collapsible = T,collapsed = F,
                      
                      actionButton(ns("select_rows"),
                      label = "Click aici pentru actualizare dosar",icon = icon("mouse-pointer")),br(),
                      DT::dataTableOutput(ns("dosare_actualizate")))
  )
  
 
}
    
#' actualizare_automata Server Function
#'
#' @noRd 
mod_actualizare_automata_server <- function(input, output, session, vals){
  ns <- session$ns
  
  load("data/litigii_sep.rda")
  
  vals_automated <- reactiveValues()
    #bi_litigii_contracte_platite = bi_litigii_contracte_platite,
    #bi_litigii_contracte_platite_snapshot = sinteza_bi_litigii$Current_Snapshot,
   # sinteza_bi_litigii = sinteza_bi_litigii)
  
  
  dosare_juridic <- readRDS("dosare_juridic_25_nov_2020") %>% 
    dplyr::filter(!is.na(ultima_data_pronuntare_results) & ultima_data_pronuntare_results > as.Date("2020-09-30"))
  
  
  
  output$text_data_actualizare <- renderText({paste0("DataSnapshot BI este ",as.Date("2020-11-20"))})
  
  # Output pentru dosarele cu sentinte automate si care trebuie prelucrate
  output$dosare_actualizate <- DT::renderDataTable({DT::datatable(
    selection = list(mode = "single",selected = NULL, target = "row"),
    options = list(dom = 'ftp', scrollX=T,pageLength = 5),
    class="nowrap",data = dosare_juridic %>% 
      dplyr::filter(!is.na(ultima_data_pronuntare_results) & ultima_data_pronuntare_results > as.Date("2020-09-30")) %>%
      dplyr::select(Numar_dosar = numar_ultimul_dosar_portal_just_results,
                    Ultima_data_pronuntare = ultima_data_pronuntare_results,
                    Calitate_FNGCIMM = lista_calitate_fngcimm_results,
                    Solutie_scurt = ultima_solutie_results,
                    Solutie_detaliat = ultima_solutie_scurt_results) %>% 
      dplyr::left_join(vals$litigii_update %>% dplyr::select(`Nr dosar instanta`,data_actualizare_sentinta), 
                            by = c("Numar_dosar" = "Nr dosar instanta")) %>% 
      dplyr::relocate(data_actualizare_sentinta,.after = Ultima_data_pronuntare) %>% 
      dplyr::rename_at(.vars = 'data_actualizare_sentinta', ~c("data_actualizare_procesata")),
        caption = "Dosare actualizate dupa data de 30 septembrie 2020:") %>%
      DT::formatDate(columns = 2,method = "toLocaleDateString") },server = F)
  
  # Observer for select row of dosare_actualizate de mai sus
  observeEvent(input$select_rows,{req(input$dosare_actualizate_rows_selected)
    vals_automated$dosare_juridic <- dosare_juridic %>%  dplyr::slice(input$dosare_actualizate_rows_selected)
    vals_automated$numar_dosar_selectat <-   vals_automated$dosare_juridic$numar_ultimul_dosar_portal_just_results
    
    if(any(stringr::str_detect(string = vals$litigii_update$`Nr dosar instanta`,
                                   pattern= vals_automated$numar_dosar_selectat))) {
    shinyWidgets::sendSweetAlert(session=session, title = "ATENTIE",type = "warning",
              text = "Litigiul selectat se gaseste deja in lista mea de litigii actualizate. Voi scrie peste modificarile tale") }
    
    
    showModal(modalDialog(
      title = ifelse(any(stringr::str_detect(string = vals$litigii_update$`Nr dosar instanta`,
                  pattern= vals_automated$numar_dosar_selectat)),
          paste0("Prelucrare litigiu ",vals_automated$numar_dosar_selectat, " - ATENTIE, 
                 datele sunt preluate din lista actualizata de litigii"),
        paste0("Prelucrare litigiu ",vals_automated$numar_dosar_selectat)),
      size = "l",
      footer = list(
        modalButton('Cancel'),
        actionButton(session$ns('submit'), 'Submit', class = "btn btn-primary")),
      
      fluidRow(
        column(
          width = 6,
          textInput(inputId = session$ns("numar_dosar_intern"),label = "Numar dosar instanta intern",width = "400px",
            value = litigii_sep %>% dplyr::filter(stringr::str_detect(pattern = vals_automated$numar_dosar_selectat,
                string = `Nr dosar instanta`)) %>%  dplyr::pull(`Nr dosar instanta`)),
          textAreaInput(
            session$ns("solutie_anterioara_dosar"),
            label = "Solutie anterioara dosar",
            width = "400px",
            height = "150px",
            value = ifelse(any(stringr::str_detect(string = vals$litigii_update$`Nr dosar instanta`,
                      pattern= vals_automated$numar_dosar_selectat)),
                vals$litigii_update %>% dplyr::filter(stringr::str_detect(pattern = vals_automated$numar_dosar_selectat,
                  string = vals$litigii_update$`Nr dosar instanta`)) %>%  dplyr::pull(update_sentinta),
              litigii_sep %>% dplyr::filter(stringr::str_detect(pattern = vals_automated$numar_dosar_selectat,
                    string = `Nr dosar instanta`)) %>%  dplyr::pull(`Stadiul procesului`))),
          
          selectInput(ns("coef_proviz_existent"),label = "Coeficientul actual de provizionare",width = "400px",
            choices = c(1,0.25,0,0.65),
            selected = ifelse(any(stringr::str_detect(string = vals$litigii_update$`Nr dosar instanta`,
                  pattern= vals_automated$numar_dosar_selectat)),
                        vals$litigii_update %>% dplyr::filter(stringr::str_detect(pattern = vals_automated$numar_dosar_selectat,
                    string = vals$litigii_update$`Nr dosar instanta`)) %>%  dplyr::pull(update_coef_proviz),
              litigii_sep %>% dplyr::filter(stringr::str_detect(pattern = vals_automated$numar_dosar_selectat,
              string = `Nr dosar instanta`)) %>%   dplyr::pull(`Coeficient provizionare`)))),
        
          column(width = 6,
              textInput(inputId = session$ns("calitate_fngcimm_solutie_noua"),
                        label = "Calitate procesuala FNGCIMM",
                width = "400px", value = vals_automated$dosare_juridic$lista_calitate_fngcimm_results),
              
              textAreaInput(session$ns("edit_solutie_dosar"),
                 label = "Solutie noua dosar",
                 width = "400px",
                 height = "150px",
                 value = vals_automated$dosare_juridic$ultima_solutie_scurt_results),
               
               selectInput(width = "400px",
                 inputId = session$ns("edit_coef_proviz"),
                 label = "Coeficientul de provizionare actualizat",
                 choices = c(1, 0.25, 0,0.65),
                 multiple = FALSE),
              dateInput(width = "400px",inputId = session$ns("edit_data_sentinta"),
                        label = "Data actualizare sentinta",
                        value = vals_automated$dosare_juridic$ultima_data_pronuntare_results)))))
    
    
  })
  
  # Observer for submit editare sentinta automata
  observeEvent(input$submit,{
    removeModal(session = session)
    
    # Nu toate numerele de dosar cautate automat coincid excat cu numerele de dosar din gestiunea interna (litigii_update care provine
    # oricum din litigii_sep)
    if (any(stringr::str_detect(string = vals$litigii_update$`Nr dosar instanta`,
                            pattern= vals_automated$numar_dosar_selectat))) {
        
      vals$litigii_update <- dplyr::bind_rows(litigii_sep %>% dplyr::slice(stringr::str_which(
        pattern = vals_automated$numar_dosar_selectat, string = litigii_sep$`Nr dosar instanta`))   %>% 
                                dplyr::mutate(data_actualizare_sentinta = input$edit_data_sentinta, 
                                       update_coef_proviz = as.numeric(input$edit_coef_proviz),
                                       update_sentinta = input$edit_solutie_dosar,updated_by = "automated"),
        vals$litigii_update %>% dplyr::slice(-(stringr::str_which(pattern = vals_automated$numar_dosar_selectat,
                           string = vals$litigii_update$`Nr dosar instanta`))))
          }
    else {
      vals$litigii_update <- dplyr::bind_rows(litigii_sep %>% dplyr::slice(stringr::str_which(
        pattern = vals_automated$numar_dosar_selectat, string = litigii_sep$`Nr dosar instanta`)) %>% 
          dplyr::mutate(data_actualizare_sentinta = input$edit_data_sentinta,
          update_coef_proviz = as.numeric(input$edit_coef_proviz),
          update_sentinta = input$edit_solutie_dosar,updated_by = "automated"),
          vals$litigii_update)
    }
    
    
    
  })
  
  # Observer - every time litigii_update se modifica, acesta va fi salvat in baza de date litigii_update.rda
  
  observeEvent(vals$litigii_update,{req(any(input$submit,input$submit_plata))
    litigii_update <- isolate(vals$litigii_update)
    
    usethis::use_data(litigii_update,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    
    shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",type = "success",
        text = "Lista de litigii a fost actualizata cu succes. Verifica in Sinteza Litigii - Litigii actualizate")
    
    
  })
  

}
    
## To be copied in the UI
# mod_actualizare_automata_ui("actualizare_automata_ui_1")
    
## To be copied in the server
# callModule(mod_actualizare_automata_server, "actualizare_automata_ui_1")
 
