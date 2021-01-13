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
  
  fluidRow(
    mod_litigii_noi_automate_ui("litigii_noi_automate_ui_1"),
    
    shinydashboard::box(title = "Actualizare automata solutii dosare existente (preluate de pe portal.just.ro)",
                      status = "success",width = 12,collapsible = T,collapsed = T,
                      
                      actionButton(ns("select_rows"),
                      label = "Click aici pentru actualizare dosar",icon = icon("mouse-pointer")),br(),
                      verbatimTextOutput(ns("diverse")),
                      DT::dataTableOutput(ns("dosare_actualizate"))),
    
    mod_litigii_platite_ui("litigii_platite_ui_1")
  )
  
 
}
    
#' actualizare_automata Server Function
#'
#' @noRd 
mod_actualizare_automata_server <- function(input, output, session, vals){
  ns <- session$ns
  
  dosare_juridic <- readRDS("sentinte_actualizate") %>% 
    dplyr::filter(!is.na(ultima_data_pronuntare_results) & ultima_data_pronuntare_results > as.Date("2020-09-30"))
  
  vals_automated <- reactiveValues()
  
  observeEvent(vals$litigii_curente,{
    vals_automated$litigii_update <- vals$litigii_curente %>% dplyr::filter(!is.na(updated_by))
    
    vals_automated$sentinte_actualizate <- dosare_juridic %>% 
      dplyr::filter(!is.na(ultima_data_pronuntare_results) & ultima_data_pronuntare_results > as.Date("2020-09-30")) %>%
      # I filter out dosare already updated with updated_by == automated
      dplyr::filter(!numar_ultimul_dosar_portal_just_results %in% vals_automated$litigii_update$`Nr dosar instanta`[
                vals_automated$litigii_update$updated_by == "automated"]) %>% 
      # I filter out dosare already updated with updated_by == automated_contract_platit
      dplyr::filter(!numar_ultimul_dosar_portal_just_results %in% vals_automated$litigii_update$`Nr dosar instanta`[
        vals_automated$litigii_update$updated_by == "automated_contract_platit"]) %>%
      dplyr::select(Numar_dosar = numar_ultimul_dosar_portal_just_results,
                    Ultima_data_pronuntare = ultima_data_pronuntare_results,
                    Calitate_FNGCIMM = lista_calitate_fngcimm_results,
                    Solutie_scurt = ultima_solutie_results,
                    Solutie_detaliat = ultima_solutie_scurt_results) %>% 
          dplyr::left_join(vals_automated$litigii_update %>%
                         dplyr::select(`Nr dosar instanta`,data_actualizare_sentinta) 
                       ,by = c("Numar_dosar" = "Nr dosar instanta")) %>% 
      dplyr::relocate(data_actualizare_sentinta,.after = Ultima_data_pronuntare) %>% 
      dplyr::rename_at(.vars = 'data_actualizare_sentinta', ~c("data_actualizare_procesata"))
  })
  
 
  
  
  
  
  output$text_data_actualizare <- renderText({paste0("DataSnapshot BI este ",as.Date("2020-11-20"))})
  
  
  # Output pentru dosarele cu sentinte automate si care trebuie prelucrate
  output$dosare_actualizate <- DT::renderDataTable({   DT::datatable(
    selection = list(mode = "single",selected = NULL, target = "row"),
    options = list(dom = 'ftp', scrollX=T,pageLength = 6),rownames = FALSE,
    class="nowrap",data = vals_automated$sentinte_actualizate,
        caption = "Dosare actualizate dupa data de 30 septembrie 2020:") %>%
      DT::formatDate(columns = 2,method = "toLocaleDateString") },server = F)
  
  # Observer for select row of dosare_actualizate de mai sus
  observeEvent(input$select_rows,{req(input$dosare_actualizate_rows_selected)
    
      # Here I store data selected from dosare juridic
     vals_automated$dosar_juridic_selectat <- vals_automated$sentinte_actualizate %>%  dplyr::slice(input$dosare_actualizate_rows_selected)
  
      # Here I store data selected from litigii curente
     vals_automated$dosar_selectat_litigii_curente <- vals$litigii_curente %>% 
       dplyr::filter(stringr::str_detect(pattern = vals_automated$dosar_juridic_selectat$Numar_dosar,
                                                string = vals$litigii_curente$`Nr dosar instanta`))
   
    
    if(any(stringr::str_detect(string = vals_automated$litigii_update$`Nr dosar instanta`,
                                   pattern= vals_automated$dosar_juridic_selectat$Numar_dosar))) {
    shinyWidgets::sendSweetAlert(session=session, title = "ATENTIE",type = "warning",
        text = "Litigiul selectat se gaseste deja in lista mea de litigii actualizate. Voi scrie peste modificarile tale") }
    
    
    showModal(modalDialog(
      title = ifelse(any(stringr::str_detect(string = vals_automated$litigii_update$`Nr dosar instanta`,
                  pattern= vals_automated$dosar_juridic_selectat$Numar_dosar)),
          paste0("Prelucrare litigiu ",vals_automated$dosar_juridic_selectat$Numar_dosar, " - ATENTIE, 
                 datele au fost actualizate o data!"),
        paste0("Prelucrare litigiu ",vals_automated$numar_dosar_selectat)),
      size = "l",
      footer = list(
        modalButton('Cancel'),
        actionButton(session$ns('submit'), 'Submit', class = "btn btn-primary")),
      
      fluidRow(
        column(width = 6,
          
               textInput(inputId = session$ns("numar_dosar_intern"),label = "Numar dosar instanta intern",width = "400px",
            value = vals_automated$dosar_selectat_litigii_curente$`Nr dosar instanta`),
          textAreaInput(
            session$ns("solutie_anterioara_dosar"),
            label = "Solutie anterioara dosar",
            width = "400px",
            height = "150px",
            value = ifelse(is.na(vals_automated$dosar_selectat_litigii_curente$update_sentinta),
                           vals_automated$dosar_selectat_litigii_curente$`Stadiul procesului`,
                           vals_automated$dosar_selectat_litigii_curente$update_sentinta)),
              
          
          selectInput(ns("coef_proviz_existent"),label = "Coeficientul actual de provizionare",width = "400px",
            choices = c(1,0.25,0,0.65),
            selected = ifelse(is.na(vals_automated$dosar_selectat_litigii_curente$update_coef_proviz),
                              vals_automated$dosar_selectat_litigii_curente$`Coeficient provizionare`,
                              vals_automated$dosar_selectat_litigii_curente$update_coef_proviz)),
          selectInput(ns("updated_by_existent"),label = "Tip update existent",width = "400px",
                      choices = c(NA,unique(vals$litigii_curente$updated_by)),
                      selected = vals_automated$dosar_selectat_litigii_curente$updated_by)),
             
        
          column(width = 6,
              textInput(inputId = session$ns("calitate_fngcimm_solutie_noua"),
                        label = "Calitate procesuala FNGCIMM",
                width = "400px", value = vals_automated$dosar_juridic_selectat$Calitate_FNGCIMM),
              
              textAreaInput(session$ns("edit_solutie_dosar"),
                 label = "Solutie noua dosar",
                 width = "400px",
                 height = "150px",
                 value = vals_automated$dosar_juridic_selectat$Solutie_detaliat),
               
               selectInput(width = "400px",
                 inputId = session$ns("edit_coef_proviz"),
                 label = "Coeficientul de provizionare actualizat",
                 choices = c(1, 0.25, 0,0.65),
                 multiple = FALSE),
              
              dateInput(width = "400px",inputId = session$ns("edit_data_sentinta"),
                        label = "Data actualizare sentinta",
                        value = as.Date(vals_automated$dosar_juridic_selectat$Ultima_data_pronuntare, format = "%Y-%m-%dT00:00:00"))
          )  )  )  )
                      
    
    
  })
  
  # Observer for submit editare sentinta automata
  observeEvent(input$submit,{
    removeModal(session = session)
    
    # Nu toate numerele de dosar cautate automat coincid exact cu numerele de dosar din gestiunea interna (litigii_update care provine
    # oricum din litigii_sep)
    if (any(stringr::str_detect(string =  vals_automated$dosar_selectat_litigii_curente$`Nr dosar instanta`,
                            pattern= vals_automated$dosar_juridic_selectat$Numar_dosar))) {
        
      vals$litigii_curente <- dplyr::bind_rows(vals$litigii_curente %>% dplyr::slice(stringr::str_which(
        pattern = vals_automated$dosar_juridic_selectat$Numar_dosar, string = vals$litigii_curente$`Nr dosar instanta`))   %>% 
                                dplyr::mutate(data_actualizare_sentinta = input$edit_data_sentinta, 
                                       update_coef_proviz = as.numeric(input$edit_coef_proviz),
                                       update_sentinta = input$edit_solutie_dosar,
                                       updated_by = "automated"),
        vals$litigii_curente %>% dplyr::slice(-(stringr::str_which(pattern = vals_automated$dosar_juridic_selectat$Numar_dosar,
                           string = vals$litigii_curente$`Nr dosar instanta`))))
          }
    else {
      vals$litigii_curente <- dplyr::bind_rows(vals$litigii_curente %>% dplyr::slice(stringr::str_which(
        pattern = vals_automated$dosar_juridic_selectat$Numar_dosar, string = vals$litigii_curente$`Nr dosar instanta`)) %>% 
          dplyr::mutate(data_actualizare_sentinta = input$edit_data_sentinta,
          update_coef_proviz = as.numeric(input$edit_coef_proviz),
          update_sentinta = input$edit_solutie_dosar,
          updated_by = "automated"),
          vals$litigii_curente %>% dplyr::filter(`Nr dosar instanta` != vals_automated$dosar_juridic_selectat$Numar_dosar))
      updateActionButton(session = session, inputId = "submit")
    }
    
    
    
  })
  
  # Observer - every time litigii_update se modifica, acesta va fi salvat in baza de date litigii_update.rda
  
  observeEvent(vals$litigii_curente,{req(any(input$submit,input$submit_plata))
    litigii_curente <- isolate(vals$litigii_curente)
    
    usethis::use_data(litigii_curente,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    
    shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",type = "success",
        text = "Lista de litigii a fost actualizata cu succes. Verifica in Sinteza Litigii - Litigii actualizate")
    
    
  })
  

}
    
## To be copied in the UI
# mod_actualizare_automata_ui("actualizare_automata_ui_1")
    
## To be copied in the server
# callModule(mod_actualizare_automata_server, "actualizare_automata_ui_1")
 
