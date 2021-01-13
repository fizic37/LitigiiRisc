#' litigii_noi_manuale UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_litigii_noi_manuale_ui <- function(id){
  ns <- NS(id)
    
 actionButton(inputId = ns("new_litigiu"), label = "Litigiu nou manual",
                        style = "color: #3cbcad;", width = "30%", icon = icon('plus'))
 
                        
                     
  
}
    
#' litigii_noi_manuale Server Function
#'
#' @noRd 
mod_litigii_noi_manuale_server <- function(input, output, session, vals){
  ns <- session$ns
  
  vals_litigii_noi_manuale <- reactiveValues()
  
  # Observer pentru buton litigiu nou (manual)
  
  observeEvent(input$new_litigiu,{
    load("data/bi_contracte.rda")
    
    showModal(modalDialog(title = "New Litigiu", size = "l", footer = list(modalButton("Cancel"),
                              actionButton(session$ns("save_new_litigiu_manual"),label = "Save",icon = icon("save"))),
              fluidRow(
                column(width = 6,
                          textInput(inputId = session$ns("numar_dosar_litigiu_nou_manual"),
                                                    label = "Numarul dosarului:", value = ""),
                                          
                          selectInput(session$ns("banca_litigiu_nou_manual"),label = "Selecteaza banca:",
                                                    choices = c("",unique(bi_contracte$Banca))),
                                          
                          selectizeInput(session$ns("beneficiar_litigiu_nou_manual"),
                                      "Selecteaza beneficiarul",choices = ""),
                                          
                          selectizeInput(session$ns("contract_litigiu_nou_manual"),
                                      "Selecteaza contractul de garantare",   choices = ""),
                                          
                           selectizeInput(session$ns("cui_beneficiar_litigiu_nou_manual"),
                                         "CUI-ul beneficiarului:",   choices = ""),
                                          
                           selectizeInput(session$ns("cod_partener_beneficiar_litigiu_nou_manual"),
                                          "Cod partener:",   choices = "")),
                                   
                               
               column(width = 6, 
                           shinyWidgets::numericInputIcon(session$ns("valoare_litigiu_nou_manual"), value = 0,
                                 label = "Introdu valoarea litigiului in lei",min = 1,width = "370px",icon = icon("dollar"),
                                        help_text = "Litigiul trebuie sa fie mai mare ca zero pentru a putea fi salvat"),
                                          
                            selectInput(session$ns("tip_litigiu_nou_manual"),
                                        label = "Selecteaza tipul litigiului",width = "370px",
                                        choices = unique(vals$litigii_curente$Tip_litigiu), selected = "somatie"),
                                          
                          dateInput(inputId = session$ns("data_litigiu_nou_manual"),min = as.Date("2020-09-30"),
                                         label = "Data dosarului:",value = Sys.Date(), width = "370px"),
                                         
                                          
                          selectInput(session$ns("coef_proviz_litigiu_nou_manual"),
                                         label = "Selecteaza coeficientul de provizionare",width = "370px",
                                            choices = c(0.65,1,0.25,0)),
                                          
                          textAreaInput(inputId = session$ns("solutie_litigiu_nou_manual"),
                                          label = "Copiaza solutia dosarului (daca exista) 
                                          in casuta de mai jos:",value = "",height = "110px",width = "370px")
                                   ))))
  })
  
  
  observeEvent(input$banca_litigiu_nou_manual,{
    vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual <- bi_contracte %>% 
      dplyr::filter(Banca == input$banca_litigiu_nou_manual)
    
    updateSelectizeInput(session = session, inputId = "beneficiar_litigiu_nou_manual", server = TRUE,selected = character(0),
        choices = c(character(0), unique(vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual$NumeBeneficiar)))
    
   })
  
  observeEvent(input$beneficiar_litigiu_nou_manual,{
    updateSelectizeInput(session = session, inputId = "contract_litigiu_nou_manual", server = TRUE, selected = character(0),
           choices = c(character(0),vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual %>% 
                      dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou_manual) %>%
                                            dplyr::pull(NumarContract) %>% unique()))
    
    updateSelectizeInput(session = session, inputId = "cui_beneficiar_litigiu_nou_manual", server = TRUE,
            choices = c(vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual %>%
                      dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou_manual) %>%
                                              dplyr::pull(CUI) %>% unique(),character(0)))
    
    updateSelectizeInput(session = session, inputId = "cod_partener_beneficiar_litigiu_nou_manual", server = TRUE,
            choices = c(vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual %>%
                          dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou_manual) %>%
                                dplyr::pull(CodPartener) %>% unique(),character(0)))
    
   })
  
  # Observer for save litigiu nou manual
  observeEvent(input$save_new_litigiu_manual,{
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_new_litigiu_manual"),
                                   btn_colors = c("#bc3c4b","#a5dc86"),type = "success",
                                   title = "CONFIRM",text = "Esti sigur ca vrei sa salvezi litigiul nou?")
  })
    
    observeEvent(input$confirm_new_litigiu_manual,{
      if (as.numeric(input$valoare_litigiu_nou_manual) < 2) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                     text = "Nu pot salva un litigiu nou fara valoare")
      }
      
      else {
        vals_litigii_noi_manuale$litigiu_nou_manual <- data.frame(Nr_crt = as.integer(max(vals$litigii_curente$Nr_crt)+1),
                                                          `Nr dosar instanta` = input$numar_dosar_litigiu_nou_manual,
                                                          `Beneficiar_Juridic` = input$beneficiar_litigiu_nou_manual,
                                                          `Nr contract` = input$contract_litigiu_nou_manual,
                                                          CodPartener = input$cod_partener_beneficiar_litigiu_nou_manual,
                                                          CUI = input$cui_beneficiar_litigiu_nou_manual,
                                                          Banca = input$banca_litigiu_nou_manual,
                                                          `Suma litigiu - echiv lei` = as.numeric(input$valoare_litigiu_nou_manual),
                                                          #`Stadiul procesului` = input$solutie_litigiu_nou_manual,
                                                          #`Coeficient provizionare` = as.numeric(input$coef_proviz_litigiu_nou_manual),
                                                          #`Necesar provizion septembrie 2020` = 0,
                                                          update_sentinta = input$solutie_litigiu_nou_manual,
                                                          update_coef_proviz = as.numeric(input$coef_proviz_litigiu_nou_manual),
                                                          Tip_litigiu = input$tip_litigiu_nou_manual,
                                                          data_actualizare_sentinta = input$data_litigiu_nou_manual,
                                                          stringsAsFactors = FALSE,check.names = FALSE) %>% 
                        dplyr::mutate(updated_by = "litigiu_nou_manual")
        
        vals$litigii_curente <- dplyr::bind_rows(vals_litigii_noi_manuale$litigiu_nou_manual,vals$litigii_curente)
        updateActionButton(session = session,inputId = "save_new_litigiu_manual")
        removeModal(session=session)
        #output$diverse <- renderPrint({c(str(vals_litigii_noi_manuale$litigiu_nou_manual), input$save_litigiu_manual)})
      }
      
    })
  
  
  # Key observer. Every time I update vals_litigii_curente following confirmation input of litigiu update,
  # litigiu nou manual sau litigiu nou automat, se salveaza vals$litigii_curente
  observeEvent(vals$litigii_curente,{req(any(input$confirm_new_litigiu_manual,input$confirm_new_litigiu,
                                             input$confirm_litigiu_update))
    litigii_curente <- isolate(vals$litigii_curente)
    
    usethis::use_data(litigii_curente,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    
    shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",type = "success",
                                 text = "Lista de litigii a fost actualizata cu succes.")
    
    #output$litigii_curente <- DT::renderDataTable({
      #DT::datatable(data = litigii_curente, options = list(dom = "ftp"))})
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
 
}
    
## To be copied in the UI
# mod_litigii_noi_manuale_ui("litigii_noi_manuale_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_noi_manuale_server, "litigii_noi_manuale_ui_1")
 
