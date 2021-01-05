#' litigii_platite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_litigii_platite_ui <- function(id){
  ns <- NS(id)
 
  shinydashboard::box(title = "Update plati",
    width = 12,  status = "success", collapsible = T,  collapsed = T,
    DT::dataTableOutput(ns("sinteza_bi_plati")),
    hr(),
    fluidRow(
      # sets color of Upload BI text and color of downloadlink
      tags$head(tags$style(HTML("label {color : #bc3c4b;} a {color: #bc3c4b;}"))),
      
      column(width = 6,
        fileInput( inputId = ns("plati_input"),   label = "Upload BI",
          buttonLabel = "Excel only",   width = "300px",  placeholder =  "no file uploaded" )   ),
      
      column(width = 6,
        br(), downloadLink(outputId = ns("down_bi_plati"),
                     label = "Click aici pentru a downloada BI-ul de plati") )  ),
    hr(),
    uiOutput(ns("show_edit_litigii_platite")),
    hr(),
    verbatimTextOutput(ns("diverse")),
    DT::dataTableOutput(ns("litigii_contracte_platite"))
  )
  
}
    
#' litigii_platite Server Function
#'
#' @noRd 
mod_litigii_platite_server <- function(input, output, session,vals){
  ns <- session$ns
  
  load('data/bi_litigii_contracte_platite.rda')
  load("data/sinteza_bi_litigii.rda")
  
  vals_litigii_platite <- reactiveValues(bi_litigii_contracte_platite = bi_litigii_contracte_platite,
                                         sinteza_bi_litigii = sinteza_bi_litigii)
  
  observe({
    
    vals_litigii_platite$litigii_platite <- vals$litigii_curente %>% 
      # I filter out contracte already processed
      dplyr::filter(updated_by != "automated_contract_platit" | is.na(updated_by)) %>% 
      # I filter only contracts within contracte platite
      dplyr::filter(`Nr contract` %in% vals_litigii_platite$bi_litigii_contracte_platite$`Numar contract`) %>%
      # I do not use left_join since I work with reactive dataframes and left_joins creates multiple columns - it is activated multiple times
      dplyr::mutate(data_plata = vals_litigii_platite$bi_litigii_contracte_platite$DataPlata1[match(`Nr contract`,
                                    table = vals_litigii_platite$bi_litigii_contracte_platite$`Numar contract`)],
                    ValoarePlata1 = vals_litigii_platite$bi_litigii_contracte_platite$ValoarePlata1[match(`Nr contract`,
                              table = vals_litigii_platite$bi_litigii_contracte_platite$`Numar contract`)]) %>%
      dplyr::filter(data_plata > as.Date("2020-09-30"))
  })
  # Here I calculate contracte platite
  output$litigii_contracte_platite <- DT::renderDataTable ({req(vals_litigii_platite$litigii_platite)
    DT::datatable(class = "nowrap",rownames = FALSE, 
                  selection = list(mode = "single",selected = NULL, target = "row"),
                  data = vals_litigii_platite$litigii_platite,
                  options =list(dom = "tp", scrollX = TRUE, pageLength = 5),
                  caption = ifelse(nrow(vals_litigii_platite$litigii_platite) >0 ,"Contracte de Garantare aferente unor litigii platite dupa data de 30 septembrie 2020 si care nu se regasesc in lista
    de litigii actualizate platite:", "Nu exista contracte platite dupa 30 septembrie neactualizate in lista de litigii platite"))  })
  
  output$show_edit_litigii_platite <- renderUI({req(nrow(vals_litigii_platite$litigii_platite)>0)
    actionButton(inputId = session$ns("edit_litigii_platite"), icon = icon("mouse-pointer"),
                 label = "Select row and click to update provizion litigiu contract platit")  })
  
  
  
  
  observeEvent(input$edit_litigii_platite,{req(input$litigii_contracte_platite_rows_selected) 
    
    vals_litigii_platite$dosar_selectat_plata <- vals_litigii_platite$litigii_platite %>% 
      dplyr::slice(input$litigii_contracte_platite_rows_selected)
    
    showModal(modalDialog(title = paste0("Edit litigiu pentru procesare plata, numar litigiu - ",
    vals_litigii_platite$dosar_selectat_plata$`Nr dosar instanta`, ", beneficiar ",
    vals_litigii_platite$dosar_selectat_plata$Beneficiar_Juridic,
            ", numar contract de garantare ", 
    vals_litigii_platite$dosar_selectat_plata$`Nr contract`),  size = "l",
                          footer = list(modalButton('Cancel'),actionButton(session$ns('submit_plata'), 
                                                              'Submit', class = "btn btn-primary")),
                          
                          fluidRow(column(width = 6,textAreaInput(session$ns("solutie_dosar_platit"),label = "Ultima solutie",
                                          value = vals_litigii_platite$dosar_selectat_plata$`Stadiul procesului`,
                                          width = "400px", height = "150px"),
                                          
                                          numericInput(inputId = session$ns("coef_proviz_contract_platit"),
                                                  label = "Coeficient actual de provizionare", width = "400px", 
                                            value = vals_litigii_platite$dosar_selectat_plata$`Coeficient provizionare`)),
                                   
                                   column(width = 6, dateInput(inputId = session$ns("edit_data_plata"),label = "Data platii contractului",
                                                               value = vals_litigii_platite$dosar_selectat_plata$data_plata,width = "400px"),
                                          selectInput(inputId = session$ns("edit_coef_plata"),label = "Selecteaza noul coeficient de provizionare",
                                                      choices = c(0,1,0.25,0.65),selected = 0,width = "400px")
                                   ))
    ))
    
    shinyjs::disable("solutie_dosar_platit")
    shinyjs::disable("coef_proviz_contract_platit")
    
  })
  
  observeEvent(input$submit_plata,{
    removeModal(session = session)
    
    vals_litigii_platite$litigiu_plati_update <- vals_litigii_platite$dosar_selectat_plata %>% 
      dplyr::mutate(data_plata = input$edit_data_plata, 
                    update_coef_proviz = as.numeric(input$edit_coef_plata),
                    update_sentinta = NA_character_,
                    updated_by = "automated_contract_platit")
    
    if (janitor::compare_df_cols_same(vals_litigii_platite$litigiu_plati_update,vals$litigii_curente)) {
      
      vals$litigii_curente <- dplyr::bind_rows(vals_litigii_platite$litigiu_plati_update,
                      vals$litigii_curente %>% 
                        dplyr::filter(`Nr dosar instanta` != vals_litigii_platite$dosar_selectat_plata$`Nr dosar instanta`))
      
      vals_litigii_platite$litigii_platite <-  vals_litigii_platite$litigii_platite %>% 
        dplyr::filter(`Nr dosar instanta` != vals_litigii_platite$dosar_selectat_plata$`Nr dosar instanta`) 
        
      }
    
    else {shinyWidgets::sendSweetAlert(session = session,title = "STOP", type = "error",
                                       text = "Nu am putut salva fisierul. Contacteaza administratorul!")}
    
  })
  
  # Observer - every time litigii_curente se modifica, acesta va fi salvat in baza de date litigii_update.rda
  
  observeEvent(vals$litigii_curente,{req(input$submit_plata)
    litigii_curente <- isolate(vals$litigii_curente)
    
    usethis::use_data(litigii_curente,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    
    shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",type = "success",
          text = "Lista de litigii a fost actualizata cu succes. Verifica in Litigii actualizate - 
          Litigiile actualizate - Litigii platite")
    
   })
  
  output$down_bi_plati <- downloadHandler(filename = {"BI_contracte_platite.xlsx"},
            content = function(file) {file.copy(from = "inst/extdata/bi_litigii.xlsx",to = file)} )
  
  
  # Below code handles processing and storing of BI plati input
  
  output$sinteza_bi_plati <- DT::renderDataTable({
    DT::datatable(rownames = FALSE, data = vals$sinteza_bi_litigii,
    caption = "Sinteza date stocate BI contracte platite:",options = list(dom = "t"))  })
  
  observeEvent(input$plati_input,{
    
    vals_litigii_platite$bi_litigii_contracte_platite <- readxl::read_excel(input$plati_input$datapath,
        sheet = "plata1", skip = 5) %>%
      dplyr::mutate(dplyr::across(.cols = Day,  ~ as.Date(.x, format = "%Y-%m-%d 00:00:00.000"))) %>%
      dplyr::select(`Numar contract`, DataPlata1 = Day,  ValoarePlata1 = Total)
    
    
    vals_litigii_platite$bi_litigii_contracte_platite_snapshot <- readxl::read_excel(input$plati_input$datapath,
              sheet = "plata1", n_max = 4) %>%
      dplyr::slice(1) %>% dplyr::pull(2) %>% as.Date()
    
    
    
    vals_litigii_platite$sinteza_bi_litigii <- data.frame(
      Current_Snapshot = vals_litigii_platite$bi_litigii_contracte_platite_snapshot,
      Minimum_DataPlata1 = min(vals_litigii_platite$bi_litigii_contracte_platite$DataPlata1),
      Maximum_DataPlata1 = max(vals_litigii_platite$bi_litigii_contracte_platite$DataPlata1),
      Nr_observatii_bi_stocat = nrow(vals_litigii_platite$bi_litigii_contracte_platite),
      stringsAsFactors = FALSE)
    
  })
  
  observeEvent(vals_litigii_platite$sinteza_bi_litigii,{req(input$plati_input)
    
    if (vals_litigii_platite$bi_litigii_contracte_platite_snapshot > sinteza_bi_litigii$Current_Snapshot) {
      
      sinteza_bi_litigii <- isolate(vals_litigii_platite$sinteza_bi_litigii)
      
      usethis::use_data(sinteza_bi_litigii,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
      
      bi_litigii_contracte_platite <- isolate( vals_litigii_platite$bi_litigii_contracte_platite)
      
      usethis::use_data(bi_litigii_contracte_platite,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
      
      
      
      shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",type = "succes",
                                   text = "Am actualizat cu succes baza mea de contracte de garantare platite") }
    
    else { shinyWidgets::sendSweetAlert(session = session,title = "Atentie!",type = "warning",
                                        text = "Data snapshot furnizata este mai mica decat ce am stocat. Nu am actualizat")}
    
  }) 
  
  output$sinteza_bi_plati <- DT::renderDataTable({
    DT::datatable(rownames = FALSE,caption = "Sinteza date stocate BI contracte platite:",
                  options = list(dom = "t"),class = "compact",
                  data = vals_litigii_platite$sinteza_bi_litigii)  })
 
}
    
## To be copied in the UI
# mod_litigii_platite_ui("litigii_platite_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_platite_server, "litigii_platite_ui_1")
 
