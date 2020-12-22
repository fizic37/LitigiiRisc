#' litigii_noi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_litigii_noi_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabBox(width = 12,tabPanel(
  tags$style(HTML('.nav-tabs-custom>.nav-tabs>li.active {border-top-color: #3cbcad;}')),
  
    title = "Litigii noi",icon = icon("plus-square"),
  fluidRow(
  shinydashboard::box(title = "Litigii noi automatizate",width = 12,status = "success",collapsible = T,collapsed = T,
  shinyjs::useShinyjs(),
  tagList(
  DT::dataTableOutput(ns("tabel_litigii")) ,
  
  tags$script(src = "datatable.js"),
  tags$script(paste0("table_module_js('", ns(''), "')"))
  )),
  shinydashboard::box(title = "Litigii noi care actualizeaza litigii existente",width = 12, 
                  status = "success",collapsible = T,collapsed = F,
    
                  DT::dataTableOutput(ns("litigii_noi_updated")),
                  tags$script(src = "datatable_updated.js"),
                  tags$script(paste0("table_updated_module_js('", ns(''), "')"))),
  shinydashboard::box(tags$style(HTML('#litigii_noi_ui_1-new_litigiu { border-color: #fff;}')),
    title = "Litigii noi - Introducere manuala",width = 12, 
                      status = "success",collapsible = T,collapsed = T,
    shinyjs::useShinyjs(debug = TRUE),
      shinyWidgets::actionBttn(inputId = ns("new_litigiu"),label = "Introdu litigiu nou",color = "primary",
                                   icon =icon("plus-circle")))
  )  ),
                  
                  
  tabPanel(title = "Litigii noi excluse",icon = icon("minus-square"),
           DT::dataTableOutput(ns("litigii_noi_excluse"))),
  tabPanel(title = "Miscellaneous"))
           
  
  
}
    
#' litigii_noi Server Function
#'
#' @noRd 
mod_litigii_noi_server <- function(input, output, session, vals){
  ns <- session$ns
  
  load("data/dosare_excluse.rda")
  load("data/litigii_sep.rda")
  load("data/litigiu_nou_from_updates.rda")
  
  
  
  dosare_noi <- readRDS("lista_dosare_noi") %>% dplyr::filter(prima_data_dosar_interogare > as.Date("2020-09-30"))
  
  
  vals_litigii_noi <- reactiveValues(dosare_excluse = dosare_excluse,litigiu_nou_from_updates = litigiu_nou_from_updates)
  
  # This observer introduces actions reactive. I need it in order to show less new litigii as I exclude them or save them
  # Also introduces vals_litigii_noi$dosare_noi. Ajuta foarte mult avand in vedere dinamica dosarelor noi de aratat (fie
  # se salveaza litigii noi, fie se exclud litigii noi)
  observe({
    
    
     
  vals_litigii_noi$dosare_noi <- dosare_noi %>% 
    # I eliminate Faliment
    dplyr::filter(categorie_caz_nume_interogare != "Faliment") %>%
    #I eliminate dosare excluse 
    dplyr::filter(!numar_dosare_interogare %in% vals_litigii_noi$dosare_excluse$numar_dosare_interogare) %>%
    # I eliminate dosare care au fost deja considerate noi sau se gasesc in lista de noi manuale
    dplyr::filter(!numar_dosare_interogare %in% vals$litigii_update$`Nr dosar instanta`[
      vals$litigii_update$updated_by %in% c("litigiu_nou_automated","litigiu_nou_manual")]) 
  
  # Here I store litigii noi care de fapt actualizeaza litigii existente
   vals_litigii_noi$dosare_noi_updated <- vals_litigii_noi$dosare_noi %>%
    dplyr::slice(purrr::map(.x = vals_litigii_noi$dosare_noi$numar_dosare_interogare,
                            # I check to see if numar old dosar se gaseste inside numarul nou
                            ~stringr::str_which(string = .x,pattern = litigii_sep$`Nr dosar instanta`)) %>%
                   purrr::map_dbl(.x = .,~purrr::detect_index(.x,~length(.x) != 0)) %>% which(x = .>0)) %>%
    # Elimin dosarele care au fost deja actualizate
    dplyr::filter(!numar_dosare_interogare %in% 
                    vals$litigii_update$`Nr dosar instanta`[vals$litigii_update$updated_by == "update_litigiu"]) %>%
     # Elimin dosarele care au fost marcate ca noi si nu ca update
     dplyr::filter(!numar_dosare_interogare %in% vals_litigii_noi$litigiu_nou_from_updates$numar_dosare_interogare)
  
  vals_litigii_noi$dosare_noi_final <- vals_litigii_noi$dosare_noi %>% dplyr::filter(!numar_dosare_interogare %in% 
                vals_litigii_noi$dosare_noi_updated$numar_dosare_interogare) %>%
    dplyr::filter(!numar_dosare_interogare %in% 
                    vals$litigii_update$`Nr dosar instanta`[vals$litigii_update$updated_by == "update_litigiu"]) %>%
    dplyr::bind_rows(vals_litigii_noi$litigiu_nou_from_updates)
  
  vals_litigii_noi$actions <- purrr::map_chr(vals_litigii_noi$dosare_noi_final$numar_dosare_interogare, function(id_) {
    paste0(
      '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', 
      id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          </div>'
    )  })
  
  vals_litigii_noi$updated_actions <- purrr::map_chr(vals_litigii_noi$dosare_noi_updated$numar_dosare_interogare, function(id_) {
    paste0(
      '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Update" id = ', 
      id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          </div>'
    )  })
  })
  
  output$litigii_noi_excluse <- DT::renderDataTable({ DT::datatable(rownames = FALSE, class = "nowrap",
    data = dosare_excluse %>% dplyr::select(1,5,7:8,2:3,6,4,9), 
    options = list(dom = "ftp", scrollX = TRUE),
    caption = "Litigiile noi ale FNGCIMM excluse din lista de litigii provizionate")
  })
  
  output$litigii_noi_updated <- DT::renderDataTable({ DT::datatable(rownames = FALSE,  class = "nowrap",
        data = cbind(tibble::tibble(" " = vals_litigii_noi$updated_actions),vals_litigii_noi$dosare_noi_updated) %>%
          dplyr::select(1:2,8,6,9,10,7,3:5),
        options = list(dom = "ftp", scrollX = TRUE), escape = 0,
        caption = "Litigii noi aflate deja in lista soldurilor inghetate"
      )
    })
  
    output$tabel_litigii <- DT::renderDataTable({DT::datatable(rownames = FALSE, escape = FALSE,
        caption = "Litigii noi ale FNGCIMM dupa 30 septembrie",
        data = cbind(tibble::tibble(" " = vals_litigii_noi$actions),
                     vals_litigii_noi$dosare_noi_final %>% dplyr::select(1, 6, 7, 5, 2:4)),
        extensions = c('Select', 'SearchPanes'),
        selection = "none",
        class = "nowrap",
        options = list(pageLength = 5, dom = "Pftp", scrollX = TRUE,
          columnDefs = list(
            list(
              searchPanes = list(show = FALSE),
              targets = c(0:2, 4:7)
            ),
            list(
              searchPanes = list(controls = FALSE),
              targets = 3
            )
          )
        )
      )
    }, server = F)
  
    observeEvent(input$id_dosar_updated,{
   vals_litigii_noi$dosar_updated_selectat <- vals_litigii_noi$dosare_noi_updated %>% 
                      dplyr::filter(numar_dosare_interogare == input$id_dosar_updated)
 
   
   vals_litigii_noi$litigii_inghetat_selectat <- litigii_sep %>% 
     dplyr::slice( stringr::str_which(pattern = litigii_sep$`Nr dosar instanta`,
                              string = input$id_dosar_updated))
   showModal(modalDialog(
     title = "Update Litigiu", size = "l",
     footer = fillRow(flex = c(1,NA),
        tagList(
      actionButton(session$ns("litigiu_nou_update"),"Acesta este un litigiu nou", icon=icon("plus-square")),
      actionButton(session$ns("litigiu_exclus_update"),"Exclude acest litigiu",icon = icon("minus-circle"))),
      br(),tagList(modalButton('Cancel'),
     actionButton(session$ns('update_litigiu'), 'Update litigiu nou', class = "btn btn-primary", icon = icon("pen-square")))),
     fluidRow(
       column(width = 6,
              textInput(inputId = session$ns("numar_litigiu_existent"),label = "Numar Litigiu existent:", width ="400px",
                        value = vals_litigii_noi$litigii_inghetat_selectat$`Nr dosar instanta`),
              textInput(inputId = session$ns("banca_litigiu_existent"),label = "Banca litigiu",width = "400px",
                        value =  vals_litigii_noi$litigii_inghetat_selectat$Banca),
              textAreaInput(inputId = session$ns("ultima_solutie_disponibila"),label = "Ultima solutie disponibila:",
                            width = "400px", height = "150px",
                            value = vals_litigii_noi$litigii_inghetat_selectat$`Stadiul procesului`),
              numericInput(inputId = session$ns("coef_actual_provizion_litigiu_updated"),width = "400px",
                           label = "Coeficient actual de provizionare",
                           value = vals_litigii_noi$litigii_inghetat_selectat$`Coeficient provizionare`)),
       
       column(width = 6,
              textInput(inputId = session$ns("numar_litigiu_nou"),label = "Numar Litigiu actualizat:", width ="400px",
                  value = input$id_dosar_updated),
              
              textAreaInput(session$ns("parti_litigiu_updated"),label = "Parti dosar:", width = "400px",
                            value = vals_litigii_noi$dosar_updated_selectat$parte_dosar),
              dateInput(session$ns("data_pronuntare_updated"),label = "Data pronuntarii sentintei:",width = "400px",
                        value = vals_litigii_noi$dosar_updated_selectat$data_pronuntare_interogare),
              
              textInput(session$ns("calitate_fngcimm_updated"),label = "Ultima calitate FNGCIMM:",width = "400px",
                        value = vals_litigii_noi$dosar_updated_selectat$lista_calitate_fngcimm_interogare),
              
              textAreaInput(inputId = session$ns("ultima_solutie_actualizata"),label = "Solutie actualizata:",
              width = "400px", height = "150px", value =  vals_litigii_noi$dosar_updated_selectat$solutie_sumar_interogare),
        
        selectInput(session$ns("coef_proviz_updated"),label = "Selecteaza noul coeficient de provizionare",width = "400px",
              choices =  c(1,0.25,0.65,0), selected = ifelse(input$ultima_solutie_actualizata =="",
                                                        as.numeric(input$coef_actual_provizion_litigiu_updated),1))
     ))
    
   ))
 })
  
  
  observeEvent(input$update_litigiu,{
    removeModal(session = session)
    
    vals_litigii_noi$litigiu_de_adaugat <- vals_litigii_noi$litigii_inghetat_selectat %>% 
      dplyr::mutate(`Nr dosar instanta` = input$numar_litigiu_nou,
                    update_coef_proviz = as.numeric(input$coef_proviz_updated),
                    update_sentinta = ifelse(is.null(input$ultima_solutie_actualizata),NA,input$ultima_solutie_actualizata),
                    data_actualizare_sentinta = ifelse(is.null(input$data_pronuntare_updated),NA,input$data_pronuntare_updated),
                    data_plata = NA,
                    updated_by = "update_litigiu") %>% 
      dplyr::mutate(dplyr::across(.cols = dplyr::contains("data"),~as.Date(.x,origin = "1970-01-01")))
    
    if (janitor::compare_df_cols_same(litigii_update,vals_litigii_noi$litigiu_de_adaugat, bind_method = "bind_rows",verbose = F)) {
      
      vals$litigii_update <-   dplyr::bind_rows(vals_litigii_noi$litigiu_de_adaugat,vals$litigii_update) }
    
  })
  
  observeEvent(input$litigiu_nou_update,{
    vals_litigii_noi$litigiu_nou_from_updates <- dosare_noi %>% dplyr::filter(numar_dosare_interogare == input$id_dosar_updated) %>%
      dplyr::bind_rows(litigiu_nou_from_updates %>% dplyr::filter(numar_dosare_interogare != input$id_dosar_updated))
    litigiu_nou_from_updates <- isolate(vals_litigii_noi$litigiu_nou_from_updates)
    usethis::use_data(litigiu_nou_from_updates,internal = FALSE,overwrite = TRUE,version = 3,compress = "gzip")
    shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",type = "success",
                                 text = "Litigiul se gaseste acum in lista de litigii noi de mai sus")
  })
  
  observeEvent(vals$litigii_update,{req(input$update_litigiu)
    litigii_update <- isolate(vals$litigii_update)
    
    usethis::use_data(litigii_update,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    
    shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",type = "success",
          text = "Lista de litigii a fost actualizata cu succes. Verifica in Litigii actualizate- Litigii noi care 
            inlocuiesc sentinte existente")
    
  })
  
  observeEvent(input$id_dosar, {
    vals_litigii_noi$dosar_selectat <- dosare_noi[dosare_noi$numar_dosare_interogare == input$id_dosar,]
    
    showModal(modalDialog(
      title = "Editeaza litigiul",
      size = "l",
      footer = list(
        modalButton('Cancel'),
        actionButton(session$ns('save_litigiu'),
                     'Save litigiu nou', class = "btn btn-primary", icon = icon("save")),
        actionButton(session$ns('exclude_litigiu'), 'Exclude litigiul', class = "btn btn-primary", icon = icon("minus-circle"))),
      
      fluidRow(
        column(
          width = 6,
          textInput(
            session$ns("numar_dosar_litigiu_nou"),
            label = "Numar dosar instanta",
            width = "400px",
            value = dosare_noi$numar_dosare_interogare[dosare_noi$numar_dosare_interogare == input$id_dosar]),
          
          shinyWidgets::numericInputIcon(session$ns("valoare_litigiu_nou"), value = 0,
                label = "Introdu valoarea litigiului in lei",min = 1,width = "400px",icon = icon("dollar"),
                                         help_text = "Litigiul trebuie sa fie mai mare ca zero pentru a putea fi salvat"),
          shinyWidgets::pickerInput(session$ns("coef_proviz_litigiu_nou"),
                                    label = "Selecteaza coeficientul de provizionare",width = "400px",
                                    choices = c(0.65,1,0.25,0)),
          
          textAreaInput(
            session$ns("solutie_litigiu_nou"),
            label = "Solutie dosar",
            width = "400px",
            height = "150px",
            value = dosare_noi$solutie_sumar_interogare[dosare_noi$numar_dosare_interogare == input$id_dosar]),
          
          textAreaInput(session$ns("parti_dosar"),label = "Parti dosar",width = "400px",height="150px",
                        value = dosare_noi$parte_dosar[dosare_noi$numar_dosare_interogare == input$id_dosar]),
          verbatimTextOutput(ns("diverse"))
          ),
        column(width = 6,
               shinyWidgets::pickerInput(session$ns("tip_litigiu_nou"),
                                         label = "Selecteaza tipul litigiului",width = "400px",
                                         choices = unique(litigii_sep$Tip_litigiu), selected = "somatie"),
               dateInput(inputId = session$ns("data_litigiu_nou"),label = "Data dosarului", width = "400px",
                         value = dosare_noi$prima_data_dosar_interogare[dosare_noi$numar_dosare_interogare == input$id_dosar]),
               dateInput(inputId = session$ns("ultima_data_pronuntare"),label = "Ultima data de pronuntare", width = "400px",
                         value = dosare_noi$data_pronuntare_interogare[dosare_noi$numar_dosare_interogare == input$id_dosar]),
               textInput(
                 session$ns("calitate_fngcimm"),
                 label = "Calitate FNGCIMM",
                 width = "400px",
                 value = dosare_noi$lista_calitate_fngcimm_interogare[dosare_noi$numar_dosare_interogare == input$id_dosar]),
               textInput(session$ns("categorie_caz"),
                 label = "Categorie caz",
                 width = "400px",
                 value = dosare_noi$ultima_categorie_caz_interogare[dosare_noi$numar_dosare_interogare == input$id_dosar]),
               
               
               actionButton(inputId = session$ns("show_more"),label = "Show more to save litigiu",
                            icon = icon("plus"),width = "400px"),
               
               uiOutput(session$ns("show_banca")),
               uiOutput(session$ns("show_beneficiar")),
               uiOutput(session$ns("show_contract")),
               uiOutput(session$ns("show_cui_litigiu_nou")),
               uiOutput(session$ns("show_cod_partener_litigiu_nou"))
               
               
               
               
        )) ) )
      
   shinyjs::disable("save_litigiu")   
  })
  
  observeEvent(input$show_more,{
    load("data/bi_contracte.rda")
    
    updateActionButton(session = session,inputId = "show_more")
    
    output$show_banca <- renderUI({req(input$show_more == 1)
      shinyWidgets::pickerInput(session$ns("banca_litigiu_nou"),label = "Select Bank:", width = "400px",
          choices = c("none",unique(bi_contracte$Banca))) })
    
    removeUI(selector = "#litigii_noi_ui_1-show_more")
    
    shinyjs::enable("save_litigiu")
    
    
      
   
    })
  
  observeEvent(input$banca_litigiu_nou,{#req(input$banca_litigiu_nou != "none")
    vals_litigii_noi$bi_contracte_banca_selectata <- bi_contracte %>% dplyr::filter(Banca == input$banca_litigiu_nou)
    
    output$show_beneficiar <- renderUI({req(input$show_more == 1)
      shinyWidgets::pickerInput(session$ns("beneficiar_litigiu_nou"),label = "Select beneficiar:",width = "400px",
         choices =  c("none",unique(vals_litigii_noi$bi_contracte_banca_selectata$NumeBeneficiar))) })
    
    
    output$show_contract   <- renderUI({req(input$show_more == 1)
      shinyWidgets::pickerInput(session$ns("contract_litigiu_nou"),label = "Select contractul de garantare:", width = "400px", 
                  choices =   c(vals_litigii_noi$bi_contracte_banca_selectata %>% 
                            dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou) %>%
                              dplyr::pull(NumarContract) %>% unique(),"none" ))  })
    
    output$show_cui_litigiu_nou <- renderUI({req(input$show_more == 1)
      shinyWidgets::pickerInput(session$ns("cui_beneficiar_litigiu_nou"),label = "CUI-ul beneficiarului:",width = "400px",
                choices = c("none",vals_litigii_noi$bi_contracte_banca_selectata %>% 
                  dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou) %>%
                    dplyr::pull(CUI) %>% unique()))    })
    
    output$show_cod_partener_litigiu_nou <- renderUI({req(input$show_more == 1)
      shinyWidgets::pickerInput(session$ns("cod_partener_beneficiar_nou"),label = "Cod partener:", width = "400px",
                choices = c(vals_litigii_noi$bi_contracte_banca_selectata %>% 
                  dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou) %>%
                  dplyr::pull(CodPartener) %>% unique(),"none"))  })
     
    
  })
  
  
  observeEvent(input$exclude_litigiu,{
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_litigiu_exclus"),title = 'CONFIRM',
                                   btn_colors = c("#bc3c4b","#a5dc86"), type = "success",
                                   text = "Esti sigur ca vrei sa excluzi litigiul?")
    
    observeEvent(input$confirm_litigiu_exclus,{
    vals_litigii_noi$dosare_excluse <- dplyr::bind_rows(vals_litigii_noi$dosar_selectat,
                                                  vals_litigii_noi$dosare_excluse)
    
    vals_litigii_noi$dosare_noi <- vals_litigii_noi$dosare_noi %>% dplyr::filter(numar_dosare_interogare != input$id_dosar)
    removeModal(session = session)
    })
  })            
  
  # Key observer. Every time litigii excluse se actualizeaza ca urmare a apasarii butonului exclude litigiul
  # datele actualizate se salveaza iar userul primeste si mesaj de confirmare
  observeEvent( vals_litigii_noi$dosare_excluse,{req(input$exclude_litigiu)
    dosare_excluse <- isolate( vals_litigii_noi$dosare_excluse)
    usethis::use_data(dosare_excluse,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    shinyWidgets::sendSweetAlert(session,title = "EXCLUDED",text = "Litigiul se gaseste in lista de dosare excluse",type = "success")
  })
  
  observeEvent(input$save_litigiu,{
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_new_litigiu"),title = 'CONFIRM',
                                   btn_colors = c("#bc3c4b","#a5dc86"), type = "success",
                      text = "Esti sigur ca vrei sa salvezi in lista de litigii noi?")
    
    observeEvent(input$confirm_new_litigiu,{
    
        if (as.numeric(input$valoare_litigiu_nou) < 2) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                     text = "Nu pot salva un litigiu nou fara valoare")    }
        
      else {
        vals_litigii_noi$litigiu_nou <- data.frame(
          Nr_crt = as.integer(max(max(litigii_sep$Nr_crt),max(vals$litigii_update$Nr_crt,na.rm = T)) + 1),
          `Nr dosar instanta` = input$numar_dosar_litigiu_nou,
          `Beneficiar_Juridic` = input$beneficiar_litigiu_nou,
          `Nr contract` = input$contract_litigiu_nou,
          CodPartener = input$cod_partener_beneficiar_nou,
          CUI = input$cui_beneficiar_litigiu_nou,
          Banca = input$banca_litigiu_nou,
         `Suma litigiu - echiv lei` = as.numeric(input$valoare_litigiu_nou),
          #`Stadiul procesului` = input$solutie_litigiu_nou,
          update_sentinta = input$solutie_litigiu_nou,
          #`Coeficient provizionare` = as.numeric(input$coef_proviz_litigiu_nou),
          #`Necesar provizion septembrie 2020` = 0,
          update_coef_proviz = as.numeric(input$coef_proviz_litigiu_nou),
          Tip_litigiu = input$tip_litigiu_nou,
          data_actualizare_sentinta = input$data_litigiu_nou,
              stringsAsFactors = FALSE,check.names = FALSE) %>% 
          dplyr::mutate(updated_by = "litigiu_nou_automated")
        
        vals$litigii_update <- dplyr::bind_rows(vals_litigii_noi$litigiu_nou,vals$litigii_update)
        removeModal(session=session)
      }
      
    })
    
    
  })
  
  observeEvent(input$new_litigiu,{
    load("data/bi_contracte.rda")
    
    showModal(modalDialog(title = "New Litigiu", size = "l", footer = list(modalButton("Cancel"),
            actionButton(session$ns("save_new_litigiu_manual"),label = "Save",icon = icon("save"))),
            fluidRow(column(width = 6,
                            
                            textInput(inputId = session$ns("numar_dosar_litigiu_nou_manual"),
                                  label = "Numarul dosarului:", value = ""),
                            
                            shinyWidgets::pickerInput(session$ns("banca_litigiu_nou_manual"),label = "Selecteaza banca:",
                            choices = c("none",unique(bi_contracte$Banca))),
                            
                            shinyWidgets::pickerInput(session$ns("beneficiar_litigiu_nou_manual"),
                                "Selecteaza beneficiarul",selected = "none",choices = c("none")),
                            
                            shinyWidgets::pickerInput(session$ns("contract_litigiu_nou_manual"),
                              "Selecteaza contractul de garantare",   choices = c("none")),
                            
                            shinyWidgets::pickerInput(session$ns("cui_beneficiar_litigiu_nou_manual"),
                                      "CUI-ul beneficiarului:",   choices = c("none")),
                            
                            shinyWidgets::pickerInput(session$ns("cod_partener_beneficiar_litigiu_nou_manual"),
                                                      "Cod partener:",   choices = c("none"))),
                            
                        
                    column(width = 6, 
                        shinyWidgets::numericInputIcon(session$ns("valoare_litigiu_nou_manual"), value = 0,
                          label = "Introdu valoarea litigiului in lei",min = 1,width = "370px",icon = icon("dollar"),
                          help_text = "Litigiul trebuie sa fie mai mare ca zero pentru a putea fi salvat"),
                        
                        shinyWidgets::pickerInput(session$ns("tip_litigiu_nou_manual"),
                                    label = "Selecteaza tipul litigiului",width = "370px",
                                choices = unique(litigii_sep$Tip_litigiu), selected = "somatie"),
                        
                        shinyWidgets::airDatepickerInput(session$ns("data_litigiu_nou_manual"),minDate = as.Date("2020-09-30"),
                              label = "Data dosarului:",value = Sys.Date(), width = "370px"),
                      
                          shinyWidgets::pickerInput(session$ns("coef_proviz_litigiu_nou_manual"),
                                    label = "Selecteaza coeficientul de provizionare",width = "370px",
                                        choices = c(0.65,1,0.25,0)),
                          
                        textAreaInput(inputId = session$ns("solutie_litigiu_nou_manual"),
                            label = "Copiaza solutia dosarului (daca exista) 
                                          in casuta de mai jos:",value = "",height = "110px",width = "370px")
            ))))
  })
  
  
  observeEvent(input$banca_litigiu_nou_manual,{
    vals_litigii_noi$bi_contracte_banca_selectata_litigiu_nou_manual <- bi_contracte %>% 
      dplyr::filter(Banca == input$banca_litigiu_nou_manual)
    
    shinyWidgets::updatePickerInput(session = session, inputId = "beneficiar_litigiu_nou_manual",
        choices = c("none", 
              unique(vals_litigii_noi$bi_contracte_banca_selectata_litigiu_nou_manual$NumeBeneficiar)))
    
    # This works
    #if (input$banca_litigiu_nou_manual == "none")   {
    #  shinyjs::disable(id = "beneficiar_litigiu_nou_manual")  }
    
  #  else if (input$banca_litigiu_nou_manual != "none") {
   #   shinyjs::enable(id = "beneficiar_litigiu_nou_manual")
      
    # Below does not work
      #if (input$beneficiar_litigiu_nou_manual == "none")      {
      #  shinyjs::disable(id = "contract_litigiu_nou_manual")
       # shinyjs::disable(id = "cui_beneficiar_litigiu_nou_manual")
      #  shinyjs::disable(id = "cod_partener_beneficiar_litigiu_nou_manual")   }
      
      #else if (input$beneficiar_litigiu_nou_manual != "none") {
      #  shinyjs::enable(id = "contract_litigiu_nou_manual")
       # shinyjs::enable(id = "cui_beneficiar_litigiu_nou_manual")
       # shinyjs::enable(id = "cod_partener_beneficiar_litigiu_nou_manual")  }
    #  }
    
    })
  
  observeEvent(input$beneficiar_litigiu_nou_manual,{
    shinyWidgets::updatePickerInput(session = session, inputId = "contract_litigiu_nou_manual",
                      choices = c("none", vals_litigii_noi$bi_contracte_banca_selectata_litigiu_nou_manual %>% 
                                    dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou_manual) %>%
                                    dplyr::pull(NumarContract) %>% unique()))
    
    shinyWidgets::updatePickerInput(session = session, inputId = "cui_beneficiar_litigiu_nou_manual",
       choices = c(vals_litigii_noi$bi_contracte_banca_selectata_litigiu_nou_manual %>%
               dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou_manual) %>%
                            dplyr::pull(CUI) %>% unique(),"none"))
    
    shinyWidgets::updatePickerInput(session = session, inputId = "cod_partener_beneficiar_litigiu_nou_manual",
        choices = c(vals_litigii_noi$bi_contracte_banca_selectata_litigiu_nou_manual %>%
              dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou_manual) %>%
                          dplyr::pull(CodPartener) %>% unique(),"none"))
    
    
  })
  
  observeEvent(input$save_new_litigiu_manual,{
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_new_litigiu_manual"),
                  btn_colors = c("#bc3c4b","#a5dc86"),
                  title = "CONFIRM",text = "Esti sigur ca vrei sa salvezi litigiul nou?",type = "success")
    observeEvent(input$confirm_new_litigiu_manual,{
      if (as.numeric(input$valoare_litigiu_nou_manual) < 2) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                     text = "Nu pot salva un litigiu nou fara valoare")
      }
      
      else {
        vals_litigii_noi$litigiu_nou_manual <- data.frame(Nr_crt = as.integer(max(litigii_sep$Nr_crt)+1),
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
        
        vals$litigii_update <- dplyr::bind_rows(vals_litigii_noi$litigiu_nou_manual,vals$litigii_update)
        removeModal(session=session)
      }
      
    })
  })
  
  
  observeEvent(vals$litigii_update,{req(any(input$confirm_new_litigiu_manual,input$confirm_new_litigiu))
    litigii_update <- isolate(vals$litigii_update)
    
    usethis::use_data(litigii_update,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    
    shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",type = "success",
      text = "Lista de litigii a fost actualizata cu succes. Verifica in Sinteza Litigii - Litigii noi")
    
    
  })
  
  
  
  
  
  
  
 
  
}
    
## To be copied in the UI
# mod_litigii_noi_ui("litigii_noi_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_noi_server, "litigii_noi_ui_1")
 
