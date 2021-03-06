#' litigii_noi_automate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_litigii_noi_automate_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinydashboard::box(title = "Litigii al caror numar s-a actualizat",width = 12, 
                        footer = "In tabelul de mai sus se gasesc noile numere ale unor litigii in derulare (
                        tipc acestea contin * sau /a)",
                        status = "success",collapsible = T,collapsed = T,
                        
                        DT::dataTableOutput(ns("litigii_noi_updated")),
                        tags$script(src = "datatable_updated.js"),
                        tags$script(paste0("table_updated_module_js('", ns(''), "')"))),
                        
    shinydashboard::box(title = "Litigii noi automatizate (preluate de pe portal.just.ro)",
                        width = 12,status = "success",collapsible = T,collapsed = T,
                        shinyjs::useShinyjs(),
                        tagList(
                          DT::dataTableOutput(ns("tabel_litigii")) ,
                          
                          tags$script(src = "datatable.js"),
                          tags$script(paste0("table_module_js('", ns(''), "')"))
                        ))
    
                        
    
  )
  
}
    
#' litigii_noi_automate Server Function
#'
#' @noRd 
mod_litigii_noi_automate_server <- function(input, output, session, vals){
  ns <- session$ns
 
  load("data/dosare_excluse.rda")
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
      dplyr::filter(!numar_dosare_interogare %in% vals$litigii_curente$`Nr dosar instanta`[
        vals$litigii_curente$updated_by %in% c("litigiu_nou_automated","litigiu_nou_manual")]) 
    
    # Here I store litigii noi care de fapt actualizeaza litigii existente
    vals_litigii_noi$dosare_noi_updated <- vals_litigii_noi$dosare_noi %>%
      dplyr::slice(purrr::map(.x = vals_litigii_noi$dosare_noi$numar_dosare_interogare,
                              # I check to see if numar old dosar se gaseste inside numarul nou
                              ~stringr::str_which(string = .x,
                                                  pattern = vals$litigii_curente$`Nr dosar instanta`)) %>%
                     purrr::map_dbl(.x = .,~purrr::detect_index(.x,~length(.x) != 0)) %>% which(x = .>0)) %>%
      # Elimin dosarele care au fost deja actualizate
      dplyr::filter(!numar_dosare_interogare %in% 
                      vals$litigii_curente$`Nr dosar instanta`[vals$litigii_curente$updated_by == "update_litigiu"]) %>%
      # Elimin dosarele care au fost marcate ca noi si nu ca update
      dplyr::filter(!numar_dosare_interogare %in% vals_litigii_noi$litigiu_nou_from_updates$numar_dosare_interogare) %>%
      
      # One time filter - to be eliminated in the future. Primul dosar s-a actualizat iar al doilea a fost introdus manual in sold inghetat
      dplyr::filter(!numar_dosare_interogare %in% c("29061/299/2019*","32373/299/2020"))
    
    vals_litigii_noi$dosare_noi_final <- vals_litigii_noi$dosare_noi %>% dplyr::filter(!numar_dosare_interogare %in% 
                                                                                         vals_litigii_noi$dosare_noi_updated$numar_dosare_interogare) %>%
      dplyr::filter(!numar_dosare_interogare %in% 
                      vals$litigii_curente$`Nr dosar instanta`[vals$litigii_curente$updated_by == "update_litigiu"]) %>%
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
  
  output$litigii_noi_excluse <-  DT::renderDataTable({
    DT::datatable(
      rownames = FALSE,
      class = "nowrap",
      data = dosare_excluse %>% dplyr::select(1, 5, 7:8, 2:3, 6, 4, 9),
      options = list(dom = "ftp", scrollX = TRUE),
      caption = "Litigiile noi ale FNGCIMM excluse din lista de litigii provizionate"
    )
  })
  
  output$litigii_noi_updated <- DT::renderDataTable({
    DT::datatable(
      rownames = FALSE,
      class = "nowrap",
      data = cbind(
        tibble::tibble(" " = vals_litigii_noi$updated_actions),
        vals_litigii_noi$dosare_noi_updated
      ) #%>%
      # dplyr::select(1:2,8,6,9,10,7,3:5)
      ,
      options = list(dom = "ftp", scrollX = TRUE),
      escape = 0,
      caption = "Litigii noi aflate deja in lista soldurilor inghetate"
    )
  })
  
  
  output$tabel_litigii <- DT::renderDataTable({
    DT::datatable(
      rownames = FALSE,
      escape = FALSE,
      caption = "Litigii noi ale FNGCIMM dupa 30 septembrie",
      data = cbind(
        tibble::tibble(" " = vals_litigii_noi$actions),
        vals_litigii_noi$dosare_noi_final %>% dplyr::select(1, 6, 7, 5, 2:4)
      ),
      extensions = c('Select', 'SearchPanes'),
      selection = "none",
      class = "nowrap",
      options = list(
        pageLength = 5,
        dom = "Pftp",
        scrollX = TRUE,
        columnDefs = list(
          list(
            searchPanes = list(show = FALSE),
            targets = c(0:2, 5:7)
          ),
          list(
            searchPanes = list(controls = FALSE),
            targets = 3
          )
        )
      )
    )
  }, server = F)
  
  # Observer for button click inside table Litigii noi care actualizeaza litigii existente
  observeEvent(input$id_dosar_updated,{
    
    vals_litigii_noi$dosar_updated_selectat <- vals_litigii_noi$dosare_noi_updated %>% 
      dplyr::filter(numar_dosare_interogare == input$id_dosar_updated)
    
    
    vals_litigii_noi$litigii_inghetat_selectat <- vals$litigii_curente %>% 
      dplyr::slice( stringr::str_which(pattern = vals$litigii_curente$`Nr dosar instanta`,
                                       string = input$id_dosar_updated))
    showModal(modalDialog(
      title = "Update Litigiu", size = "l",
      footer = fillRow(flex = c(1,NA),
                       tagList(
                         actionButton(session$ns("litigiu_nou_update"),"Acesta este un litigiu nou", icon=icon("plus-square")),
                         actionButton(session$ns("litigiu_exclus_update"),"Exclude acest litigiu",icon = icon("minus-circle"))),
                       br(),tagList(modalButton('Cancel'),
                                    actionButton(session$ns('update_litigiu'), 'Update litigiu', class = "btn btn-primary", icon = icon("pen-square")))),
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
                            value = vals_litigii_noi$litigii_inghetat_selectat$`Coeficient provizionare`),
               selectInput(inputId = session$ns("tip_update_litigiu_update"),label = "Tip update existent",
                           choices = c(NA_character_,unique(vals$litigii_curente$updated_by)), width = "400px",
                           selected = vals_litigii_noi$litigii_inghetat_selectat$updated_by)),
        
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
  
  
  # Observer for clik Update litigiu inside modal from above observer
  observeEvent(input$update_litigiu,{
    
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_litigiu_update"),title = "CONFIRM",type = "warning",
                                   text = "Esti sigur ca vrei sa actualizez litigiul cu datele furnizate?",
                                   btn_colors = c("#bc3c4b","#a5dc86"))
  })
  
  
  observeEvent(input$confirm_litigiu_update,{
    
    
    vals_litigii_noi$litigiu_de_adaugat <- vals_litigii_noi$litigii_inghetat_selectat %>% 
      dplyr::mutate(`Nr dosar instanta` = input$numar_litigiu_nou,
                    update_coef_proviz = as.numeric(input$coef_proviz_updated),
                    update_sentinta = ifelse(is.null(input$ultima_solutie_actualizata),NA,input$ultima_solutie_actualizata),
                    data_actualizare_sentinta = ifelse(is.null(input$data_pronuntare_updated),NA,input$data_pronuntare_updated),
                    updated_by = "update_litigiu") %>% 
      dplyr::mutate(dplyr::across(.cols = dplyr::contains("data"),~as.Date(.x,origin = "1970-01-01")))
    
    if (janitor::compare_df_cols_same(vals$litigii_curente,
                                      vals_litigii_noi$litigiu_de_adaugat, bind_method = "bind_rows",verbose = FALSE)) {
      
      vals$litigii_curente <-   dplyr::bind_rows(vals_litigii_noi$litigiu_de_adaugat,
                                                 vals$litigii_curente %>% dplyr::slice(-stringr::str_which(pattern = vals$litigii_curente$`Nr dosar instanta`,
                                                                                                           string = input$id_dosar_updated)))
      removeModal(session = session) }
    else {
      shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                   text = paste0("Nu am actualizat litigiul. Probleme de tipul ",
                                                 janitor::compare_df_cols_same(vals$litigii_curente,
                                                                               vals_litigii_noi$litigiu_de_adaugat, bind_method = "bind_rows",verbose = TRUE)    ))
    }
  })
  
  
  
  
  
  # Observer for button acesta este un litigiu nou din modal update litigii. Trimite litigiul in lista de litigii noi
  observeEvent(input$litigiu_nou_update,{
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_litigiu_nou_from_updates"),title = "CONFIRM",
                                   text = "Esti sigur ca vrei sa consider acest litigiu ca unul nou-nout?",
                                   btn_colors = c("#bc3c4b","#a5dc86"), type = "info")
  })
    observeEvent(input$confirm_litigiu_nou_from_updates,{
      
      vals_litigii_noi$litigiu_nou_from_updates <- dosare_noi %>% dplyr::filter(numar_dosare_interogare == input$id_dosar_updated) %>%
        dplyr::bind_rows(litigiu_nou_from_updates %>% dplyr::filter(numar_dosare_interogare != input$id_dosar_updated))
      
      litigiu_nou_from_updates <- isolate(vals_litigii_noi$litigiu_nou_from_updates)
      
      usethis::use_data(litigiu_nou_from_updates,internal = FALSE,overwrite = TRUE,version = 3,compress = "gzip")
      
      removeModal(session = session)
      
      shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",type = "success",
                                   text = "Litigiul se gaseste acum in lista de litigii noi de mai sus")
    })
  
  
  # Observer for click inside tabel litigii noi
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
          
          dateInput(inputId = session$ns("data_litigiu_nou"),label = "Data dosarului", width = "400px",
                    value = dosare_noi$prima_data_dosar_interogare[dosare_noi$numar_dosare_interogare == input$id_dosar]),
          
          shinyWidgets::numericInputIcon(session$ns("valoare_litigiu_nou"), value = 0,
                                         label = "Introdu valoarea litigiului in lei",min = 1,width = "400px",icon = icon("dollar"),
                                         help_text = "Litigiul trebuie sa fie mai mare ca zero pentru a putea fi salvat"),
          selectInput(session$ns("coef_proviz_litigiu_nou"),
                                    label = "Selecteaza coeficientul de provizionare",width = "400px",
                                    choices = c(0.65,1,0.25,0)),
          
          textAreaInput(
            session$ns("solutie_litigiu_nou"),
            label = "Solutie dosar",
            width = "400px",
            height = "150px",
            value = dosare_noi$solutie_sumar_interogare[dosare_noi$numar_dosare_interogare == input$id_dosar]),
          
          textAreaInput(session$ns("parti_dosar"),label = "Parti dosar",width = "400px",height="150px",
                        value = dosare_noi$parte_dosar[dosare_noi$numar_dosare_interogare == input$id_dosar])),
        
        column(width = 6,
               selectInput(session$ns("tip_litigiu_nou"),
                                         label = "Selecteaza tipul litigiului",width = "400px",
                                         choices = unique(vals$litigii_curente$Tip_litigiu), 
                          selected = ifelse(stringr::str_detect(string = dosare_noi$ultima_categorie_caz_interogare[
                             dosare_noi$numar_dosare_interogare == input$id_dosar],pattern = "preten"),"pretentii","somatie")),
               
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
  
  # Observer related to the above modal edit litigiu nou
  observeEvent(input$show_more,{
    load("data/bi_contracte.rda")
    
    removeUI("#litigii_noi_automate_ui_1-show_more")
    
    updateActionButton(session = session,inputId = "show_more")
    
    output$show_banca <- renderUI({req(input$show_more == 1)
      banci_bi_contracte <- unique(bi_contracte$Banca)
      
      selectInput(inputId = session$ns("banca_litigiu_nou"),label = "Select Bank:", width = "400px",
                  choices = banci_bi_contracte,
                  selected = ifelse(stringr::str_detect(string = dosare_noi$parte_dosar[
                    dosare_noi$numar_dosare_interogare == input$id_dosar],pattern = banci_bi_contracte),
                    banci_bi_contracte[stringr::str_which(pattern = banci_bi_contracte,
                      string = dosare_noi$parte_dosar[dosare_noi$numar_dosare_interogare == input$id_dosar])],
                    "none"))   })
    
    removeUI(selector = "#litigii_noi_ui_1-show_more")
    
    shinyjs::enable("save_litigiu")
    
    
    
    
  })
  
  # Observer related to the above modal edit litigiu nou
  observeEvent(input$banca_litigiu_nou,{req(input$banca_litigiu_nou != "none")
    #vals_litigii_noi$bi_contracte_banca_selectata <- bi_contracte %>% dplyr::filter(Banca == input$banca_litigiu_nou)
    
    
    output$show_beneficiar <- renderUI({req(input$banca_litigiu_nou != "none", input$show_more==1)
      
        selectizeInput(session$ns("beneficiar_litigiu_nou"),label = "Select beneficiar:", width = "400px",
                          choices =  "none")
          
        })
    
    updateSelectizeInput(session = session, inputId = "beneficiar_litigiu_nou",server = TRUE,
                         choices = c("none",unique(bi_contracte$NumeBeneficiar)))
    
    
    output$show_contract   <- renderUI({req(input$banca_litigiu_nou != "none",input$show_more == 1, 
                                            input$beneficiar_litigiu_nou)
      
      selectizeInput(session$ns("contract_litigiu_nou"),label = "Select contractul de garantare:", width = "400px", 
                  choices =   "none" )  })
      
    
    
    output$show_cui_litigiu_nou <- renderUI({req(input$banca_litigiu_nou != "none", input$show_more == 1,
                                                 input$beneficiar_litigiu_nou)
      selectizeInput(session$ns("cui_beneficiar_litigiu_nou"),label = "CUI-ul beneficiarului:",width = "400px",
                  choices = "none")    })
    
    
    output$show_cod_partener_litigiu_nou <- renderUI({req(input$banca_litigiu_nou != "none",input$show_more == 1,
                                                          input$beneficiar_litigiu_nou)
      selectizeInput(session$ns("cod_partener_beneficiar_nou"),label = "Cod partener:", width = "400px",
                                choices = "none")  })
    
    
  })
  
  observeEvent(input$beneficiar_litigiu_nou ,{req(input$beneficiar_litigiu_nou != "none")
    
    vals_litigii_noi$bi_contracte_beneficiar_selectat <- bi_contracte %>% 
                          dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou)
    
      updateSelectizeInput(session = session, inputId = "contract_litigiu_nou",server = TRUE,
                           choices =  c("none",vals_litigii_noi$bi_contracte_beneficiar_selectat %>%
                             dplyr::pull(NumarContract) %>% unique()))
      
      updateSelectizeInput(session = session, inputId = "cui_beneficiar_litigiu_nou",server = TRUE,
                           choices = vals_litigii_noi$bi_contracte_beneficiar_selectat %>%
                             dplyr::pull(CUI) %>% unique())
      
      updateSelectizeInput(session = session, inputId = "cod_partener_beneficiar_nou",server = TRUE,
                           choices = vals_litigii_noi$bi_contracte_beneficiar_selectat %>%
                             dplyr::pull(CodPartener) %>% unique())
    
    })
  
  #observeEvent(input$show_more_beneficiari,{
    
   # vals_litigii_noi$bi_contracte_banca_selectata <- bi_contracte 
 # })
  
  
  
  
  # Observer for button excude litigiul inside modal for litigiu nou
  observeEvent(input$exclude_litigiu,{
    
    
    #confirmation_func(id_ok = 'confirm_litigiu_exclus',session=session,text = "Esti sigur ca doresti sa excluzi litigiul?")
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_litigiu_exclus"),title = 'CONFIRM',
                                   btn_colors = c("#bc3c4b","#a5dc86"), type = "success",
                                   text = "Esti sigur ca vrei sa excluzi litigiul?")
  })
  
  observeEvent(input$confirm_litigiu_exclus,{
    vals_litigii_noi$dosare_excluse <- dplyr::bind_rows(vals_litigii_noi$dosar_selectat,
                                                        vals_litigii_noi$dosare_excluse)
    
    vals_litigii_noi$dosare_noi <- vals_litigii_noi$dosare_noi %>% dplyr::filter(numar_dosare_interogare != input$id_dosar)
    removeModal(session = session)
  })
  
  
  # Key observer. Every time litigii excluse se actualizeaza ca urmare a apasarii butonului exclude litigiul
  # datele actualizate se salveaza iar userul primeste si mesaj de confirmare
  observeEvent( vals_litigii_noi$dosare_excluse,{req(input$exclude_litigiu)
    dosare_excluse <- isolate( vals_litigii_noi$dosare_excluse)
    usethis::use_data(dosare_excluse,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    shinyWidgets::sendSweetAlert(session,title = "EXCLUDED",text = "Litigiul se gaseste in lista de dosare excluse",type = "success")
  })
  
  observeEvent(input$save_litigiu,{
    
    #confirmation_func(id_ok = 'confirm_new_litigiu',session=session,text = "Esti sigur ca doresti sa salvezi litigiul nou?")
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_new_litigiu"),title = 'CONFIRM',
                                   btn_colors = c("#bc3c4b","#a5dc86"), type = "success",
                                   text = "Esti sigur ca vrei sa salvezi litigiul ca unul nou?")
  })  
  observeEvent(input$confirm_new_litigiu,{
    
    if (as.numeric(input$valoare_litigiu_nou) < 2) {
      shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                   text = "Nu pot salva un litigiu nou fara valoare")    }
    
    else {
      vals_litigii_noi$litigiu_nou <- data.frame(
        Nr_crt = as.integer(max(vals$litigii_curente$Nr_crt,na.rm = TRUE) + 1),
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
      
      vals$litigii_curente <- dplyr::bind_rows(vals_litigii_noi$litigiu_nou,vals$litigii_curente)
      
      updateActionButton(session = session, inputId = 'confirm_new_litigiu')
      updateActionButton(session = session, inputId = 'save_litigiu')
      removeModal(session=session)
      
    }
    
  })
  
  
}
    
## To be copied in the UI
# mod_litigii_noi_automate_ui("litigii_noi_automate_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_noi_automate_server, "litigii_noi_automate_ui_1")
 
