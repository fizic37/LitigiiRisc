#' actualizare_manuala UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_actualizare_manuala_ui <- function(id){
  ns <- NS(id)
  
  tagList(tags$style('#actualizare_manuala_ui_1-welcome {color: #000;'),
    shinyjs::useShinyjs(),
          shinybusy::add_busy_spinner(color = "#bc3c4b", position = "bottom-right", timeout = 200),
                           
               textOutput(ns("welcome")), hr(),
                                               
               DT::dataTableOutput(ns("tabel_litigii")),
                                               
               tags$script(src = "datatable.js"),
               tags$script(paste0("table_module_js('", ns(''), "')")),
                                               
               DT::dataTableOutput(ns("sinteza_sold_litigii")))
  
}
    
#' actualizare_manuala Server Function
#'
#' @noRd 
mod_actualizare_manuala_server <- function(input, output, session){
  ns <- session$ns
 
  load("data/litigii_sep.rda")
  load("data/litigii_update.rda")
  
  
  vals <- reactiveValues(litigii_update = litigii_update)
  
  output$welcome <- renderText({paste("Clic pe butonul edit din tabelul de mai jos pentru 
                      actualizarea manuala a litigiilor in derulare la 30 sep 2020.")})
  
  
  
  actions <- purrr::map_chr(litigii_sep$Nr_crt, function(id_) {
    paste0(
      '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', 
      id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          </div>'
    )
  })
  
  
  output$tabel_litigii <- DT::renderDataTable({  DT::datatable( class = "nowrap",
        escape = FALSE,   rownames = TRUE,    extensions = "Buttons",
        data = cbind(tibble::tibble(" " = actions), litigii_sep) %>% dplyr::select(-Nr_crt),
        caption = "Sold litigii la 30 septembrie 2020:",
        options = list(scrollX = TRUE,  dom = "Bftp",  buttons = c("excel", "csv"), pageLength = 8)  )  })
  
 
  observeEvent(input$id_dosar, {
    removeUI("#litigii_ui_1-welcome")
    showModal(modalDialog(
      title = "Editeaza litigiul",
      size = "l",
      footer = list(
        modalButton('Cancel'),
        actionButton(session$ns('submit'), 'Submit', class = "btn btn-primary")),
      
      fluidRow(
        column(
          width = 6,
          textInput(
            session$ns("edit_dosar"),
            label = "Numar dosar instanta",
            width = "400px",
            value = litigii_sep$`Nr dosar instanta`[litigii_sep$Nr_crt ==
                                                      as.numeric(input$id_dosar)]),
          textAreaInput(
            session$ns("edit_solutie_dosar"),
            label = "Solutie dosar",
            width = "400px",
            height = "150px",
            value = litigii_sep$`Stadiul procesului`[litigii_sep$Nr_crt ==
                                                       as.numeric(input$id_dosar)])   ),
        column(width = 6,
          numericInput(
            inputId = session$ns("edit_valoare_litigiu"),
            label = "Valoarea litigiului",
            value = litigii_sep$`Suma litigiu - echiv lei`[litigii_sep$Nr_crt ==
                                                             as.numeric(input$id_dosar)] ),
          selectInput(
            inputId = session$ns("edit_coef_proviz"),
            label = "Coeficientul de provizionare",
            choices = c(0.65, 1, 0.25, 0),
            multiple = FALSE,
            selected = litigii_sep$`Coeficient provizionare`[litigii_sep$Nr_crt == as.numeric(input$id_dosar)] ),
          
          div(id = "select_action_div",
            selectInput(inputId = session$ns("select_action"),
              label = "Select action",
              choices = c("none", "UpdateSentinta", "Platit")
            )  ) ) ) ) )
    
    shinyjs::disable("edit_dosar")
    shinyjs::disable("edit_solutie_dosar")
    shinyjs::disable("edit_valoare_litigiu")
    shinyjs::disable("edit_coef_proviz")
    shinyjs::disable("submit")
  }) 
  
  observeEvent(input$select_action,{req(input$select_action)
    
    if (input$select_action == "Platit"){
      removeUI(selector = "#litigii_ui_1-update_sentinta_select_action")
      shinyjs::enable("submit")
      insertUI(selector = "#select_action_div",where = "afterEnd",
               ui = div(id = session$ns("plati_select_action"),
                        dateInput(label = "Selecteaza data platii litigiului",value = Sys.Date(),
                                  inputId = session$ns("data_plata_litigiu")),
                        selectInput(inputId = session$ns("update_coef_proviz"),label = "Noul coeficient de provizionare",
                                    choices = c(0,1,0.25,0.65)))  )    } 
    
    else if (input$select_action == "UpdateSentinta") {
      removeUI(selector = "#litigii_ui_1-plati_select_action")
      shinyjs::enable("submit")
      insertUI(selector = "#select_action_div", where = "afterEnd",
               div(id = session$ns("update_sentinta_select_action"),
                   textAreaInput(session$ns("update_sentinta"),label = "Sentinta actualizata",placeholder = "copiaza aici sentinta"),
                   selectInput(inputId = session$ns("update_coef_proviz"),label = "Noul coeficient de provizionare",
                               choices = c(1,0,0.25,0.65)),
                   dateInput(inputId = session$ns("data_actualizare_sentinta"),value = Sys.Date(),min = as.Date("2020-09-30"),
                             label = "Data actualizarii sentintei")))
    }
    
    else {
      shinyjs::disable("submit")
      removeUI(selector = "#litigii_ui_1-update_sentinta_select_action")
      removeUI(selector = "#litigii_ui_1-plati_select_action")
    }
  })
  
  observeEvent(input$submit,{
    removeModal(session = session)
    removeUI("#litigii_ui_1-result_select_action")
    
    if (input$select_action == "Platit" & 
        as.numeric(input$id_dosar) %in% vals$litigii_update$Nr_crt[!is.na(vals$litigii_update$data_plata)] ) {
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_owerwrite_plati"),session=session,
                                     title = "Atentie",text = "Am updatat deja contractul platit. Vrei sa scriu peste?") }
    else if (input$select_action == "UpdateSentinta" & 
             as.numeric(input$id_dosar) %in% vals$litigii_update$Nr_crt[!is.na(vals$litigii_update$update_sentinta)] ) {
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_owerwrite_plati"),session=session,
            title = "Atentie",text= "Am updatat deja contractul cu sentinta noua. Vrei sa scriu peste?") }
    else {
      vals$litigii_update <- dplyr::bind_rows(litigii_update, litigii_sep[litigii_sep$Nr_crt==as.numeric(input$id_dosar),] %>% 
                              dplyr::mutate(data_plata = input$data_plata_litigiu,
                                     update_coef_proviz = as.numeric(input$update_coef_proviz),
                                     update_sentinta = input$update_sentinta,updated_by = "manual"))
    }
    
  })
  
  observe({req(input$confirm_owerwrite_plati == TRUE)
    vals$litigii_update <- dplyr::bind_rows(litigii_update %>% dplyr::filter(Nr_crt != as.numeric(input$id_dosar)),
            litigii_sep[litigii_sep$Nr_crt==as.numeric(input$id_dosar),] %>% 
        dplyr::mutate(data_plata = input$data_plata_litigiu,
                  update_coef_proviz = as.numeric(input$update_coef_proviz),
                    update_sentinta = input$update_sentinta,updated_by = "manual"))
  })
  
  observeEvent(vals$litigii_update,{req(input$submit)
    litigii_update <- isolate(vals$litigii_update)
    
    usethis::use_data(litigii_update,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    
  })
  
}
    
## To be copied in the UI
# mod_actualizare_manuala_ui("actualizare_manuala_ui_1")
    
## To be copied in the server
# callModule(mod_actualizare_manuala_server, "actualizare_manuala_ui_1")
 
