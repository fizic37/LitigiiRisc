confirmation_func <- function(session,id_ok,text="") {
  showModal(session = session,modalDialog(title = "CONFIRM",size = "m",
                      text,
                      footer = tagList(modalButton("Cancel"),
                            actionButton(inputId = session$ns(id_ok),label = "OK",icon = icon("thumbs-up")))))
}