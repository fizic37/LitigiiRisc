function table_module_js(ns_prefix) {
  
  $("#" + ns_prefix + "tabel_litigii").on("click", ".edit_btn", function() {
    Shiny.setInputValue(ns_prefix + "id_dosar", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

}




