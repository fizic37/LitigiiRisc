function table_updated_module_js(ns_prefix) {
  
  $("#" + ns_prefix + "litigii_noi_updated").on("click", ".edit_btn", function() {
    Shiny.setInputValue(ns_prefix + "id_dosar_updated", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
}