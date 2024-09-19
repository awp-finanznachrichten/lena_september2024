manual_chart_ids <- c("UD88Z", "jIuty", #FR_TOP
"AXIvL", "NnKcU", #FR_FLOP
"A2Btp","t59uF", #DE_TOP
"UIUHi", "rx9of", #DE_FLOP
"hbDxD", "wQM0t", #IT_TOP,
"2yfQU", "vU4c2") #IT_FLOP 

manual_chart_ids <- c("UrcQx","hGSCa",
                      "uXVyB", "C0P0y",
                      "Poiov", "KRcBk",
                      "EsMoB","wVBoH")


manual_chart_summary <- data.frame("",
                                   "",
                                   "",
                                   "",
                                   "",
                                   "",
                                   "",
                                   "")

manual_chart_summary <- data.frame(matrix(ncol = length(manual_chart_summary), nrow = 0))

colnames(manual_chart_summary) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")


#°retrive_test <- dw_retrieve_chart_metadata("rOTIf")

#retrive_test$content$title

for (chart_id in manual_chart_ids) {
  
  # Récupérer les métadonnées du graphique
  metadata_chart <- DatawRappr::dw_retrieve_chart_metadata(chart_id)
  
  # Créer une nouvelle entrée de métadonnées
  new_entry <- data.frame(
    "Typ" = "Top10",
    "Vorlage" = "alle",
    "Titel" = metadata_chart$content$title,
    "Sprache" = metadata_chart$content$language,
    "ID" = metadata_chart$id,
    "Link" = metadata_chart$content$publicUrl,
    "Iframe" = metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
    "Script" = metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`
  )
  
  # Ajouter la nouvelle entrée au tableau récapitulatif
  manual_chart_summary <- rbind(manual_chart_summary, new_entry)
}

writexl::write_xlsx(manual_chart_summary, "C:/Users/yove/Documents/R/selfpick/data-raw/resources/vot_fed_09_2024/sch_be_ju.xlsx")
