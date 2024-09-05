grafiken_uebersicht <- read_excel("./Data/metadaten_grafiken_kantonale_Abstimmungen.xlsx")


for (i in 1:nrow(grafiken_uebersicht)) {

metadata_chart <- dw_retrieve_chart_metadata(grafiken_uebersicht$ID[i])

dw_edit_chart(grafiken_uebersicht$ID[i],
              visualize = list("hide-empty-regions" = TRUE))

dw_publish_chart(grafiken_uebersicht$ID[i])
}
