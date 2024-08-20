for (i in 1:nrow(grafiken_uebersicht)) {

metadata_chart <- dw_retrieve_chart_metadata(grafiken_uebersicht$ID[i])

dw_edit_chart(grafiken_uebersicht$ID[i],
              data=list("column-format"=right_column_format),
              visualize = list("mapView" = "crop"))

dw_publish_chart(grafiken_uebersicht$ID[i])
}