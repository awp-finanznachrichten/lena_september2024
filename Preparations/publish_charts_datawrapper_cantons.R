datawrapper_codes <- as.data.frame(read_excel("Data/metadaten_grafiken_eidgenössische_Abstimmungen.xlsx",
                                              sheet = "Kantone"))

for (i in 1:nrow(datawrapper_codes)) {
dw_publish_chart(datawrapper_codes$ID[i])  
}  