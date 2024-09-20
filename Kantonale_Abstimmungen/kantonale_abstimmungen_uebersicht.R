kantone_list <- json_data_kantone[["kantone"]]

mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM output_overview WHERE date = '",voting_date,"' AND voting_type = 'cantonal' AND mail_results = 'done'" ))
output_overview <- DBI::fetch(rs,n=-1)
dbDisconnectAll()
completed_cantons <- unique(output_overview$area_ID)

for (k in 1:nrow(kantone_list)) {

if (sum(grepl(kantone_list$geoLevelname[k],completed_cantons)) == 0) {
  
data_overview <- data.frame(50,50,"Abstimmung_de","Abstimmung_fr","Abstimmung_it")
colnames(data_overview) <- c("Ja","Nein","Abstimmung_de","Abstimmung_fr","Abstimmung_it")  
vorlagen_kantonal <- kantone_list$vorlagen[[k]]

check_counted <- c() 

for (i in 1:nrow(vorlagen_kantonal)) {
check_counted[i] <- FALSE
results <- get_results_kantonal(json_data_kantone,
                                  k,
                                  i)

results <- treat_gemeinden(results)
results <- format_data_g(results)

Ja_Stimmen_Kanton <- get_results_kantonal(json_data_kantone,
                                          k,
                                          i,
                                          "kantonal")
 
#Titel aus Metadata
titel_all <- votes_metadata %>%
  filter(area_ID == kantone_list$geoLevelname[k],
         votes_ID == vorlagen_kantonal$vorlagenId[i])

#Eintrag für Uebersicht
uebersicht_text_de <- paste0("<b>",titel_all$title_de[1],"</b><br>",
                             "Es sind noch keine Gemeinden ausgezählt.")

uebersicht_text_fr <- paste0("<b>",titel_all$title_fr[1],"</b><br>",
                             "Aucun résultat n'est encore connu.")

uebersicht_text_it <- paste0("<b>",titel_all$title_it[1],"</b><br>",
                             "Nessun risultato è ancora noto.")
Ja_Anteil <- 50
Nein_Anteil <- 50

if (is.na(Ja_Stimmen_Kanton) == FALSE) {
  uebersicht_text_de <- paste0("<b>",titel_all$title_de[1],"</b><br>",
                               "Die brieflichen Stimmen sind ausgezählt.")
  
  uebersicht_text_fr <- paste0("<b>",titel_all$title_fr[1],"</b><br>",
                               "Les votes par correspondance ont été dépouillés.")
  
  uebersicht_text_it <- paste0("<b>",titel_all$title_it[1],"</b><br>",
                               "I voti per corrispondenza sono stati scrutinati.")
if (sum(results$Gebiet_Ausgezaehlt) > 0 ) {  
  
  uebersicht_text_de <- paste0("<b>",titel_all$title_de[1],"</b><br>",
                               sum(results$Gebiet_Ausgezaehlt)," von ",nrow(results)," Gemeinden ausgezählt (",
                               round2((sum(results$Gebiet_Ausgezaehlt)*100)/nrow(results),1),
                               "%)")
  
  uebersicht_text_fr <- paste0("<b>",titel_all$title_fr[1],"</b><br>",
                               sum(results$Gebiet_Ausgezaehlt)," des ",nrow(results)," communes sont connus (",
                               round2((sum(results$Gebiet_Ausgezaehlt)*100)/nrow(results),1),
                               "%)")
  
  uebersicht_text_it <- paste0("<b>",titel_all$title_it[1],"</b><br>",
                               sum(results$Gebiet_Ausgezaehlt)," dei ",nrow(results)," comuni sono noti (",
                               round2((sum(results$Gebiet_Ausgezaehlt)*100)/nrow(results),1),
                               "%)")

  if (sum(results$Gebiet_Ausgezaehlt) == nrow(results)) {
    uebersicht_text_de <- paste0("<b>",titel_all$title_de[1],"</b>")
    
    uebersicht_text_fr <- paste0("<b>",titel_all$title_fr[1],"</b>")
    
    uebersicht_text_it <- paste0("<b>",titel_all$title_it[1],"</b>")
    
    cat(paste0("Resultate von folgender kantonalen Abstimmung aus ",kantone_list$geoLevelname[k]," sind komplett:\n",
                 titel_all$title_de[1],"\n",
                 titel_all$title_fr[1],"\n",
                 titel_all$title_it[1],"\n\n"))
    check_counted[i] <- TRUE
  }  
}
  Ja_Anteil <- round2(Ja_Stimmen_Kanton,1)
  Nein_Anteil <- round2(100-Ja_Stimmen_Kanton,1)
}  

entry_overview <- data.frame(Ja_Anteil,Nein_Anteil,uebersicht_text_de,uebersicht_text_fr,uebersicht_text_it)
colnames(entry_overview) <- c("Ja","Nein","Abstimmung_de","Abstimmung_fr","Abstimmung_it")
data_overview <- rbind(data_overview,entry_overview)
}
data_overview <- data_overview[-1,]
write.csv(data_overview,paste0("Output_Overviews/Uebersicht_dw_",kantone_list$geoLevelname[k],".csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")

#Update Datawrapper-Chart
datawrapper_ids <- datawrapper_codes_kantonal %>%
  filter(Typ == "Uebersicht Kanton",
         Vorlage == kantone_list$geoLevelname[k])

for (d in 1:nrow(datawrapper_ids)) {
dw_data_to_chart(data_overview,datawrapper_ids$ID[d])

if (datawrapper_ids$Sprache[d] == "de-DE") {
if (sum(results$Gebiet_Ausgezaehlt) == nrow(results)) {  
dw_edit_chart(datawrapper_ids$ID[d],intro="")   
  } else {
dw_edit_chart(datawrapper_ids$ID[d],intro=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%H:%M Uhr")))    
}    
}
if (datawrapper_ids$Sprache[d] == "fr-CH") {
  if (sum(results$Gebiet_Ausgezaehlt) == nrow(results)) {  
    dw_edit_chart(datawrapper_ids$ID[d],intro="")   
  } else {
dw_edit_chart(datawrapper_ids$ID[d],intro=paste0("Dernière mise à jour: ",format(Sys.time(),"%Hh%M")))
  }  
}
if (datawrapper_ids$Sprache[d] == "it-CH") {
  if (sum(results$Gebiet_Ausgezaehlt) == nrow(results)) {  
    dw_edit_chart(datawrapper_ids$ID[d],intro="")   
  } else {
dw_edit_chart(datawrapper_ids$ID[d],intro=paste0("Ultimo aggiornamento: ",format(Sys.time(),"%H:%M")))
  }  
}    
dw_publish_chart(datawrapper_ids$ID[d])
}

if (sum(check_counted) == nrow(vorlagen_kantonal)) {
cat(paste0("Alle Abstimmungen aus dem Kanton ",kantone_list$geoLevelname[k]," sind ausgezählt!\n\n")) 

#Set mail output to done
mydb <- connectDB(db_name = "sda_votes")  
sql_qry <- paste0("UPDATE output_overview SET mail_results = 'done' WHERE date = '",voting_date,"' AND voting_type = 'cantonal' AND area_ID = '",kantone_list$geoLevelname[k],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 

#Send Mail
selected_mail <- mail_cantons %>%
  filter(area_ID == kantone_list$geoLevelname[k])

Subject <- paste0("Kanton ",kantone_list$geoLevelname[k],": Kantonale Abstimmungen komplett")
Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
               "Die Ergebnisse der kantonalen Abstimmungen im Kanton ",kantone_list$geoLevelname[k]," sind bekannt. ",
               "Ihr findet die Übersichts-Grafiken unter folgenden Links:\n",
               paste(paste0("https://datawrapper.dwcdn.net/",datawrapper_ids$ID),collapse = "\n"),
               "\n\nBitte falls gewünscht die Übersichtsgrafik sowie die Karten (falls vorhanden) ins Visual hochladen.\n\n",
               "Liebe Grüsse\n\nLENA")
send_notification(Subject,Body,
                  paste0(DEFAULT_MAILS,",",selected_mail$mail_KeySDA[1]))

}
}  else {
  cat(paste0("\nKanton ",kantone_list$geoLevelname[k]," bereits komplett\n"))  
}  
}  

