mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM votes_metadata WHERE date = '",voting_date,"' AND area_ID != 'CH' AND status = 'done'" ))
votes_metadata_cantons <- DBI::fetch(rs,n=-1)
dbDisconnectAll()
completed_votes <- na.omit(unique(votes_metadata_cantons$spreadsheet))

for (k in 1:length(kantonal_short) ) {
  
  if (sum(grepl(kantonal_short[k],completed_votes)) == 0) {

  cat(paste0("\nErmittle Daten für folgende Vorlage: ",kantonal_short[k],"\n"))
  
  results <- get_results_kantonal(json_data_kantone,
                                  kantonal_number[k],
                                  kantonal_add[k])

  #Daten anpassen Gemeinden
  results <- treat_gemeinden(results)
  results <- format_data_g(results)
  
  #Kantonsergebnis hinzufügen
  Ja_Stimmen_Kanton <- get_results_kantonal(json_data_kantone,
                                            kantonal_number[k],
                                            kantonal_add[k],
                                            "kantonal")
  
  results$Ja_Stimmen_In_Prozent_Kanton <- Ja_Stimmen_Kanton
  
  #Wie viele Gemeinden sind ausgezählt?
  cat(paste0(sum(results$Gebiet_Ausgezaehlt)," Gemeinden sind ausgezählt.\n"))
  
  #Neue Variablen
  results <- results %>%
    mutate(Ja_Nein = NA,
           Oui_Non = NA,
           Nein_Stimmen_In_Prozent = NA,
           Unentschieden = NA,
           Einstimmig_Ja = NA,
           Einstimmig_Nein = NA,
           kleine_Gemeinde = NA,
           Highest_Yes_Kant = FALSE,
           Highest_No_Kant = FALSE,
           Storyboard = NA,
           Text_d = "Die Resultate dieser Gemeinde sind noch nicht bekannt.",
           Text_f = "Les résultats ne sont pas encore connus dans cette commune.",
           Text_i = "I risultati di questo comune non sono ancora noti.")
  
  #Spezialfälle
  hist_check <- FALSE
  hist_several_check <- FALSE
  other_check <- FALSE
  
  #Ausgezählte Gemeinden auswählen
  results_notavailable <- results[results$Gebiet_Ausgezaehlt == FALSE,]
  results <- results[results$Gebiet_Ausgezaehlt == TRUE,]
  
  #Sind schon Daten vorhanden?
  if (nrow(results) > 0) {
    
    #Daten anpassen
    results <- augment_raw_data(results)

    #Intros generieren
    results <- normal_intro(results)
    
#Vergleich innerhalb des Kantons (falls Daten vom Kanton vorhanden)
    
    if (json_data_kantone$kantone$vorlagen[[kantonal_number[k]]]$vorlageBeendet[[kantonal_add[k]]] == TRUE) {
    results <- kanton_storyfinder_kantonal(results)
    }
    
    #Textvorlagen laden
    Textbausteine <- as.data.frame(read_excel(paste0("Texte/Textbausteine_LENA_",abstimmung_date,".xlsx"), 
                                              sheet = kantonal_short[k]))

    cat("Textvorlagen geladen\n\n")

    #Texte einfügen
    results <- build_texts(results)
    
    #Variablen ersetzen 
    results <- replace_variables(results)
    
    ###Texte anpassen und optimieren
    results <- excuse_my_french(results)
    
  }
  ###Ausgezählte und nicht ausgezählte Gemeinden wieder zusammenführen -> Immer gleiches Format für Datawrapper
  if (nrow(results_notavailable) > 0) {
    
    results_notavailable$Ja_Stimmen_In_Prozent <- 0
    results_notavailable$Nein_Stimmen_In_Prozent <- 0
    results_notavailable$Gemeinde_color <- 50
    
    results <- rbind(results,results_notavailable) %>%
      arrange(Gemeinde_Nr)
    
  }

#Texte speichern
  if (save_texts == TRUE) {
  texts <- results %>%
    select(Gemeinde_KT_d,
           Storyboard,
           Text_d,
           Text_f,
           Text_i)
library(xlsx)
write.xlsx(texts,paste0("./Texte/",kantonal_short[k],"_texte.xlsx"),row.names = FALSE)
  }  
  ###Output generieren für Datawrapper
  #Output Abstimmungen Gemeinde
  output_dw_de <- get_output_gemeinden(results,language = "de")
  output_dw_fr <- get_output_gemeinden(results,language = "fr")
  output_dw_it <- get_output_gemeinden(results,language = "it")
  
  #Output speichern
  write.csv(output_dw_de,paste0("Output_Cantons/",kantonal_short[k],"_dw_de.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_fr,paste0("Output_Cantons/",kantonal_short[k],"_dw_fr.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_it,paste0("Output_Cantons/",kantonal_short[k],"_dw_it.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  cat(paste0("\nGenerated output for Vorlage ",kantonal_short[k],"\n"))
  
  ###
  count_non_gemeinden <- output_dw_de[output_dw_de$Nein_Stimmen_In_Prozent>50,]
  count_yes_gemeinden <- output_dw_de[output_dw_de$Ja_Stimmen_In_Prozent>50,]
  count_tie_gemeinden <- output_dw_de[output_dw_de$Ja_Stimmen_In_Prozent == 50,]
  print(paste0("Nein-Stimmen: ",nrow(count_non_gemeinden),"; Ja-Stimmen: ",nrow(count_yes_gemeinden),
               "; Unentschieden: ",nrow(count_tie_gemeinden)))
  
  #Datawrapper-Karten aktualisieren
  undertitel_de <- "Es sind noch keine Gemeinden ausgezählt."
  undertitel_fr <- "Aucun résultat n'est encore connu."
  undertitel_it <- "Nessun risultato è ancora noto."

    if (is.na(Ja_Stimmen_Kanton) == FALSE) {
      undertitel_de <- paste0("Die brieflichen Stimmen sind ausgezählt.<br>Stand: <b>",
                              round2(Ja_Stimmen_Kanton,1)," %</b> Ja, <b>",
                              round2(100-Ja_Stimmen_Kanton,1)," %</b> Nein")
      
      undertitel_fr <- paste0("Les votes par correspondance ont été dépouillés.<br>Etat: <b>",
                              round2(Ja_Stimmen_Kanton,1)," %</b> oui, <b>",
                              round2(100-Ja_Stimmen_Kanton,1)," %</b> non")
      
      undertitel_it <- paste0("I voti per corrispondenza sono stati scrutinati.<br>Stato: <b>",
                              round2(Ja_Stimmen_Kanton,1)," %</b> sì, <b>",
                              round2(100-Ja_Stimmen_Kanton,1)," %</b> no")
      
    
      if (nrow(results_notavailable) == 0) {
      undertitel_de <- paste0("Resultat: <b>",
                                round2(Ja_Stimmen_Kanton,1)," %</b> Ja, <b>",
                                round2(100-Ja_Stimmen_Kanton,1)," %</b> Nein")
        
      undertitel_fr <- paste0("Résultats: <b>",
                                round2(Ja_Stimmen_Kanton,1)," %</b> oui, <b>",
                                round2(100-Ja_Stimmen_Kanton,1)," %</b> non")
        
      undertitel_it <- paste0("Risultati: <b>",
                                round2(Ja_Stimmen_Kanton,1)," %</b> sì, <b>",
                                round2(100-Ja_Stimmen_Kanton,1)," %</b> no")
      } else if (sum(results$Gebiet_Ausgezaehlt) > 0 ) {
      undertitel_de <- paste0("Es sind <b>",sum(results$Gebiet_Ausgezaehlt),"</b> von <b>",nrow(results),
                              "</b> Gemeinden ausgezählt.<br>Stand: <b>",
                              round2(Ja_Stimmen_Kanton,1)," %</b> Ja, <b>",
                              round2(100-Ja_Stimmen_Kanton,1)," %</b> Nein")
      
      undertitel_fr <- paste0("Les résultats de <b>",sum(results$Gebiet_Ausgezaehlt),"</b> des <b>",nrow(results),
                              "</b> communes sont connus.<br>Etat: <b>",
                              round2(Ja_Stimmen_Kanton,1)," %</b> oui, <b>",
                              round2(100-Ja_Stimmen_Kanton,1)," %</b> non")
      
      undertitel_it <- paste0("I risultati di <b>",sum(results$Gebiet_Ausgezaehlt),"</b> dei <b>",nrow(results),
                              "</b> comuni sono noti.<br>Stato: <b>",
                              round2(Ja_Stimmen_Kanton,1)," %</b> sì, <b>",
                              round2(100-Ja_Stimmen_Kanton,1)," %</b> no")
      
    } 
    }
    
    datawrapper_codes_vorlage <- datawrapper_codes_kantonal[datawrapper_codes_kantonal$Vorlage == kantonal_short[k],]
    
    #Karten Gemeinden
    for (r in 1:nrow(datawrapper_codes_vorlage)) {
      if (datawrapper_codes_vorlage$Sprache[r] == "de-DE") {
        dw_edit_chart(datawrapper_codes_vorlage[r,5],intro=undertitel_de,annotate=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
        dw_publish_chart(datawrapper_codes_vorlage[r,5])
      } else if (datawrapper_codes_vorlage$Sprache[r] == "fr-CH") {
        dw_edit_chart(datawrapper_codes_vorlage[r,5],intro=undertitel_fr,annotate=paste0("dernière mise à jour: ",format(Sys.time(),"%d.%m.%Y %Hh%M")))
        dw_publish_chart(datawrapper_codes_vorlage[r,5])
      } else if (datawrapper_codes_vorlage$Sprache[r] == "it-CH") {
        dw_edit_chart(datawrapper_codes_vorlage[r,5],intro=undertitel_it,annotate=paste0("Ultimo aggiornamento: ",format(Sys.time(),"%H:%M")))
        dw_publish_chart(datawrapper_codes_vorlage[r,5])
      }
    }
    
    if (sum(results$Gebiet_Ausgezaehlt) == nrow(results)) {
      
      #Set mail output to done
      mydb <- connectDB(db_name = "sda_votes")  
      sql_qry <- paste0("UPDATE votes_metadata SET status = 'done' WHERE date = '",voting_date,"' AND spreadsheet = '",kantonal_short[k],"'")
      rs <- dbSendQuery(mydb, sql_qry)
      dbDisconnectAll() 
      
      print(paste0("Alle Daten der Abstimmung ",kantonal_short[k]," vorhanden"))
    }
    
    
  } else {
    cat(paste0("\nVorlage ",kantonal_short[k]," bereits komplett\n"))  
  }  
}


