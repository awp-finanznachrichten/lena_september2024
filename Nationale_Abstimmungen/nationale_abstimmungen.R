#Daten für Uebersicht
data_overview <- data.frame(50,50,"Abstimmung_de","Abstimmung_fr","Abstimmung_it")
colnames(data_overview) <- c("Ja","Nein","Abstimmung_de","Abstimmung_fr","Abstimmung_it")

for (i in 1:length(vorlagen_short)) {

  cat(paste0("\nErmittle Daten für folgende Vorlage: ",vorlagen$text[i],"\n"))
  
  ###Nationale Resultate aus JSON auslesen
  results_national <- get_results(json_data,i,level="national")

  ###Resultate aus JSON auslesen für Gemeinden
  results <- get_results(json_data,i)

  #Daten anpassen Gemeinden
  results <- treat_gemeinden(results)
  results <- format_data_g(results)
  
  #Kantonsdaten hinzufügen
  results_kantone <- get_results(json_data,i,"cantonal")
  
  Ja_Stimmen_Kanton <- results_kantone %>%
    select(Kantons_Nr,jaStimmenInProzent) %>%
    rename(Ja_Stimmen_In_Prozent_Kanton = jaStimmenInProzent) %>%
    mutate(Highest_Yes_Kant = FALSE,
           Highest_No_Kant = FALSE)
  
  results <- merge(results,Ja_Stimmen_Kanton)
  results_all <- results

  #Alle Daten speichern
  write.csv(results_all,paste0("Output_Switzerland/",vorlagen_short[i],"_all_data.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")

  #Wie viele Gemeinden sind ausgezählt
  cat(paste0(sum(results$Gebiet_Ausgezaehlt)," Gemeinden sind ausgezählt.\n"))
  
  #Neue Variablen
  results <- results %>%
    mutate(Ja_Nein = NA,
           Oui_Non = NA,
           Unentschieden = NA,
           Einstimmig_Ja = NA,
           Einstimmig_Nein = NA,
           kleine_Gemeinde = NA,
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
    
    #LENA-Classics (falls alle Gemeinden ausgezählt):
    if (nrow(results_notavailable) == 0) {
      results <- lena_classics(results)
    }  
  
    #Special Vergleich mit anderer Abstimmung am selben Datum
    if (votes_metadata_CH$remarks[i] == "other_comparison_first" || votes_metadata_CH$remarks[i] == "other_comparison_second") { 
    other_check <- TRUE
    if (votes_metadata_CH$remarks[i] == "other_comparison_first") {
      results_othervote <- get_results(json_data,i+1)
    } else if (votes_metadata_CH$remarks[i] == "other_comparison_second") {
      results_othervote <- get_results(json_data,i-1)
    }
    results_othervote <- results_othervote %>%
      select(Gemeinde_Nr,
             jaStimmenInProzent) %>%
      rename(Other_Ja_Stimmen_In_Prozent = jaStimmenInProzent) %>%
      mutate(Other_Nein_Stimmen_In_Prozent = 100-Other_Ja_Stimmen_In_Prozent)
    
    results <- merge(results,results_othervote,all.x = TRUE)
    results <- other_storyfinder(results)
    }
    

    #Historischer Vergleich (falls vorhanden)
    if (votes_metadata_CH$remarks[i] == "history_comparison") {
      hist_check <- TRUE 
      results <- merge(results,data_hist,all.x = TRUE)
      results <- hist_storyfinder(results)
    }
    
    if (votes_metadata_CH$remarks[i] == "history_comparison_special") {
      hist_check <- TRUE 
      results <- merge(results,data_hist,all.x = TRUE)
      results <- hist_storyfinder_special(results)
    }

    if (votes_metadata_CH$remarks[i] == "history_comparison_several") {
      hist_check <- TRUE
      hist_several_check <- TRUE
      data_hist_1 <- format_data_hist(daten_covid1_bfs)
      data_hist_1 <- data_hist_1 %>%
        select(Gemeinde_Nr,Hist_Ja_Stimmen_In_Prozent) %>%
        rename(Hist1_Ja_Stimmen_In_Prozent = Hist_Ja_Stimmen_In_Prozent)
      data_hist_2 <- format_data_hist(daten_covid2_bfs)
      data_hist_2 <- data_hist_2 %>%
        select(Gemeinde_Nr,Hist_Ja_Stimmen_In_Prozent) %>%
        rename(Hist2_Ja_Stimmen_In_Prozent = Hist_Ja_Stimmen_In_Prozent)
      results <- merge(results,data_hist_1,all.x = TRUE)
      results <- merge(results,data_hist_2,all.x = TRUE)
      results <- hist_storyfinder_several(results)
    }

    #Vergleich innerhalb des Kantons (falls alle Daten vom Kanton vorhanden)
    if (votes_metadata_CH$remarks[i] == "canton_comparison") {
      #Falls mindestens ein Kanton ausgezählt -> Stories für die Kantone finden
      if (length(unique(results_notavailable$Kantons_Nr)) < 26) {
        results <- kanton_storyfinder(results)
      }
    }

    ###Storybuilder
    
    #Textvorlagen laden
    Textbausteine <- as.data.frame(read_excel(paste0("Texte/Textbausteine_LENA_",abstimmung_date,".xlsx"), 
                                              sheet = vorlagen_short[i]))
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
    
    if ((hist_check == TRUE) & (hist_several_check == FALSE)) {
      results_notavailable$Hist_Ja_Stimmen_In_Prozent <- NA
      results_notavailable$Hist_Ja_Stimmen_Absolut <- NA
      results_notavailable$Hist_Nein_Stimmen_In_Prozent <- NA
      results_notavailable$Hist_Nein_Stimmen_Absolut <- NA
      
    }
    
    if ((hist_check == TRUE) & (hist_several_check == TRUE)) {
      results_notavailable$Hist1_Ja_Stimmen_In_Prozent <- NA
      results_notavailable$Hist2_Ja_Stimmen_In_Prozent <- NA
    }
    
    if (other_check == TRUE) {
      results_notavailable$Other_Ja_Stimmen_In_Prozent <- NA
      results_notavailable$Other_Nein_Stimmen_In_Prozent <- NA
    }

    results <- rbind(results,results_notavailable) %>%
      arrange(Gemeinde_Nr)
    
  }
  
###Texte speichern
if (save_texts == TRUE) {
texts <- results %>%
  select(Gemeinde_KT_d,
         Storyboard,
         Text_d,
         Text_f,
         Text_i)

library(xlsx)
write.xlsx(texts,paste0("./Texte/",vorlagen_short[i],"_texte.xlsx"),row.names = FALSE)
}  
  ###Output generieren für Datawrapper
  
  #Output Abstimmungen Gemeinde
  output_dw_de <- get_output_gemeinden(results,language = "de")
  output_dw_fr <- get_output_gemeinden(results,language = "fr")
  output_dw_it <- get_output_gemeinden(results,language = "it")

  #Output speichern
  write.csv(output_dw_de,paste0("Output_Switzerland/",vorlagen_short[i],"_dw_de.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_fr,paste0("Output_Switzerland/",vorlagen_short[i],"_dw_fr.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_it,paste0("Output_Switzerland/",vorlagen_short[i],"_dw_it.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")

  #Output Abstimmungen pro Kanton
  for (c in 1:nrow(cantons_overview)) {
    if (grepl("de",cantons_overview$languages[c]) == TRUE) {
    output_kanton_dw_de <- output_dw_de %>%
      filter(grepl(paste0(" ",cantons_overview$area_ID[c]),Gemeinde_de) == TRUE)
    write.csv(output_kanton_dw_de,paste0("Output_Cantons/",cantons_overview$area_ID[c],"_",vorlagen_short[i],"_dw_de.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
    }
    if (grepl("fr",cantons_overview$languages[c]) == TRUE) {
      output_kanton_dw_fr <- output_dw_fr %>%
        filter(grepl(paste0(" [(]",cantons_overview$area_ID[c],"[)]"),Gemeinde_fr) == TRUE)
    write.csv(output_kanton_dw_fr,paste0("Output_Cantons/",cantons_overview$area_ID[c],"_",vorlagen_short[i],"_dw_fr.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
    }
    if (grepl("it",cantons_overview$languages[c]) == TRUE) {
      output_kanton_dw_it <- output_dw_it %>%
        filter(grepl(paste0(" [(]",cantons_overview$area_ID[c],"[)]"),Gemeinde_it) == TRUE)
    write.csv(output_kanton_dw_it,paste0("Output_Cantons/",cantons_overview$area_ID[c],"_",vorlagen_short[i],"_dw_it.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
    }
  }  

  count_non_gemeinden <- output_dw_de[output_dw_de$Nein_Stimmen_In_Prozent>50,]
  count_yes_gemeinden <- output_dw_de[output_dw_de$Ja_Stimmen_In_Prozent>50,]
  count_tie_gemeinden <- output_dw_de[output_dw_de$Ja_Stimmen_In_Prozent == 50,]
  
  print(paste0("Nein-Stimmen: ",nrow(count_non_gemeinden),"; Ja-Stimmen: ",nrow(count_yes_gemeinden),
               "; Unentschieden: ",nrow(count_tie_gemeinden)))
  
  #Stimmbeteiligung und Ständemehr
  print(paste0("Stimmbeteiligung: ",results_national$stimmbeteiligungInProzent," %"))
  print(paste0("Stände JA: ",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2)))
  print(paste0("Stände NEIN: ",results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2)))
  
  #source("outputs_einzugsgebiete.R", encoding = "UTF-8")
  
  #Output Abstimmungen Kantone
  output_dw_kantone <- get_output_kantone(results)
  write.csv(output_dw_kantone,paste0("Output_Switzerland/",vorlagen_short[i],"_dw_kantone.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  cat(paste0("\nGenerated output for Vorlage ",vorlagen_short[i],"\n"))
  
  #Datawrapper-Karten aktualisieren
  undertitel_de <- "Es sind noch keine Gemeinden ausgezählt."
  undertitel_fr <- "Aucun résultat n'est encore connu."
  undertitel_it <- "Nessun risultato è ancora noto."

  if (nrow(results_notavailable) == 0) {
    undertitel_de <- paste0("Volk: <b>",
                            round2(results_national$jaStimmenInProzent,1)," %</b> Ja, <b>",
                            round2(100-results_national$jaStimmenInProzent,1)," %</b> Nein.<br>",
                            "Stände: <b>",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2),"</b> Ja, <b>",
                            results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), "</b> Nein.")
    undertitel_de <- gsub("NA","0",undertitel_de)
    
    undertitel_fr <- paste0("Peuple: <b>",
                            round2(results_national$jaStimmenInProzent,1)," %</b> oui, <b>",
                            round2(100-results_national$jaStimmenInProzent,1)," %</b> non.<br>",
                            "Cantons: <b>",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2),"</b> oui, <b>",
                            results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), "</b> non.")
    undertitel_fr <- gsub("NA","0",undertitel_fr)
    
    undertitel_it <- paste0("Popolo: <b>",
                            round2(results_national$jaStimmenInProzent,1)," %</b> sì, <b>",
                            round2(100-results_national$jaStimmenInProzent,1)," %</b> no.<br>",
                            "Cantoni: <b>",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2),"</b> sì, <b>",
                            results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), "</b> no.")
    undertitel_it <- gsub("NA","0",undertitel_it)
  
  } else if (sum(results$Gebiet_Ausgezaehlt) > 0 ) {
    
    undertitel_de <- paste0("Es sind <b>",sum(results$Gebiet_Ausgezaehlt),"</b> von <b>",nrow(results),
                            "</b> Gemeinden ausgezählt.<br>Volk: <b>",
                            round2(results_national$jaStimmenInProzent,1)," %</b> Ja, <b>",
                            round2(100-results_national$jaStimmenInProzent,1)," %</b> Nein.<br>",
                            "Stände: <b>",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2),"</b> Ja, <b>",
                            results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), "</b> Nein.")
    undertitel_de <- gsub("NA","0",undertitel_de)

    undertitel_fr <- paste0("Les résultats de <b>",sum(results$Gebiet_Ausgezaehlt),"</b> des <b>",nrow(results),
                            "</b> communes sont connus. Peuple: <b>",
                            round2(results_national$jaStimmenInProzent,1)," %</b> oui, <b>",
                            round2(100-results_national$jaStimmenInProzent,1)," %</b> non.<br>",
                            "Cantons: <b>",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2),"</b> oui, <b>",
                            results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), "</b> non.")
    undertitel_fr <- gsub("NA","0",undertitel_fr)
    
    undertitel_it <- paste0("I risultati di <b>",sum(results$Gebiet_Ausgezaehlt),"</b> dei <b>",nrow(results),
                            "</b> comuni sono noti. Popolo: <b>",
                            round2(results_national$jaStimmenInProzent,1)," %</b> sì, <b>",
                            round2(100-results_national$jaStimmenInProzent,1)," %</b> no.<br>",
                            "Cantoni: <b>",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2),"</b> sì, <b>",
                            results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), "</b> no.")
    undertitel_it <- gsub("NA","0",undertitel_it)

  }  
  
  datawrapper_codes_vorlage <- datawrapper_codes[datawrapper_codes$Vorlage == vorlagen_short[i],]

    #Karten Gemeinden
    dw_edit_chart(datawrapper_codes_vorlage[1,5],intro=undertitel_de,annotate=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
    dw_publish_chart(datawrapper_codes_vorlage[1,5])
    
    dw_edit_chart(datawrapper_codes_vorlage[3,5],intro=undertitel_fr,annotate=paste0("dernière mise à jour: ",format(Sys.time(),"%d.%m.%Y %Hh%M")))
    dw_publish_chart(datawrapper_codes_vorlage[3,5])
    
    dw_edit_chart(datawrapper_codes_vorlage[5,5],intro=undertitel_it,annotate=paste0("Ultimo aggiornamento: ",format(Sys.time(),"%d.%m.%Y %H:%M")))
    dw_publish_chart(datawrapper_codes_vorlage[5,5])
    
    #Karten Kantone
    dw_edit_chart(datawrapper_codes_vorlage[2,5],intro=undertitel_de,annotate=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
    dw_publish_chart(datawrapper_codes_vorlage[2,5])
    
    dw_edit_chart(datawrapper_codes_vorlage[4,5],intro=undertitel_fr,annotate=paste0("dernière mise à jour: ",format(Sys.time(),"%d.%m.%Y %Hh%M")))
    dw_publish_chart(datawrapper_codes_vorlage[4,5])
    
    dw_edit_chart(datawrapper_codes_vorlage[6,5],intro=undertitel_it,annotate=paste0("Ultimo aggiornamento: ",format(Sys.time(),"%d.%m.%Y %H:%M")))
    dw_publish_chart(datawrapper_codes_vorlage[6,5])
  

#Eintrag für Uebersicht
uebersicht_text_de <- paste0("<b>",vorlagen$text[i],"</b><br>",
                             "Es sind noch keine Gemeinden ausgezählt.")

uebersicht_text_fr <- paste0("<b>",vorlagen_fr$text[i],"</b><br>",
                             "Aucun résultat n'est encore connu.")

uebersicht_text_it <- paste0("<b>",vorlagen_it$text[i],"</b><br>",
                             "Nessun risultato è ancora noto.")
Ja_Anteil <- 50
Nein_Anteil <- 50

if (nrow(results_notavailable) == 0) {
print(paste0("Alle Gemeinden der Vorlage ",vorlagen$text[i]," ausgezählt!"))
    
uebersicht_text_de <- paste0("<b>",vorlagen$text[i],"</b><br>",
                             "Stände: ",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2)," Ja, ",
                             results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), " Nein")
uebersicht_text_de <- gsub("NA","0",uebersicht_text_de)

uebersicht_text_fr <- paste0("<b>",vorlagen_fr$text[i],"</b><br>",
                             "Cantons: ",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2)," oui, ",
                             results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), " non")
uebersicht_text_fr <- gsub("NA","0",uebersicht_text_fr) 

uebersicht_text_it <- paste0("<b>",vorlagen_it$text[i],"</b><br>",
                             "Cantoni: ",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2)," sì, ",
                             results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), " no")
uebersicht_text_it <- gsub("NA","0",uebersicht_text_it) 

Ja_Anteil <- round2(results_national$jaStimmenInProzent,1)
Nein_Anteil <- round2(100-results_national$jaStimmenInProzent,1)  
} else if (sum(results$Gebiet_Ausgezaehlt) > 0 ) {  

uebersicht_text_de <- paste0("<b>",vorlagen$text[i],"</b><br>",
                          sum(results$Gebiet_Ausgezaehlt)," von ",nrow(results)," Gemeinden ausgezählt (",
                          round2((sum(results$Gebiet_Ausgezaehlt)*100)/nrow(results),1),
                          "%)<br>",
                          "Stände: ",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2)," Ja, ",
                          results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), " Nein")
uebersicht_text_de <- gsub("NA","0",uebersicht_text_de)                    

uebersicht_text_fr <- paste0("<b>",vorlagen_fr$text[i],"</b><br>Les résultats de ",
                             sum(results$Gebiet_Ausgezaehlt)," des ",nrow(results)," communes sont connus (",
                             round2((sum(results$Gebiet_Ausgezaehlt)*100)/nrow(results),1),
                             "%)<br>",
                             "Cantons: ",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2)," oui, ",
                             results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), " non")
uebersicht_text_fr <- gsub("NA","0",uebersicht_text_fr) 

uebersicht_text_it <- paste0("<b>",vorlagen_it$text[i],"</b><br>",
                             sum(results$Gebiet_Ausgezaehlt)," dei ",nrow(results)," comuni sono noti (",
                             round2((sum(results$Gebiet_Ausgezaehlt)*100)/nrow(results),1),
                             "%)<br>",
                             "Cantoni: ",results_national$jaStaendeGanz+(results_national$jaStaendeHalb/2)," sì, ",
                             results_national$neinStaendeGanz+(results_national$neinStaendeHalb/2), " no")
uebersicht_text_it <- gsub("NA","0",uebersicht_text_it) 

Ja_Anteil <- round2(results_national$jaStimmenInProzent,1)
Nein_Anteil <- round2(100-results_national$jaStimmenInProzent,1)

}

entry_overview <- data.frame(Ja_Anteil,Nein_Anteil,uebersicht_text_de,uebersicht_text_fr,uebersicht_text_it)
colnames(entry_overview) <- c("Ja","Nein","Abstimmung_de","Abstimmung_fr","Abstimmung_it")
data_overview <- rbind(data_overview,entry_overview)

}

#Uebersicht für Datawrapper
data_overview <- data_overview[-1,]
write.csv(data_overview,"Output_Overviews/Uebersicht_dw.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")


#Charts Uebersicht
#DEUTSCH
dw_data_to_chart(data_overview,datawrapper_codes[1,5])
if (nrow(results_notavailable) == 0) {
titel_de <- paste0("Die Abstimmungen vom ",day(voting_date),". ",monate_de[month(voting_date)]," ",year(voting_date)," in der Übersicht")  
dw_edit_chart(datawrapper_codes[1,5],
              title = titel_de,
              intro = "")
} else {
dw_edit_chart(datawrapper_codes[1,5],intro=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%H:%M Uhr")))
}  
dw_publish_chart(datawrapper_codes[1,5])

#FRANZOESICH
dw_data_to_chart(data_overview,datawrapper_codes[2,5])
if (nrow(results_notavailable) == 0) {
titel_fr <- paste0("Les résultats des votes du ",day(voting_date)," ",monate_fr[month(voting_date)]," ",year(voting_date))
dw_edit_chart(datawrapper_codes[2,5],
                title = titel_fr,
                intro = "")
} else {  
dw_edit_chart(datawrapper_codes[2,5],intro=paste0("Dernière mise à jour: ",format(Sys.time(),"%Hh%M")))
}
dw_publish_chart(datawrapper_codes[2,5])

#ITALIENISCH
dw_data_to_chart(data_overview,datawrapper_codes[3,5])
if (nrow(results_notavailable) == 0) {
titel_it <- paste0("I risultati delle votazioni del ",day(voting_date)," ",monate_it[month(voting_date)]," ",year(voting_date))
dw_edit_chart(datawrapper_codes[3,5],
              title = titel_it,
              intro = "")
} else {  
dw_edit_chart(datawrapper_codes[3,5],intro=paste0("Ultimo aggiornamento: ",format(Sys.time(),"%H:%M")))  
}  
dw_publish_chart(datawrapper_codes[3,5])

#Output Overall Texts
for (i in 1:length(vorlagen_short)) {
data_de <- read.csv(paste0("./Output_Switzerland/",vorlagen_short[i],"_dw_de.csv"))
data_de <- data_de %>%
  select(Gemeinde_de,Text_de)
data_fr <- read.csv(paste0("./Output_Switzerland/",vorlagen_short[i],"_dw_fr.csv"))
data_fr <- data_fr %>%
  select(Gemeinde_fr,Text_fr)
data_it <- read.csv(paste0("./Output_Switzerland/",vorlagen_short[i],"_dw_it.csv"))
data_it <- data_it %>%
  select(Gemeinde_it,Text_it)

data_all <- cbind(data_de,data_fr,data_it)
write.csv(data_all,paste0("Output_Switzerland/",vorlagen_short[i],"_all_texts.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
}  

