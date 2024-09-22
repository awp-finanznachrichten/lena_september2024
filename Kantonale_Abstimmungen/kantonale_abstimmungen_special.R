mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM votes_metadata WHERE date = '",voting_date,"' AND area_ID != 'CH' AND status = 'done'" ))
votes_metadata_cantonal <- DBI::fetch(rs,n=-1)
dbDisconnectAll()
completed_votes <- na.omit(unique(votes_metadata_cantonal$spreadsheet))

for (s in 1:length(kantonal_short_special) ) {

  if (sum(grepl(kantonal_short_special[s],completed_votes)) == 0) {
  cat(paste0("\nErmittle Daten für folgende Vorlage: ",kantonal_short_special[s],"\n"))
  
  results <- get_results_kantonal(json_data_kantone,
                                  kantonal_number_special[s],
                                  kantonal_add_special[s])
  
  results_kantonal_special_initiative <- get_results_kantonal(json_data_kantone,
                                                   kantonal_number_special[s],
                                                   kantonal_add_special[s],
                                                   level="kantonal")
  
  #Daten anpassen Gemeinden
  results <- treat_gemeinden(results)
  results <- format_data_g(results)
 

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
           Text_i = "I resultati di questa comune non sono ancora noti.")
  
  hist_check <- FALSE
  
  #Gegenvorschlag hinzufügen
  results_gegenvorschlag <- get_results_kantonal(json_data_kantone,
                                                 kantonal_number_special[s],
                                                 ifelse(kantonal_short_special[s] == "VS_Verfassung",
                                                        kantonal_add_special[s]+2,
                                                        kantonal_add_special[s]+1))
                                                 
  results_kantonal_special_gegenvorschlag <- get_results_kantonal(json_data_kantone,
                                                              kantonal_number_special[s],
                                                              ifelse(kantonal_short_special[s] == "VS_Verfassung",
                                                                     kantonal_add_special[s]+2,
                                                                     kantonal_add_special[s]+1),
                                                              level="kantonal")
  

  results_gegenvorschlag <- results_gegenvorschlag[,c(3:6,11)]
  results_gegenvorschlag$neinStimmenInProzent <- 100-results_gegenvorschlag$jaStimmenInProzent
  
  colnames(results_gegenvorschlag) <- c("Gebiet_Ausgezaehlt_Gegenvorschlag","Ja_Prozent_Gegenvorschlag","Ja_Absolut_Gegenvorschlag","Nein_Absolut_Gegenvorschlag",
                                        "Gemeinde_Nr","Nein_Prozent_Gegenvorschlag")

  results <- merge(results,results_gegenvorschlag)
  

  #Stichentscheid hinzufügen
  results_stichentscheid <- get_results_kantonal(json_data_kantone,
                                                 kantonal_number_special[s],
                                                 ifelse(kantonal_short_special[s] == "VS_Verfassung",
                                                        kantonal_add_special[s]+3,
                                                        kantonal_add_special[s]+2))
  
  results_kantonal_special_stichentscheid <- get_results_kantonal(json_data_kantone,
                                                                  kantonal_number_special[s],
                                                                  ifelse(kantonal_short_special[s] == "VS_Verfassung",
                                                                         kantonal_add_special[s]+3,
                                                                         kantonal_add_special[s]+2),
                                                                  level="kantonal")
  
  results_stichentscheid  <- results_stichentscheid[,c(3:4,11)]
  results_stichentscheid$neinStimmenInProzent <- 100-results_stichentscheid$jaStimmenInProzent
  
  colnames(results_stichentscheid) <- c("Gebiet_Ausgezaehlt_Stichentscheid","Stichentscheid_Zustimmung_Hauptvorlage","Gemeinde_Nr","Stichentscheid_Zustimmung_Gegenvorschlag")
  
  results <- merge(results,results_stichentscheid)

  #Ausgezählte Gemeinden auswählen
  results_notavailable <- results[results$Gebiet_Ausgezaehlt == FALSE |
                                    results$Gebiet_Ausgezaehlt_Gegenvorschlag == FALSE |
                                    results$Gebiet_Ausgezaehlt_Stichentscheid == FALSE,]
  
  results <- results[results$Gebiet_Ausgezaehlt == TRUE &
                       results$Gebiet_Ausgezaehlt_Gegenvorschlag == TRUE &
                       results$Gebiet_Ausgezaehlt_Stichentscheid == TRUE,]

  #Sind schon Daten vorhanden?
  if (nrow(results) > 0) {
    
    #Daten anpassen
    results <- augment_raw_data(results)
    
    #Texte generieren
    results <- special_intro(results)
  
    #Textvorlagen laden
    Textbausteine <- as.data.frame(read_excel(paste0("Texte/Textbausteine_LENA_",abstimmung_date,".xlsx"), 
                                              sheet = kantonal_short_special[s]))
    cat("Textvorlagen geladen\n\n")


    #Texte einfügen
    results <- build_texts(results)
    
    #Variablen ersetzen 
    results <- replace_variables_special(results)
    
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
write.xlsx(texts,paste0("./Texte/",kantonal_short_special[s],"_texte.xlsx"),row.names = FALSE)
}
  ###Output generieren für Datawrapper
  
  ###OUTPUT OVERVIEW
  output_dw_de_overview <- get_output_gemeinden(results,language = "de")
  output_dw_fr_overview <- get_output_gemeinden(results,language = "fr")
  output_dw_it_overview <- get_output_gemeinden(results,language = "it")
  

  for (z in 1:nrow(results)) {
    
  if (grepl("Intro_HauptvorlageJa_GegenvorschlagJa_StichentscheidHauptvorlage|Intro_HauptvorlageJa_GegenvorschlagNein",results$Storyboard[z]) == TRUE) {
  output_dw_de_overview$Gemeinde_color[z] <- "Initiative Ja"
  output_dw_fr_overview$Gemeinde_color[z] <- "initiative oui"
  output_dw_it_overview$Gemeinde_color[z] <- "iniziative si"
  }  
    
  if (grepl("Intro_HauptvorlageJa_GegenvorschlagJa_StichentscheidGegenvorschlag|Intro_HauptvorlageNein_GegenvorschlagJa",results$Storyboard[z]) == TRUE) {
      output_dw_de_overview$Gemeinde_color[z] <- "Gegenvorschlag Ja"
      output_dw_fr_overview$Gemeinde_color[z] <- "contre-proposition oui"
      output_dw_it_overview$Gemeinde_color[z] <- "controproposta si"
  }  
    
    
  if (grepl("Intro_HauptvorlageNein_GegenvorschlagNein",results$Storyboard[z]) == TRUE) {
      output_dw_de_overview$Gemeinde_color[z] <- "Zweimal Nein"
      output_dw_fr_overview$Gemeinde_color[z] <- "deux fois non"
      output_dw_it_overview$Gemeinde_color[z] <- "due no"
  }  
    
    if (is.na(results$Storyboard[z]) == TRUE) {
      output_dw_de_overview$Gemeinde_color[z] <- "Noch keine Resultate"
      output_dw_fr_overview$Gemeinde_color[z] <- "aucun résultat"
      output_dw_it_overview$Gemeinde_color[z] <- "nessun risultato"
    }  
      
  }  
    
  write.csv(output_dw_de_overview,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_de_overview.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_fr_overview,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_fr_overview.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_it_overview,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_it_overview.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  

    

  ###OUTPUT INITIATIVE
  #Output Abstimmungen Gemeinde
  output_dw_de <- get_output_gemeinden(results,language = "de")
  output_dw_fr <- get_output_gemeinden(results,language = "fr")
  output_dw_it <- get_output_gemeinden(results,language = "it")

  #Output speichern
  write.csv(output_dw_de,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_de_initiative.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_fr,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_fr_initiative.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_it,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_it_initiative.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  ###OUTPUT GEGENVORSCHLAG
  for (r in 1:nrow(results)) {

  if (is.na(results$Ja_Prozent_Gegenvorschlag[r]) == FALSE) {
  results$Gemeinde_color[r] <- results$Ja_Prozent_Gegenvorschlag[r]
  results$Ja_Stimmen_In_Prozent[r] <- results$Ja_Prozent_Gegenvorschlag[r]
  results$Nein_Stimmen_In_Prozent[r] <- results$Nein_Prozent_Gegenvorschlag[r]
  }
  }

  #Output Abstimmungen Gemeinde
  output_dw_de_gegenvorschlag <- get_output_gemeinden(results,language = "de")
  output_dw_fr_gegenvorschlag <- get_output_gemeinden(results,language = "fr")
  output_dw_it_gegenvorschlag <- get_output_gemeinden(results,language = "it")
  
  #Output speichern
  write.csv(output_dw_de_gegenvorschlag,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_de_gegenvorschlag.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_fr_gegenvorschlag,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_fr_gegenvorschlag.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_it_gegenvorschlag,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_it_gegenvorschlag.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  ###OUTPUT STICHENTSCHEIDUG
  for (r in 1:nrow(results)) {
  if (is.na(results$Stichentscheid_Zustimmung_Hauptvorlage[r]) == FALSE) {
  results$Gemeinde_color[r] <- results$Stichentscheid_Zustimmung_Hauptvorlage[r]
  results$Ja_Stimmen_In_Prozent[r] <- results$Stichentscheid_Zustimmung_Hauptvorlage[r]
  results$Nein_Stimmen_In_Prozent[r] <- results$Stichentscheid_Zustimmung_Gegenvorschlag[r]
  }
  }
  #Output Abstimmungen Gemeinde
  output_dw_de_stichentscheid <- get_output_gemeinden(results,language = "de")
  output_dw_fr_stichentscheid <- get_output_gemeinden(results,language = "fr")
  output_dw_it_stichentscheid <- get_output_gemeinden(results,language = "it")
  
  #Output speichern
  write.csv(output_dw_de_stichentscheid,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_de_stichentscheid.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_fr_stichentscheid,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_fr_stichentscheid.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(output_dw_it_stichentscheid,paste0("Output_Cantons/",kantonal_short_special[s],"_dw_it_stichentscheid.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  

  
  cat(paste0("\nGenerated output for Vorlage ",kantonal_short_special[s],"\n"))
  
  
  ###
  count_non_gemeinden <- output_dw_de[output_dw_de$Nein_Stimmen_In_Prozent>50,]
  
  count_yes_gemeinden <- output_dw_de[output_dw_de$Ja_Stimmen_In_Prozent>50,]
  
  count_tie_gemeinden <- output_dw_de[output_dw_de$Ja_Stimmen_In_Prozent == 50,]
  
  print(paste0("Nein-Stimmen: ",nrow(count_non_gemeinden),"; Ja-Stimmen: ",nrow(count_yes_gemeinden),
               "; Unentschieden: ",nrow(count_tie_gemeinden)))
  
  #Datawrapper-Karten aktualisieren
  undertitel_overview_de <- "Es sind noch keine Gemeinden ausgezählt."
  undertitel_initiative_de <- "Es sind noch keine Gemeinden ausgezählt."
  undertitel_gegenvorschlag_de <- "Es sind noch keine Gemeinden ausgezählt."
  undertitel_stichentscheid_de <- "Es sind noch keine Gemeinden ausgezählt."
  undertitel_overview_fr <- "Aucun résultat n'est encore connu."
  undertitel_initiative_fr <- "Aucun résultat n'est encore connu."
  undertitel_gegenvorschlag_fr <- "Aucun résultat n'est encore connu."
  undertitel_stichentscheid_fr <- "Aucun résultat n'est encore connu."
  
  hold <- FALSE
  if (hold == FALSE) {
    if (is.na(results_kantonal_special_initiative) == FALSE) {
      
      
      undertitel_overview_de <- paste0("Die brieflichen Stimmen sind ausgezählt." )
      
      undertitel_initiative_de <- paste0("Die brieflichen Stimmen sind ausgezählt.<br>Stand Initiative: <b>",
                              round2(results_kantonal_special_initiative,1)," %</b> Ja, <b>",
                              round2(100-results_kantonal_special_initiative,1)," %</b> Nein")
      
      undertitel_gegenvorschlag_de <- paste0("Die brieflichen Stimmen sind ausgezählt.<br>Stand Gegenvorschlag: <b>",
                              round2(results_kantonal_special_gegenvorschlag,1)," %</b> Ja, <b>",
                              round2(100-results_kantonal_special_gegenvorschlag,1)," %</b> Nein")
      
      undertitel_stichentscheid_de <- paste0("Die brieflichen Stimmen sind ausgezählt.<br>Stand Stichentscheid: <b>",
                                             round2(results_kantonal_special_stichentscheid,1)," %</b> Initiative, <b>",
                                             round2(100-results_kantonal_special_stichentscheid,1)," %</b> Gegenvorschlag")
      
      undertitel_overview_fr <- paste0("Les votes par correspondance ont été dépouillés")
      
      undertitel_initiative_fr <- paste0("Les votes par correspondance ont été dépouillés.<br>Etat initiative: <b>",
                                         round2(results_kantonal_special_initiative,1)," %</b> oui, <b>",
                                         round2(100-results_kantonal_special_initiative,1)," %</b> non")
      
      undertitel_gegenvorschlag_fr <- paste0("Les votes par correspondance ont été dépouillés.<br>Etat contre-proposition: <b>",
                                             round2(results_kantonal_special_gegenvorschlag,1)," %</b> oui, <b>",
                                             round2(100-results_kantonal_special_gegenvorschlag,1)," %</b> non")
      
      undertitel_stichentscheid_fr <- paste0("Les votes par correspondance ont été dépouillés.<br>Etat question subsidiaire: <b>",
                                             round2(results_kantonal_special_stichentscheid,1)," %</b> initiative, <b>",
                                             round2(100-results_kantonal_special_stichentscheid,1)," %</b> contre-proposition")
      
      if (nrow(results_notavailable) == 0) {
        
        undertitel_overview_de <-  paste0("Resultat Initiative: <b>",
                                                                round2(results_kantonal_special_initiative,1)," %</b> Ja, <b>",
                                                                round2(100-results_kantonal_special_initiative,1)," %</b> Nein<br>",
                                                                "Resultat Gegenvorschlag: <b>",
                                                                round2(results_kantonal_special_gegenvorschlag,1)," %</b> Ja, <b>",
                                                                round2(100-results_kantonal_special_gegenvorschlag,1)," %</b> Nein<br>",
                                                                "Resultat Stichentscheid: <b>",
                                                                round2(results_kantonal_special_stichentscheid,1)," %</b> Initiative, <b>",
                                                                round2(100-results_kantonal_special_stichentscheid,1)," %</b> Gegenvorschlag"
        )
        
        undertitel_initiative_de <- paste0("Resultat Initiative: <b>",
                                           round2(results_kantonal_special_initiative,1)," %</b> Ja, <b>",
                                           round2(100-results_kantonal_special_initiative,1)," %</b> Nein")
        undertitel_gegenvorschlag_de <- paste0("Resultat Gegenvorschlag: <b>",
                                               round2(results_kantonal_special_gegenvorschlag,1)," %</b> Ja, <b>",
                                               round2(100-results_kantonal_special_gegenvorschlag,1)," %</b> Nein")
        undertitel_stichentscheid_de<- paste0("Resultat Stichentscheid: <b>",
                                              round2(results_kantonal_special_stichentscheid,1)," %</b> Initiative, <b>",
                                              round2(100-results_kantonal_special_stichentscheid,1)," %</b> Gegenvorschlag")
        
        undertitel_overview_fr <- paste0("Résultats initiative: <b>",
                                         round2(results_kantonal_special_initiative,1)," %</b> oui, <b>",
                                         round2(100-results_kantonal_special_initiative,1)," %</b> non<br>",
                                         "Résultats contre-proposition: <b>",
                                         round2(results_kantonal_special_gegenvorschlag,1)," %</b> oui, <b>",
                                         round2(100-results_kantonal_special_gegenvorschlag,1)," %</b> non<br>",
                                         "Résultats question subsidiaire: <b>",
                                         round2(results_kantonal_special_stichentscheid,1)," %</b> initiative, <b>",
                                         round2(100-results_kantonal_special_stichentscheid,1)," %</b> contre-proposition"
        )
        
        undertitel_initiative_fr <- paste0("Résultats initiative: <b>",
                                           round2(results_kantonal_special_initiative,1)," %</b> oui, <b>",
                                           round2(100-results_kantonal_special_initiative,1)," %</b> non")
        undertitel_gegenvorschlag_fr <- paste0("Résultats contre-proposition: <b>",
                                               round2(results_kantonal_special_gegenvorschlag,1)," %</b> oui, <b>",
                                               round2(100-results_kantonal_special_gegenvorschlag,1)," %</b> non")
        uundertitel_stichentscheid_fr <- paste0("Résultats question subsidiaire: <b>",
                                                round2(results_kantonal_special_stichentscheid,1)," %</b> initiative, <b>",
                                                round2(100-results_kantonal_special_stichentscheid,1)," %</b> contre-proposition")
        
      } else if  (sum(results$Gebiet_Ausgezaehlt) > 0 ) {
        
        undertitel_overview_de <- paste0("Es sind <b>",sum(results$Gebiet_Ausgezaehlt),"</b> von <b>",nrow(results),
                                           "</b> Gemeinden ausgezählt.")  
        
      undertitel_initiative_de <- paste0("Es sind <b>",sum(results$Gebiet_Ausgezaehlt),"</b> von <b>",nrow(results),
                              "</b> Gemeinden ausgezählt.<br>Stand Initiative: <b>",
                              round2(results_kantonal_special_initiative,1)," %</b> Ja, <b>",
                              round2(100-results_kantonal_special_initiative,1)," %</b> Nein")
      undertitel_gegenvorschlag_de <- paste0("Es sind <b>",sum(results$Gebiet_Ausgezaehlt),"</b> von <b>",nrow(results),
                                         "</b> Gemeinden ausgezählt.<br>Stand Gegenvorschlag: <b>",
                                         round2(results_kantonal_special_gegenvorschlag,1)," %</b> Ja, <b>",
                                         round2(100-results_kantonal_special_gegenvorschlag,1)," %</b> Nein")
      undertitel_stichentscheid_de<- paste0("Es sind <b>",sum(results$Gebiet_Ausgezaehlt),"</b> von <b>",nrow(results),
                                         "</b> Gemeinden ausgezählt.<br>Stand Stichentscheid: <b>",
                                         round2(results_kantonal_special_stichentscheid,1)," %</b> Initiative, <b>",
                                         round2(100-results_kantonal_special_stichentscheid,1)," %</b> Gegenvorschlag")
      
      undertitel_overview_fr <- paste0("Les résultats de <b>",sum(results$Gebiet_Ausgezaehlt),"</b> des <b>",nrow(results),
                                         "</b> communes sont connus.")
      
      undertitel_initiative_fr <- paste0("Les résultats de <b>",sum(results$Gebiet_Ausgezaehlt),"</b> des <b>",nrow(results),
                              "</b> communes sont connus.<br>Etat initiative: <b>",
                              round2(results_kantonal_special_initiative,1)," %</b> oui, <b>",
                              round2(100-results_kantonal_special_initiative,1)," %</b> non")
      undertitel_gegenvorschlag_fr <- paste0("Les résultats de <b>",sum(results$Gebiet_Ausgezaehlt),"</b> des <b>",nrow(results),
                              "</b> communes sont connus.<br>Etat contre-proposition: <b>",
                              round2(results_kantonal_special_gegenvorschlag,1)," %</b> oui, <b>",
                              round2(100-results_kantonal_special_gegenvorschlag,1)," %</b> non")
      uundertitel_stichentscheid_fr <- paste0("Les résultats de <b>",sum(results$Gebiet_Ausgezaehlt),"</b> des <b>",nrow(results),
                              "</b> communes sont connus.<br>Etat question subsidiaire: <b>",
                              round2(results_kantonal_special_stichentscheid,1)," %</b> initiative, <b>",
                              round2(100-results_kantonal_special_stichentscheid,1)," %</b> contre-proposition")
    }
#    if (kantonal_short_special[s] == "VS_Verfassung") {
#      undertitel_de <- gsub("Initiative","Entwurf",undertitel_de)
#      undertitel_de <- gsub("Gegenvorschlag","Variante",undertitel_de)
#      undertitel_fr <- gsub("initiative","project",undertitel_fr)
#      undertitel_fr <- gsub("contre[-]proposition","variante",undertitel_fr)
#    }
      if (sum(results$Gebiet_Ausgezaehlt) == nrow(results)) {
        
        #Set mail output to done
        mydb <- connectDB(db_name = "sda_votes")  
        sql_qry <- paste0("UPDATE votes_metadata SET status = 'done' WHERE date = '",voting_date,"' AND spreadsheet = '",kantonal_short_special[s],"'")
        rs <- dbSendQuery(mydb, sql_qry)
        dbDisconnectAll() 

        print(paste0("Alle Daten der Abstimmung ",kantonal_short_special[s]," vorhanden"))
      }
    }    
    
    ###Karten Gemeinden
    
    datawrapper_codes_vorlage_overview <- datawrapper_codes_kantonal[datawrapper_codes_kantonal$Vorlage == kantonal_short_special[s] &
                                                                         datawrapper_codes_kantonal$Typ == "Kantonale Vorlage Overview",]
    datawrapper_codes_vorlage_initiative <- datawrapper_codes_kantonal[datawrapper_codes_kantonal$Vorlage == kantonal_short_special[s] &
                                                              datawrapper_codes_kantonal$Typ == "Kantonale Vorlage Initiative",]
    datawrapper_codes_vorlage_gegenvorschlag <- datawrapper_codes_kantonal[datawrapper_codes_kantonal$Vorlage == kantonal_short_special[s] &
                                                              datawrapper_codes_kantonal$Typ == "Kantonale Vorlage Gegenvorschlag",]
    datawrapper_codes_vorlage_stichentscheid <- datawrapper_codes_kantonal[datawrapper_codes_kantonal$Vorlage == kantonal_short_special[s] &
                                                              datawrapper_codes_kantonal$Typ == "Kantonale Vorlage Stichentscheid",]
    
    
    for (r in 1:nrow(datawrapper_codes_vorlage_overview)) {
      if (datawrapper_codes_vorlage_overview$Sprache[r] == "de-DE") {
        dw_edit_chart(datawrapper_codes_vorlage_overview[r,5],
                      intro=paste0(undertitel_overview_de,'<br>
<span style="line-height:30px">
  <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_overview[r,5],'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Übersicht&nbsp;&nbsp;</a> &nbsp;
  <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_initiative[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Initiative&nbsp;&nbsp;</a> &nbsp;
                                     <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_gegenvorschlag[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Gegenvorschlag</a> &nbsp;
                                   <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_stichentscheid[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Stichentscheid</a> &nbsp;'),
                      annotate=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
        dw_publish_chart(datawrapper_codes_vorlage_overview[r,5])
      } else {
        dw_edit_chart(datawrapper_codes_vorlage_overview[r,5],
                      intro=paste0(undertitel_overview_fr,'<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_overview[r,5],'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;aperçu&nbsp;&nbsp;</a> &nbsp;
  <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_initiative[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;initiative&nbsp;&nbsp;</a> &nbsp;
                                     <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_gegenvorschlag[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> contre-proposition</a> &nbsp;
                                   <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_stichentscheid[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> question subsidiaire</a> &nbsp;'),
                      annotate=paste0("dernière mise à jour: ",format(Sys.time(),"%d.%m.%Y %Hh%M")))
        dw_publish_chart(datawrapper_codes_vorlage_overview[r,5])
      }
    }
    
    
    
    for (r in 1:nrow(datawrapper_codes_vorlage_initiative)) {
      if (datawrapper_codes_vorlage_initiative$Sprache[r] == "de-DE") {
        dw_edit_chart(datawrapper_codes_vorlage_initiative[r,5],
                      intro=paste0(undertitel_initiative_de,'<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_overview[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Übersicht&nbsp;&nbsp;</a> &nbsp;
  <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_initiative[r,5],'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Initiative&nbsp;&nbsp;</a> &nbsp;
                                     <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_gegenvorschlag[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Gegenvorschlag</a> &nbsp;
                                   <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_stichentscheid[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Stichentscheid</a> &nbsp;'),
                      annotate=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
        dw_publish_chart(datawrapper_codes_vorlage_initiative[r,5])
      } else {
        dw_edit_chart(datawrapper_codes_vorlage_initiative[r,5],
                      intro=paste0(undertitel_initiative_fr,'<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_overview[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;aperçu&nbsp;&nbsp;</a> &nbsp;
  <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_initiative[r,5],'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;initiative&nbsp;&nbsp;</a> &nbsp;
                                     <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_gegenvorschlag[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> contre-proposition</a> &nbsp;
                                   <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_stichentscheid[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> question subsidiaire</a> &nbsp;'),
                      annotate=paste0("dernière mise à jour: ",format(Sys.time(),"%d.%m.%Y %Hh%M")))
        dw_publish_chart(datawrapper_codes_vorlage_initiative[r,5])
      }
    }

    for (r in 1:nrow(datawrapper_codes_vorlage_gegenvorschlag)) {
      if (datawrapper_codes_vorlage_gegenvorschlag$Sprache[r] == "de-DE") {
        dw_edit_chart(datawrapper_codes_vorlage_gegenvorschlag[r,5],
                      intro=paste0(undertitel_gegenvorschlag_de,'<br>
                        <span style="line-height:30px">
                        <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_overview[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Übersicht&nbsp;&nbsp;</a> &nbsp;
                        <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_initiative[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Initiative&nbsp;&nbsp;</a> &nbsp;
                      <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_gegenvorschlag[r,5],'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Gegenvorschlag</a> &nbsp;
                      <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_stichentscheid[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Stichentscheid</a> &nbsp;'),
                      annotate=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
        dw_publish_chart(datawrapper_codes_vorlage_gegenvorschlag[r,5])
      } else {
        dw_edit_chart(datawrapper_codes_vorlage_gegenvorschlag[r,5],
                      intro=paste0(undertitel_gegenvorschlag_fr,'<br>
                        <span style="line-height:30px">
                        <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_overview[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;aperçu&nbsp;&nbsp;</a> &nbsp;
                        <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_initiative[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;initiative&nbsp;&nbsp;</a> &nbsp;
                      <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_gegenvorschlag[r,5],'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> contre-proposition</a> &nbsp;
                      <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_stichentscheid[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> question subsidiaire</a> &nbsp;'),
                      annotate=paste0("dernière mise à jour: ",format(Sys.time(),"%d.%m.%Y %Hh%M")))
        dw_publish_chart(datawrapper_codes_vorlage_gegenvorschlag[r,5])
      }
    }

    for (r in 1:nrow(datawrapper_codes_vorlage_stichentscheid)) {
      if (datawrapper_codes_vorlage_stichentscheid$Sprache[r] == "de-DE") {
        dw_edit_chart(datawrapper_codes_vorlage_stichentscheid[r,5],
                      intro=paste0(undertitel_stichentscheid_de,'<br>
                        <span style="line-height:30px">
                        <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_overview[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Übersicht&nbsp;&nbsp;</a> &nbsp;
                        <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_initiative[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Initiative&nbsp;&nbsp;</a> &nbsp;
                      <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_gegenvorschlag[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Gegenvorschlag</a> &nbsp;
                      <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_stichentscheid[r,5],'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Stichentscheid</a> &nbsp;'),
                      annotate=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
        dw_publish_chart(datawrapper_codes_vorlage_stichentscheid[r,5])
      } else {
        dw_edit_chart(datawrapper_codes_vorlage_stichentscheid[r,5],
                      intro=paste0(undertitel_stichentscheid_fr,'<br>
                        <span style="line-height:30px">
                        <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_overview[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;aperçu&nbsp;&nbsp;</a> &nbsp;
                        <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_initiative[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;initiative&nbsp;&nbsp;</a> &nbsp;
                      <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_gegenvorschlag[r,5],'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> contre-proposition</a> &nbsp;
                      <a target="_self" href="https://datawrapper.dwcdn.net/',datawrapper_codes_vorlage_stichentscheid[r,5],'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> question subsidiaire<</a> &nbsp;'),
                      annotate=paste0("dernière mise à jour: ",format(Sys.time(),"%d.%m.%Y %Hh%M")))
        dw_publish_chart(datawrapper_codes_vorlage_stichentscheid[r,5])
      }
    }
  }  
  }
}

