###GET CURRENT RESULTS ###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, "SELECT * FROM cantons_results")
cantons_results <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

###CHECK FOR EACH VOTE IF THERE ARE NEW RESULTS AVAILABLE OR THERE WAS AN UPDATE
for (v in 1:nrow(vorlagen)) {
print(paste0("checking ",vorlagen$text[v]))
results_vorlage <- json_data$schweiz$vorlagen$kantone[[v]]
results_vorlage$geoLevelnummer <- as.numeric(results_vorlage$geoLevelnummer)
results_vorlage$resultat.jaStimmenInProzent <- round2(results_vorlage$resultat.jaStimmenInProzent,2)
results_vorlage$resultat.stimmbeteiligungInProzent <- round2(results_vorlage$resultat.stimmbeteiligungInProzent,2)
results_vorlage[is.na(results_vorlage)] <- 0

results_all <- cantons_results %>%
  filter(votes_ID == vorlagen$id[v]) %>%
  left_join(meta_kt) %>%
  left_join(results_vorlage,join_by(bfs_ID == geoLevelnummer))

#New entries available?
results_new <- results_all %>%
  filter(final_results == 0,
         resultat.gebietAusgezaehlt == TRUE)

if (nrow(results_new) > 0) {
print(paste0("new entries found for ",paste(results_new$area_ID, collapse = ", ")))  
mydb <- connectDB(db_name = "sda_votes")
for (i in 1:nrow(results_new)) {
    sql_qry <- paste0(
      "UPDATE cantons_results SET ",
      " result = '",
      ifelse(results_new$resultat.jaStimmenInProzent[i] >= 50,"yes","no"),
      "'",
      ", share_yes_percentage = '",
      results_new$resultat.jaStimmenInProzent[i],
      "'",
      ", share_yes_votes = '",
      results_new$resultat.jaStimmenAbsolut[i],
      "'",
      ", share_no_percentage = '",
      100-results_new$resultat.jaStimmenInProzent[i],
      "'",
      ", share_no_votes = '",
      results_new$resultat.neinStimmenAbsolut[i],
      "'",
      ", voter_participation = '",
      results_new$resultat.stimmbeteiligungInProzent[i],
      "'",
      ", final_results = 1, source_update = 'BFS'",
      " WHERE area_ID = '",
      results_new$area_ID[i],
      "' AND votes_ID = '",
      vorlagen$id[v],
      "'"
    )
    rs <- dbSendQuery(mydb, sql_qry)
  }
dbDisconnectAll()
} else {
print(paste0("no new data for ",vorlagen$text[v]," found"))
} 

#Current entries with different data?
results_check <- results_all %>%
  filter(final_results == 1,
         resultat.gebietAusgezaehlt == TRUE) %>%
  mutate(check_votes = resultat.jaStimmenInProzent == share_yes_percentage,
         check_participation = resultat.stimmbeteiligungInProzent == voter_participation)

changes_voter_share <- results_check %>%
  filter(check_votes == FALSE)


if (nrow(changes_voter_share) > 0) {
print(paste0("ATTENTION: adapted/corrected voter share found for ",paste(changes_voter_share$area_ID, collapse = ", ")))
  mydb <- connectDB(db_name = "sda_votes")
  for (i in 1:nrow(changes_voter_share)) {
    sql_qry <- paste0(
      "UPDATE cantons_results SET ",
      " result = '",
      ifelse(changes_voter_share$resultat.jaStimmenInProzent[i] >= 50,"yes","no"),
      "'",
      ", share_yes_percentage = '",
      changes_voter_share$resultat.jaStimmenInProzent[i],
      "'",
      ", share_yes_votes = '",
      changes_voter_share$resultat.jaStimmenAbsolut[i],
      "'",
      ", share_no_percentage = '",
      100-changes_voter_share$resultat.jaStimmenInProzent[i],
      "'",
      ", share_no_votes = '",
      changes_voter_share$resultat.neinStimmenAbsolut[i],
      "'",
      ", voter_participation = '",
      changes_voter_share$resultat.stimmbeteiligungInProzent[i],
      "'",
      ", final_results = 1, source_update = 'BFS'",
      " WHERE area_ID = '",
      changes_voter_share$area_ID[i],
      "' AND votes_ID = '",
      vorlagen$id[v],
      "'"
    )
    rs <- dbSendQuery(mydb, sql_qry)
    
    #Send Mail
    if (changes_voter_share$source_update[i] == "Vot-Tool") {
      Subject <- paste0("***TEST***ACHTUNG: BFS-Daten unterscheiden sich von den Vot-Tool-Daten zur Vorlage ",vorlagen$text[v]," im Kanton ",changes_voter_share$area_ID[i],"!")
      Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                     "Die Ergebnisse des BFS zur VOrlage ",vorlagen$text[v]," im Kanton ",changes_voter_share$area_ID[i]," unterscheiden sich von den Eingaben via Vot-Tool.\n\n",
                     "Neue Ergebnisse (BFS):\n",
                     "Ja-Anteil: ",changes_voter_share$resultat.jaStimmenInProzent[i],"%, Ja-Stimmen: ",changes_voter_share$resultat.jaStimmenAbsolut[i],"\n",
                     "Nein-Anteil: ",100-changes_voter_share$resultat.jaStimmenInProzent[i],"%, Nein-Stimmen: ",changes_voter_share$resultat.neinStimmenAbsolut[i],"\n\n",
                     "Bisherige Ergebnisse (Vot-Tool):\n",
                     "Ja-Anteil: ",changes_voter_share$share_yes_percentage[i],"%, Ja-Stimmen: ",changes_voter_share$share_yes_votes,"\n",
                     "Nein-Anteil: ",changes_voter_share$share_no_percentage[i],"%, Nein-Stimmen: ",changes_voter_share$share_no_votes,"\n\n",
                     "Liebe Grüsse\n\nLENA")
      send_notification(Subject,
                        Body,
                        paste0(DEFAULT_MAILS))  

    } else {

    Subject <- paste0("***TEST***ACHTUNG: Korrektur bei den Resultaten zur ",vorlagen$text[v]," im Kanton ",changes_voter_share$area_ID[i]," gefunden!")
    Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                   "Das BFS hat die Ergebnisse zur ",vorlagen$text[v]," im Kanton ",changes_voter_share$area_ID[i]," korrigiert.\n\n",
                   "Neue Ergebnisse:\n",
                   "Ja-Anteil: ",changes_voter_share$resultat.jaStimmenInProzent[i],"%, Ja-Stimmen: ",changes_voter_share$resultat.jaStimmenAbsolut[i],"\n",
                   "Nein-Anteil: ",100-changes_voter_share$resultat.jaStimmenInProzent[i],"%, Nein-Stimmen: ",changes_voter_share$resultat.neinStimmenAbsolut[i],"\n\n",
                   "Bisherige Ergebnisse:\n",
                   "Ja-Anteil: ",changes_voter_share$share_yes_percentage[i],"%, Ja-Stimmen: ",changes_voter_share$share_yes_votes,"\n",
                   "Nein-Anteil: ",changes_voter_share$share_no_percentage[i],"%, Nein-Stimmen: ",changes_voter_share$share_no_votes,"\n\n",
                   "Liebe Grüsse\n\nLENA")
    send_notification(Subject,
                      Body,
                      paste0(DEFAULT_MAILS))
    }
  }
  dbDisconnectAll()


  
}  

changes_participation <- results_check %>%
  filter(check_participation == FALSE)

if (nrow(changes_participation) > 0) {
  print(paste0("ATTENTION: adapted/corrected participation found for ",paste(changes_participation$area_ID, collapse = ", ")))
  mydb <- connectDB(db_name = "sda_votes")
  for (i in 1:nrow(changes_participation)) {
    sql_qry <- paste0(
      "UPDATE cantons_results SET ",
      " voter_participation = '",
      changes_participation$resultat.stimmbeteiligungInProzent[i],
      "', final_results = 1, source_update = 'BFS'",
      " WHERE area_ID = '",
      changes_participation$area_ID[i],
      "' AND votes_ID = '",
      vorlagen$id[v],
      "'"
    )
    rs <- dbSendQuery(mydb, sql_qry)
    
    
    if (changes_participation$source_update[i] == "Vot-Tool") {
      #Send Mail
      Subject <- paste0("***TEST***ACHTUNG: BFS-Stimmbeteiligung unterscheidet sich von der Vot-Tool-Stimmbeteiligung zur Vorlage ",vorlagen$text[v]," im Kanton ",changes_participation$area_ID[i],"!")
      Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                     "Die Stimmbeteiligung des BFS zur Vorlage ",vorlagen$text[v]," im Kanton ",changes_participation$area_ID[i]," unterscheiden sich von den Eingaben via Vot-Tool.\n\n",
                     "Neue Stimmbeteiligung (BFS): ",changes_participation$resultat.stimmbeteiligungInProzent[i],"%\n",
                     "Bisherige Stimmbeteiligung (Vot-Tool): ",changes_participation$voter_participation[i],"%\n\n",
                     "Liebe Grüsse\n\nLENA")
      send_notification(Subject,
                        Body,
                        paste0(DEFAULT_MAILS))  
      
    } else {  
    #Send Mail
    Subject <- paste0("***TEST***ACHTUNG: Korrektur bei der Stimmbeteiligung zur ",vorlagen$text[v]," im Kanton ",changes_participation$area_ID[i]," gefunden!")
    Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                   "Das BFS hat die Stimmbeteiligung zur ",vorlagen$text[v]," im Kanton ",changes_participation$area_ID[i]," korrigiert.\n\n",
                   "Neue Stimmbeteiligung: ",changes_participation$resultat.stimmbeteiligungInProzent[i],"%\n",
                   "Bisherige Stimmbeteiligung: ",changes_participation$voter_participation[i],"%\n\n",
                   "Liebe Grüsse\n\nLENA")
    send_notification(Subject,
                      Body,
                      paste0(DEFAULT_MAILS))
    }
    
  }
  dbDisconnectAll()
}  



}  