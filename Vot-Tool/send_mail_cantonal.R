
text_results <- ""
for (c in 1:nrow(canton_results)) {
  text_results <- paste0(text_results,vorlagen$text[c],": ",ifelse(canton_results$result[c] == "yes","JA","NEIN"),"\n",
                         "Ja-Anteil: ",canton_results$share_yes_percentage[c],"% (",format(canton_results$share_yes_votes[c],big.mark = "'")," Stimmen)\n",
                         "Nein-Anteil: ",canton_results$share_no_percentage[c],"% (",format(canton_results$share_no_votes[c],big.mark = "'")," Stimmen)\n",
                         "Stimmbeteiligung: ",canton_results$voter_participation[c],"%\n\n")
}  

#Send Mail
Subject <- paste0("***TEST***Kanton ",canton_results$area_ID[1],": Abstimmungsergebnisse komplett")
Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
               "Die Ergebnisse zu den eidgenössischen Abstimmungen vom Kanton ",canton_results$area_ID[1]," sind komplett.\n\n",
               text_results,
               "Ihr findet die Meldungen im Mars im Input-Ordner Lena.\n\n",
               "Liebe Grüsse\n\nLENA")
send_notification(Subject,
                  Body,
                  paste0(DEFAULT_MAILS))  

#Set mail output to done
mydb <- connectDB(db_name = "sda_votes")  
sql_qry <- paste0("UPDATE output_overview SET mail_results = 'done' WHERE date = '",voting_date,"' AND voting_type = 'national' AND area_ID = '",output_overview$area_ID[i],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 


