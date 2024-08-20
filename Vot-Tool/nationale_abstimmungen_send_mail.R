###GET CURRENT RESULTS ###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, "SELECT * FROM cantons_results")
cantons_results <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

###GET OUTPUT OVERVIEW###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM output_overview WHERE date = '",voting_date,"' AND area_ID != 'CH' AND voting_type = 'national'"))
output_overview <- DBI::fetch(rs,n=-1)
dbDisconnectAll()


for (i in 1:nrow(output_overview)) {
  
canton_results <- cantons_results %>%
  filter(area_ID == output_overview$area_ID[i]) 

if ((sum(canton_results$final_results) == 4) & (output_overview$mail_results[i] == "pending")) {
print(paste0("sending mail with results of canton ",canton_results$area_ID[1],"..."))
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
                 "Liebe Grüsse\n\nLENA")
send_notification(Subject,
                    Body,
                    paste0(DEFAULT_MAILS))  
  
#Set mail output to done
mydb <- connectDB(db_name = "sda_votes")  
sql_qry <- paste0("UPDATE output_overview SET mail_results = 'done' WHERE date = '",voting_date,"' AND voting_type = 'national' AND area_ID = '",output_overview$area_ID[i],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 
}  
}  