mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM output_overview WHERE date = '",voting_date,"' AND area_ID = 'CH'"))
output_overview <- DBI::fetch(rs,n=-1)
dbDisconnectAll()
if ((output_overview$mail_results[1] == "pending") & (sum(json_data[["schweiz"]][["vorlagen"]][["vorlageBeendet"]] == FALSE) == 0) ) {
  print("Alle Abstimmungsresultate komplett!")
  source("./Vot-Tool/report_election_completed.R", encoding="UTF-8") 
}  

