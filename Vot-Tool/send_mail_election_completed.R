Subject <- paste0("Eidgenössische Abstimmungen: Es sind alle Resultate bekannt")
Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
               "Die Ergebnisse der eidgenössischen Abstimmungen sind bekannt.\n\n",
               "Die Resultate:\n",
               paste(paste0(vorlagen$text,": ",gsub("[.]",",",format(round2(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenInProzent"]],2),nsmall=2)),"% Ja, ",
                            gsub("[.]",",",format(100-round2(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenInProzent"]],2),nsmall=2)),"% Nein"),collapse = "\n"),
               "\n\nDie schweizweite Stimmbeteiligung der Vorlagen:\n",
               paste(paste0(vorlagen$text,": ",round2(json_data[["schweiz"]][["vorlagen"]][["resultat.stimmbeteiligungInProzent"]],2),"%"),collapse = "\n"),
               "\n\nDie Kantons- und Gemeindekarten werden in Kürze ins Visual hochgeladen und allfällige Analysen bereitgestellt.\n\n",
               "Liebe Grüsse\n\nLENA")

send_notification(Subject,
                  Body,
                  paste0(DEFAULT_MAILS,",inland@keystone-sda.ch,suisse@keystone-ats.ch"))

#Set mail output to done
mydb <- connectDB(db_name = "sda_votes")  
sql_qry <- paste0("UPDATE output_overview SET mail_results = 'done' WHERE date = '",voting_date,"' AND voting_type = 'national' AND area_ID = 'CH'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 
