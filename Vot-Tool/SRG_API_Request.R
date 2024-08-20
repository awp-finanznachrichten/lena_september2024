votation_ids <- c(5005,5006,5007,5008)

###GET CURRENT RESULTS ###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, "SELECT * FROM extrapolations")
extrapolations <- DBI::fetch(rs,n=-1)
dbDisconnectAll()
extrapolations$last_update <- strptime(extrapolations$last_update, format = '%Y-%m-%d %H:%M:%S')

for (v in 1:length(votation_ids)) {

current_trend <- extrapolations %>%
  filter(votes_ID == vorlagen$id[v],
         type == "trend")
  
#Trend
link <- paste0("https://srgssr-prod.apigee.net/polis-api-internal/v2/Polis.Votations?apikey=a660OBYrTkO9dNaxb3ExzKMDFIqGOiH4&lang=de&votationid=",votation_ids[v],"&locationtypeid=1&dataConditionID=6")
data <- GET(link)
content <- read_xml(data)
timestamp <- strptime(xml_text(xml_find_all(content,".//LastUpdate")),format = '%Y-%m-%dT%H:%M:%S')

if (length(timestamp) > 0) { 
  if (timestamp != current_trend$last_update) {
print(paste0("New trend found for ",vorlagen$text[v]))
trend <- xml_text(xml_find_all(content,".//ResultCondition"))

#Write in DB
mydb <- connectDB(db_name = "sda_votes")
sql_qry <- paste0(
  "UPDATE extrapolations SET ",
  " result = '",
  trend,
  "'",
  ", last_update = '",
  toString(timestamp),
  "'",
  " WHERE votes_ID = '",
  vorlagen$id[v],
  "' AND type = 'trend'"
)
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll()

#Send Mail
Subject <- paste0("***TEST***Neuer SRG-Trend zur ",vorlagen$text[v]," veröffentlicht!")
Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
               "Die SRG hat einen Trend zur ",vorlagen$text[v]," veröffentlicht.\n\n",
               "Trend: ",trend,"\n",
               "Veröffentlichungszeitpunkt: ",timestamp,"\n\n",
               "Liebe Grüsse\n\nLENA")
send_notification(Subject,
                  Body,
                  paste0(DEFAULT_MAILS))
}
}

#Hochrechnung 1
current_extrapolation <- extrapolations %>%
  filter(votes_ID == vorlagen$id[v],
         type == "extrapolation 1")

link <- paste0("https://srgssr-prod.apigee.net/polis-api-internal/v2/Polis.Votations?apikey=a660OBYrTkO9dNaxb3ExzKMDFIqGOiH4&lang=de&votationid=",votation_ids[v],"&locationtypeid=1&dataConditionID=4")
data <- GET(link)
content <- read_xml(data)
timestamp <- strptime(xml_text(xml_find_all(content,".//LastUpdate")),format = '%Y-%m-%dT%H:%M:%S')

if (length(timestamp) > 0) { 
  if (timestamp != current_extrapolation$last_update) {
  print(paste0("New extrapolation 1 found for ",vorlagen$text[v]))
  
  hochrechnung <- xml_text(xml_find_all(content,".//ResultCondition"))
  votes_yes <- as.numeric(xml_text(xml_find_all(content,".//Relative/Yes")))
  votes_no <- as.numeric(xml_text(xml_find_all(content,".//Relative/No")))
  
  #Write in DB
  
  mydb <- connectDB(db_name = "sda_votes")
  sql_qry <- paste0(
    "UPDATE extrapolations SET ",
    " result = '",
    hochrechnung,"'",
    ", share_votes_yes = '",
    votes_yes,
    "'",
    ", share_votes_no = '",
    votes_no,
    "'",
    ", last_update = '",
    toString(timestamp),
    "'",
    " WHERE votes_ID = '",
    vorlagen$id[v],
    "' AND type = 'extrapolation 1'"
  )
  rs <- dbSendQuery(mydb, sql_qry)
  dbDisconnectAll()
  
  #Send Mail
  Subject <- paste0("***TEST***Neue SRG-Hochrechnung zur ",vorlagen$text[v]," veröffentlicht!")
  Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                 "Die SRG hat eine Hochrechnung zur ",vorlagen$text[v]," veröffentlicht.\n\n",
                 "Ergebnis: ",hochrechnung,"\n",
                 "Ja-Anteil: ",votes_yes,"%\n",
                 "Nein-Anteil: ",votes_no,"%\n",
                 "Veröffentlichungszeitpunkt: ",timestamp,"\n\n",
                 "Liebe Grüsse\n\nLENA")
  send_notification(Subject,
                    Body,
                    paste0(DEFAULT_MAILS))
  }
}


#Hochrechnung 2
current_extrapolation <- extrapolations %>%
  filter(votes_ID == vorlagen$id[v],
         type == "extrapolation 2")

link <- paste0("https://srgssr-prod.apigee.net/polis-api-internal/v2/Polis.Votations?apikey=a660OBYrTkO9dNaxb3ExzKMDFIqGOiH4&lang=de&votationid=",votation_ids[v],"&locationtypeid=1&dataConditionID=8")
data <- GET(link)
content <- read_xml(data)
timestamp <- strptime(xml_text(xml_find_all(content,".//LastUpdate")),format = '%Y-%m-%dT%H:%M:%S')

if (length(timestamp) > 0) { 
  if (timestamp != current_extrapolation$last_update) {
    print(paste0("New extrapolation 2 found for ",vorlagen$text[v]))
    
    hochrechnung <- xml_text(xml_find_all(content,".//ResultCondition"))
    votes_yes <- as.numeric(xml_text(xml_find_all(content,".//Relative/Yes")))
    votes_no <- as.numeric(xml_text(xml_find_all(content,".//Relative/No")))
    
    #Write in DB
    
    mydb <- connectDB(db_name = "sda_votes")
    sql_qry <- paste0(
      "UPDATE extrapolations SET ",
      " result = '",
      hochrechnung,"'",
      ", share_votes_yes = '",
      votes_yes,
      "'",
      ", share_votes_no = '",
      votes_no,
      "'",
      ", last_update = '",
      toString(timestamp),
      "'",
      " WHERE votes_ID = '",
      vorlagen$id[v],
      "' AND type = 'extrapolation 2'"
    )
    rs <- dbSendQuery(mydb, sql_qry)
    dbDisconnectAll()
    
    #Send Mail
    Subject <- paste0("***TEST***Zweite SRG-Hochrechnung zur ",vorlagen$text[v]," veröffentlicht!")
    Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                   "Die SRG hat eine zweite Hochrechnung zur ",vorlagen$text[v]," veröffentlicht.\n\n",
                   "Ergebnis: ",hochrechnung,"\n",
                   "Ja-Anteil: ",votes_yes,"%\n",
                   "Nein-Anteil: ",votes_no,"%\n",
                   "Veröffentlichungszeitpunkt: ",timestamp,"\n\n",
                   "Liebe Grüsse\n\nLENA")
    send_notification(Subject,
                      Body,
                      paste0(DEFAULT_MAILS))
  }
}

#Hochrechnung 3
current_extrapolation <- extrapolations %>%
  filter(votes_ID == vorlagen$id[v],
         type == "extrapolation 3")

link <- paste0("https://srgssr-prod.apigee.net/polis-api-internal/v2/Polis.Votations?apikey=a660OBYrTkO9dNaxb3ExzKMDFIqGOiH4&lang=de&votationid=",votation_ids[v],"&locationtypeid=1&dataConditionID=9")
data <- GET(link)
content <- read_xml(data)
timestamp <- strptime(xml_text(xml_find_all(content,".//LastUpdate")),format = '%Y-%m-%dT%H:%M:%S')

if (length(timestamp) > 0) { 
  if (timestamp != current_extrapolation$last_update) {
    print(paste0("New extrapolation 3 found for ",vorlagen$text[v]))
    
    hochrechnung <- xml_text(xml_find_all(content,".//ResultCondition"))
    votes_yes <- as.numeric(xml_text(xml_find_all(content,".//Relative/Yes")))
    votes_no <- as.numeric(xml_text(xml_find_all(content,".//Relative/No")))
    
    #Write in DB
    
    mydb <- connectDB(db_name = "sda_votes")
    sql_qry <- paste0(
      "UPDATE extrapolations SET ",
      " result = '",
      hochrechnung,"'",
      ", share_votes_yes = '",
      votes_yes,
      "'",
      ", share_votes_no = '",
      votes_no,
      "'",
      ", last_update = '",
      toString(timestamp),
      "'",
      " WHERE votes_ID = '",
      vorlagen$id[v],
      "' AND type = 'extrapolation 3'"
    )
    rs <- dbSendQuery(mydb, sql_qry)
    dbDisconnectAll()
    
    #Send Mail
    Subject <- paste0("***TEST***Dritte SRG-Hochrechnung zur ",vorlagen$text[v]," veröffentlicht!")
    Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                   "Die SRG hat eine dritte Hochrechnung zur ",vorlagen$text[v]," veröffentlicht.\n\n",
                   "Ergebnis: ",hochrechnung,"\n",
                   "Ja-Anteil: ",votes_yes,"%\n",
                   "Nein-Anteil: ",votes_no,"%\n",
                   "Veröffentlichungszeitpunkt: ",timestamp,"\n\n",
                   "Liebe Grüsse\n\nLENA")
    send_notification(Subject,
                      Body,
                      paste0(DEFAULT_MAILS))
  }
}
}
