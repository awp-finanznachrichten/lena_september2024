#Canton Results
mydb <- connectDB(db_name = "sda_votes")
for (i in 1:nrow(vorlagen)) {
  for (k in 1:nrow(meta_kt)) {
  sql_qry <- paste0("INSERT IGNORE INTO cantons_results(area_ID,votes_ID,final_results) VALUES ",
                    "('",meta_kt$area_ID[k],"','",vorlagen$id[i],"','0')")
  rs <- dbSendQuery(mydb, sql_qry)
  }
}

dbDisconnectAll()

#Output Overview
mydb <- connectDB(db_name = "sda_votes")
  for (k in 1:nrow(meta_kt)) {
    sql_qry <- paste0("INSERT IGNORE INTO output_overview(date,area_ID,voting_type) VALUES ",
                      "('",date_voting,"','",meta_kt$area_ID[k],"','national')")
    rs <- dbSendQuery(mydb, sql_qry)
  }


dbDisconnectAll()

###ADD CH-Entry
mydb <- connectDB(db_name = "sda_votes")
sql_qry <- paste0("INSERT IGNORE INTO output_overview(date,area_ID,voting_type) VALUES ",
                    "('",date_voting,"','CH','national')")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll()

#Extrapolations
types <- c("extrapolation 1","extrapolation 2","extrapolation 3","trend")
SRG_IDs <- c()
mydb <- connectDB(db_name = "sda_votes")
for (i in 1:nrow(vorlagen)) {
  for (type in types) {
  sql_qry <- paste0("INSERT IGNORE INTO extrapolations(votes_ID,type,SRG_ID) VALUES ",
                    "('",vorlagen$id[i],"','",type,"','",SRG_IDs[i],"')")
  rs <- dbSendQuery(mydb, sql_qry)
  }
}  