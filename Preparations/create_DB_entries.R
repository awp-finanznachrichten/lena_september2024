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
    sql_qry <- paste0("INSERT IGNORE INTO output_overview(date,area_ID,voting_type,mail_results) VALUES ",
                      "('2024-06-09','",meta_kt$area_ID[k],"','national','pending')")
    rs <- dbSendQuery(mydb, sql_qry)
  }


dbDisconnectAll()

