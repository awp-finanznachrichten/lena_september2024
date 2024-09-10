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
                      "('",voting_date,"','",meta_kt$area_ID[k],"','national')")
    rs <- dbSendQuery(mydb, sql_qry)
  }



kantone_list <- json_data_kantone[["kantone"]]

for (k in 1:nrow(kantone_list)) {
  sql_qry <- paste0("INSERT IGNORE INTO output_overview(date,area_ID,voting_type) VALUES ",
                    "('",voting_date,"','",kantone_list$geoLevelname[k],"','cantonal')")
  rs <- dbSendQuery(mydb, sql_qry)
}


dbDisconnectAll()


###ADD CH-Entry
mydb <- connectDB(db_name = "sda_votes")
sql_qry <- paste0("INSERT IGNORE INTO output_overview(date,area_ID,voting_type) VALUES ",
                    "('",voting_date,"','CH','national')")
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

#Enter Metadata from Spreadsheet
metadata <- as.data.frame(read_excel(paste0("Texte/Textbausteine_LENA_",abstimmung_date,".xlsx"), 
                                          sheet = "Vorlagen_Uebersicht"))

metadata$Vorlage_d <- str_replace_all(metadata$Vorlage_d ,"'","\\\\'")
metadata$Vorlage_f <- str_replace_all(metadata$Vorlage_f ,"'","\\\\'")
metadata$Vorlage_i <- str_replace_all(metadata$Vorlage_i ,"'","\\\\'")

mydb <- connectDB(db_name = "sda_votes")
for (m in 1:nrow(metadata)) {
  sql_qry <- paste0("INSERT IGNORE INTO votes_metadata(votes_ID,date,area_ID,title_de,title_fr,title_it,status) VALUES ",
                    "('",metadata$Vorlage_ID[m],"','",date_voting,"','",metadata$Kanton[m],"','",metadata$Vorlage_d[m],"','",metadata$Vorlage_f[m],"','",metadata$Vorlage_i[m],"','upcoming')")
  rs <- dbSendQuery(mydb, sql_qry)
}


