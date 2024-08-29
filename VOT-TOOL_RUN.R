MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20240922_LENA_Abstimmungen"

#Working Directory definieren
setwd(MAIN_PATH)

#Load Libraries and Functions
source("./Config/load_libraries_functions.R",encoding = "UTF-8")

###Set Constants###
source("./Config/set_constants.R",encoding = "UTF-8")

###Load texts and metadata###
source("./Config/load_texts_metadata.R",encoding = "UTF-8")

repeat{
###Load JSON Data
source("./Config/load_json_data.R",encoding = "UTF-8")

#SRG Hochrechnungen
source("./Vot-Tool/SRG_API_Request.R", encoding = "UTF-8")

###Write Data in DB###
source("./Vot-Tool/write_DB_entries.R", encoding = "UTF-8")

#####CREATE NEWS AND SEND MAIL IF CANTON IS COMPLETE#####
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

###GET OUTPUT NEWS INTERMEDIATE###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, "SELECT * FROM output_news_intermediate WHERE news_intermediate = 'pending'")
output_news_intermediate <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

###GET EXTRAPOLATIONS###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM extrapolations"))
extrapolations <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

  for (i in 1:nrow(output_overview)) {
    canton_results <- cantons_results %>%
      filter(area_ID == output_overview$area_ID[i]) 
    
    canton_metadata <- meta_kt %>%
      filter(area_ID == output_overview$area_ID[i])

    if (sum(canton_results$final_results) == nrow(vorlagen)) {
      #SEND MAIL
      if (output_overview$mail_results[i] == "pending") {
      print(paste0("sending mail with results of canton ",canton_results$area_ID[1],"..."))
      source("./Vot-Tool/send_mail_cantonal.R", encoding="UTF-8")
      }  
      
      #CREATE MARS NEWS
      if (output_overview$news_results[i] == "pending") {
      print(paste0("creating mars news with results of canton ",canton_results$area_ID[1],"..."))
      source("./Vot-Tool/create_news_cantonal.R", encoding="UTF-8") 
      }  
    }
  }  
  

#Zwischenstand?
if (Sys.time() > output_news_intermediate$timestamp[1]) {
  print("Generiere Zwischenstands-Meldungen...")
  source("./Vot-Tool/create_news_intermediate.R", encoding="UTF-8") 
}


#Abstimmung komplett?
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM output_overview WHERE date = '",voting_date,"' AND area_ID = 'CH'"))
output_overview <- DBI::fetch(rs,n=-1)
dbDisconnectAll()
if ((output_overview$mail_results[1] == "pending") & (sum(json_data[["schweiz"]][["vorlagen"]][["vorlageBeendet"]] == FALSE) == 0) ) {
  print("Alle Abstimmungsresultate komplett!")
  source("./Vot-Tool/create_report_election_completed.R", encoding="UTF-8") 
}  
Sys.sleep(5)
}  