MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20240922_LENA_Abstimmungen"

#Working Directory definieren
setwd(MAIN_PATH)

###Funktionen laden
source("./Funktionen/functions_readin.R", encoding = "UTF-8")
source("./Funktionen/functions_github.R", encoding = "UTF-8")
source("./tools/Funktionen/Utils.R", encoding = "UTF-8")

repeat{
###Config: Bibliotheken laden, Pfade/Links definieren, bereits vorhandene Daten laden
source("config.R",encoding = "UTF-8")

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