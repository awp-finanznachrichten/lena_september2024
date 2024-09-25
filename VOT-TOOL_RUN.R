MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20240922_LENA_Abstimmungen"

#Working Directory definieren
setwd(MAIN_PATH)

#Load Libraries and Functions
source("./Config/load_libraries_functions.R",encoding = "UTF-8")

###Set Constants###
source("./Config/set_constants.R",encoding = "UTF-8")

#SET ADDITIONAL CONSTANTS
VOTATION_IDS_SRG <- c(5081,5082)
CATCHWORDS_DE <- c("Umwelt","Altersvorsorge")

###Load texts and metadata###
source("./Config/load_texts_metadata.R",encoding = "UTF-8")

repeat{
###Load JSON Data
source("./Config/load_json_data.R",encoding = "UTF-8")

#SRG Hochrechnungen -> Generate Flashes and send Mail if new data available
source("./Vot-Tool/SRG_API_Request.R", encoding = "UTF-8")

###Write Data in DB###
source("./Vot-Tool/write_DB_entries.R", encoding = "UTF-8")

###Load DBs
source("./Vot-Tool/load_DBs.R", encoding = "UTF-8")


#####CREATE NEWS AND SEND MAIL IF CANTON IS COMPLETE#####
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
  
#####CREATE FLASH AM STÄNDEMEHR GESCHEITERT (IF NEEDED)####
for (v in 1:nrow(vorlagen)) {
if (output_flashes[output_flashes$votes_ID == vorlagen$id[v],]$flash_staendemehr == "pending") {  
source("./Vot-Tool/create_flash_staendemehr.R", encoding = "UTF-8")  
}  
}  

#####CREATE INTERMEDIATE NEWS####
if (Sys.time() > output_news_intermediate$timestamp[1]) {
  print("Generiere Zwischenstands-Meldungen...")
  source("./Vot-Tool/create_news_intermediate.R", encoding="UTF-8") 
}

#####CREATE ELECTION COMPLETED REPORT####
if (sum(grepl("pending",output_overview$news_results)) == 0) {
  print("Alle Abstimmungsresultate komplett!")
if (output_overview_national$mail_results[1] == "pending") { 
source("./Vot-Tool/send_mail_election_completed.R", encoding="UTF-8")
}  
if (output_overview_national$news_results[1] == "pending") { 
source("./Vot-Tool/create_news_election_completed.R", encoding="UTF-8") 
}   
}  
Sys.sleep(5)
}  

