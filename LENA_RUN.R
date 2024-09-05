#Set Working Path
MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20240922_LENA_Abstimmungen"
setwd(MAIN_PATH)

#Load Libraries and Functions
source("./Config/load_libraries_functions.R",encoding = "UTF-8")

###Set Constants###
source("./Config/set_constants.R",encoding = "UTF-8")

###Load texts and metadata###
source("./Config/load_texts_metadata.R",encoding = "UTF-8")

#####START LOOP#####
repeat{
###Load JSON Data
source("./Config/load_json_data.R",encoding = "UTF-8")

#Simulate Data (if needed)
if (simulation == TRUE) {
source("./Simulation/data_simulation.R")  
}  

#Aktualisierungs-Check: Gibt es neue Daten?
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM timestamps"))
timestamps <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

timestamp_national <- timestamps$last_update[2]
timestamp_kantonal <- timestamps$last_update[1]
time_check_national <- timestamp_national == json_data$timestamp
time_check_kantonal <- timestamp_kantonal == json_data_kantone$timestamp

if ((time_check_national == TRUE) & (time_check_kantonal == TRUE)) {
print("Keine neuen Daten gefunden")  
} else {
print("Neue Daten gefunden")
time_start <- Sys.time()

if (time_check_national == FALSE) {
  
  ###Nationale Abstimmungen###
  source("./Nationale_Abstimmungen/nationale_abstimmungen.R", encoding="UTF-8")
  
  #Make Commit
  source("./Config/commit.R", encoding="UTF-8")
  
  #Tabellen aktualisieren
  #source("votations_juin_2024/top_flop/top_flop_run.R", encoding="UTF-8")
}

if (time_check_kantonal == FALSE) {  
  ###Kantonale Abstimmungen Uebersicht  
  source("./Kantonale_Abstimmungen/kantonale_abstimmungen_uebersicht.R", encoding="UTF-8")
  
  ###Kantonale Abstimmungen###
  if (length(kantonal_number) > 0) {
  source("./Kantonale_Abstimmungen/kantonale_abstimmungen.R", encoding="UTF-8")
  }

  ###Kantonale Abstimmungen Sonderfälle###
  if (length(kantonal_number_special) > 0) {
  source("./Kantonale_Abstimmungen/kantonale_abstimmungen_special.R", encoding="UTF-8")
  }
  #Make Commit
  source("./Config/commit.R", encoding="UTF-8")
}

#Timestamp speichern
mydb <- connectDB(db_name = "sda_votes")  
sql_qry <- paste0("UPDATE timestamps SET last_update = '",json_data$timestamp,"' WHERE data_type = 'results_national'")
rs <- dbSendQuery(mydb, sql_qry)
sql_qry <- paste0("UPDATE timestamps SET last_update = '",json_data_kantone$timestamp,"' WHERE data_type = 'results_cantonal'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 

#Wie lange hat LENA gebraucht
time_end <- Sys.time()
cat(time_end-time_start)
}

#Sys.sleep(10)
}

