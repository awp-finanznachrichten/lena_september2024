#Bibliotheken laden
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(stringr)
library(stringi)
library(xml2)
library(rjson)
library(jsonlite)
library(readxl)
library(git2r)
library(DatawRappr)
library(lubridate)
library(httr)
cat("Benoetigte Bibliotheken geladen\n")

#Welche Abstimmung?
abstimmung_date <- "September2024"
voting_date <- "2024-09-22"

#Mail
DEFAULT_MAILS <- "contentdevelopment@keystone-sda.ch, robot-notification@awp.ch"
#DEFAULT_MAILS <- "robot-notification@awp.ch"

res <- GET("https://app-prod-static-voteinfo.s3.eu-central-1.amazonaws.com/v1/ogd/sd-t-17-02-20240609-eidgAbstimmung.json")
json_data <- fromJSON(rawToChar(res$content), flatten = TRUE)

res <- GET("https://app-prod-static-voteinfo.s3.eu-central-1.amazonaws.com/v1/ogd/sd-t-17-02-20240609-kantAbstimmung.json")
json_data_kantone <- fromJSON(rawToChar(res$content), flatten = TRUE)

cat("Aktuelle Abstimmungsdaten geladen\n")

excel_sheets <- excel_sheets(paste0("Texte/Textbausteine_LENA_",abstimmung_date,".xlsx"))

#Kantonale Vorlagen Titel
Vorlagen_Titel <- as.data.frame(read_excel(paste0("Texte/Textbausteine_LENA_",abstimmung_date,".xlsx"), 
                                           sheet = "Vorlagen_Uebersicht"))

###Anzahl, Name und Nummer der Vorlagen von JSON einlesen

##Deutsch
vorlagen <- get_vorlagen(json_data,"de")
for (v in 1:nrow(vorlagen)) {
  vorlagen$text[v] <- Vorlagen_Titel$Vorlage_d[v]
}

#Französisch
vorlagen_fr <- get_vorlagen(json_data,"fr")
for (v in 1:nrow(vorlagen_fr)) {
  vorlagen_fr$text[v] <- Vorlagen_Titel$Vorlage_f[v]
}

#Italienisch
vorlagen_it <- get_vorlagen(json_data,"it")
for (v in 1:nrow(vorlagen_it)) {
  vorlagen_it$text[v] <- Vorlagen_Titel$Vorlage_i[v]
}

#Kurznamen Vorlagen (Verwendet im File mit den Textbausteinen)
vorlagen_short <- excel_sheets[2:(nrow(vorlagen)+1)]

###Kurznamen und Nummern kantonale Vorlagen 
kantonal_short <- excel_sheets[c(6,8:13)]

#Nummer in JSON 
kantonal_number <- c(6,3,8,8,8,8,2) 

#Falls mehrere Vorlagen innerhalb eines Kantons, Vorlage auswaehlen
kantonal_add <- c(1,4,1,2,3,4,1)

###Kurznamen und Nummern kantonale Vorlagen Spezialfaelle
kantonal_short_special <- excel_sheets[c(7,14)]

#Nummer in JSON 
kantonal_number_special <- c(3,2) 

#Falls mehrere Vorlagen innerhalb eines Kantons, Vorlage auswaehlen
kantonal_add_special <- c(1,2)

#Spezialfälle
other_check <- FALSE

###Vorhandene Daten laden
#daten_co2_bfs <- read_excel("Data/daten_co2_bfs.xlsx",skip=5)
#daten_covid1_bfs <- read_excel("Data/daten_covid1_bfs.xlsx",skip=5)
#daten_covid2_bfs <- read_excel("Data/daten_covid2_bfs.xlsx",skip=5)
#cat("Daten zu historischen Abstimmungen geladen\n")

#Metadaten Gemeinden und Kantone
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, "SELECT * FROM communities_metadata")
meta_gmd_kt <- DBI::fetch(rs,n=-1)
dbDisconnectAll()


meta_gmd_kt <- meta_gmd_kt %>%
  select(-created,-last_update)

mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, "SELECT * FROM cantons_metadata WHERE area_type = 'canton'")
meta_kt <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

cantons_overview <- meta_kt %>%
  select(area_ID,languages)

mail_cantons <- meta_kt %>%
  select(area_ID,mail_KeySDA)

cat("Metadaten zu Gemeinden und Kantonen geladen\n")

#Datawrapper-Codes
datawrapper_codes <- as.data.frame(read_excel("Data/metadaten_grafiken_eidgenössische_Abstimmungen.xlsx"))
datawrapper_codes_kantonal <- as.data.frame(read_excel("Data/metadaten_grafiken_kantonale_Abstimmungen.xlsx"))
datawrapper_codes_kantonal <- datawrapper_codes_kantonal[,c(1:5)]

datawrapper_auth(Sys.getenv("DW_KEY"), overwrite = TRUE)


sprachen <- c("de","fr","it")

monate_de <- c("Januar", "Februar", "März", 
               "April", "Mai", "Juni", "July", 
               "August", "September", "Oktober",
               "November", "Dezember")

monate_fr <- c("janvier","février","mars",
               "avril","mai","juin","juillet",
               "août","septembre","octobre",
               "novembre","décembre")

monate_it <- c("gennaio","febbraio","marzo",
               "aprile","maggio","giugno",
               "luglio","agosto","settembre",
               "ottobre","novembre","dicembre")

date_voting <- as.Date(json_data$abstimmtag,format="%Y%m%d")