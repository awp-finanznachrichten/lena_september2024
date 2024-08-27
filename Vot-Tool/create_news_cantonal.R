###Deutsch
date_and_time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),"+02:00")

#ID
ID <- sample(100000000000000:999999999999999,1)

#ID Long
ID_long <- paste0(format(Sys.Date(), "%Y%m%d"),":",format(Sys.time(), "%Y%m%d%H%M%S"),ID)

#Vorlage laden
vorlage <- read_file("./tools/SDA/Vorlage_SDA_Meldungen.txt")

#Text kreieren
text <- ""
for (c in 1:nrow(canton_results)) {
  
###GET STAENDE AND YES/NO CANTONS###  
staende <- meta_kt %>%
    select(area_ID,staende_count)
all_cantons <- cantons_results %>%
  filter(votes_ID == canton_results$votes_ID[c]) %>%
  left_join(staende)
yes_cantons <- all_cantons %>%
  filter(result == "yes")
no_cantons <- all_cantons %>%
  filter(result == "no")

yes_cantons_list <- "-"
staende_yes <- 0
if (nrow(yes_cantons) > 0) {
yes_cantons_list <- paste(yes_cantons$area_ID,collapse = ", ")  
staende_yes <- sum(yes_cantons$staende_count)
}  
no_cantons_list <- "-"
staende_no <- 0
if (nrow(no_cantons) > 0) {
  no_cantons_list <- paste(no_cantons$area_ID,collapse = ", ")  
  staende_no <- sum(no_cantons$staende_count)
}  

###GET EXTRAPOLATION
extrapolation_vorlage <- extrapolations %>%
  filter(votes_ID == canton_results$votes_ID[c]) %>%
  arrange(desc(last_update)) %>%
  .[1,]

#Title
text <- paste0(text,'<p class="paragraph">',vorlagen$text[c],": ",
       ifelse(canton_results$result[c] == "yes","JA","NEIN"),
       "</p>\n")

#Table
text <- paste0(text,"<table><tbody>\n",
       "<tr><td>Ja-Anteil: ",gsub("[.]",",",format(canton_results$share_yes_percentage[c],nsmall=2))," Prozent (",format(canton_results$share_yes_votes[c],big.mark = "'")," Stimmen)</td></tr>\n",
       "<tr><td>Nein-Anteil: ",gsub("[.]",",",format(canton_results$share_no_percentage[c],nsmall=2))," Prozent (",format(canton_results$share_no_votes[c],big.mark = "'")," Stimmen)</td></tr>\n",
       ifelse(is.na(canton_results$voter_participation[c]) == FALSE,
              paste0("<tr><td>Stimmbeteiligung: ",gsub("[.]",",",format(canton_results$voter_participation[c],nsmall=2))," Prozent</td></tr>\n"),
              ""),
       "<tr><td></td></tr>\n",
       "<tr><td>Stände: ",gsub("[.]",",",staende_yes)," Ja, ",gsub("[.]",",",staende_no)," Nein",
       ifelse(staende_yes > 11.5," (Ständemehr erreicht)",""),
       ifelse(staende_no >= 11.5," (am Ständemehr gescheitert)",""),
       "</td></tr>\n",
       "<tr><td>Ja-Kantone: ",yes_cantons_list,"</td></tr>\n",
       "<tr><td>Nein-Kantone: ",no_cantons_list,"</td></tr>\n",
       "<tr><td>",
       ifelse(extrapolation_vorlage$type == "trend" & is.na(extrapolation_vorlage$result) == FALSE,
              paste0("SRG-Trend um ",format(strptime(extrapolation_vorlage$last_update, "%Y-%m-%d %H:%M:%S"),"%H:%M")," Uhr: ",extrapolation_vorlage$result),
              ""),
       ifelse(is.na(extrapolation_vorlage$share_votes_yes) == FALSE,
              paste0("SRG-Hochrechnung um ",format(strptime(extrapolation_vorlage$last_update, "%Y-%m-%d %H:%M:%S"),"%H:%M")," Uhr: ",
              extrapolation_vorlage$share_votes_yes," Prozent Ja, ",extrapolation_vorlage$share_votes_no," Prozent Nein"),
              ""),
       "</td></tr>\n</tbody></table>\n"
    )
}  

###Daten einfügen
vorlage <- gsub("INSERT_LONGID",ID_long,vorlage)
vorlage <- gsub("INSERT_TIME",date_and_time,vorlage)
vorlage <- gsub("INSERT_PROVIDER","KSDA",vorlage)
vorlage <- gsub("INSERT_STATUS","withheld",vorlage)
vorlage <- gsub("INSERT_SERVICE","bsd",vorlage)
vorlage <- gsub("INSERT_NOTE","DIES IST EIN TEST",vorlage)
vorlage <- gsub("INSERT_MEMO","DIES IST EIN TEST",vorlage)
vorlage <- gsub("INSERT_HYPERLINK","",vorlage)
vorlage <- gsub("INSERT_URGENCY","2",vorlage)
vorlage <- gsub("INSERT_ID",ID,vorlage)
vorlage <- gsub("INSERT_DATELINE","Bern",vorlage)
vorlage <- gsub("INSERT_LANGUAGE","de",vorlage)
vorlage <- gsub("INSERT_GENRE","RES",vorlage)
vorlage <- gsub("INSERT_STORYTYPES",
                '<subject type="cpnat:abstract" qcode="sdastorytype:tble"></subject>',
                vorlage)
vorlage <- gsub("INSERT_CHANNELS",
                paste0('<subject type="cpnat:abstract" qcode="sdamarschannel:POL"></subject>\n',
                       '<subject type="cpnat:abstract" qcode="sdamarschannel:VOT"></subject>'),
                vorlage)
vorlage <- gsub("INSERT_LOCATIONS",
                paste0('<located type="loctype:city" qcode="sdamarsgeo:',canton_metadata$hauptort_mars_code,'">\n<name>',canton_metadata$hauptort_de,'</name>\n</located>'),
                vorlage)
vorlage <- gsub("INSERT_CATCHWORD",paste0("vot ",output_overview$area_ID[i]),vorlage)
vorlage <- gsub("INSERT_HEADLINE",paste0("***TEST***Ergebnisse aus dem Kanton ",canton_metadata$area_name_de),vorlage)
vorlage <- gsub("INSERT_LEAD"," ",vorlage)
vorlage <- gsub("INSERT_CATCHLINE","",vorlage)
vorlage <- gsub("INSERT_TEXT",text,vorlage)

#Datei speichern
setwd("./Output_Mars")
filename <- paste0(format(Sys.Date(),"%Y%m%d"),"_",output_overview$area_ID[i],"_results_de.xml")
cat(vorlage, file = (con <- file(filename, "w", encoding="UTF-8"))); close(con)

Sys.sleep(5)
###FTP-Upload
ftpUpload(filename, paste0("ftp://awp-lena.sda-ats.ch/",filename),userpwd=Sys.getenv("ftp_sda"))

setwd("..")

#Set mail output to done
mydb <- connectDB(db_name = "sda_votes")  
sql_qry <- paste0("UPDATE output_overview SET news_results = 'done' WHERE date = '",voting_date,"' AND voting_type = 'national' AND area_ID = '",output_overview$area_ID[i],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 




