for (v in 1:nrow(vorlagen)) {

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

###GET STAENDE AND YES/NO CANTONS###  
staende <- meta_kt %>%
    select(area_ID,staende_count,area_name_de)
all_cantons <- cantons_results %>%
  filter(votes_ID ==   vorlagen$id[v]) %>%
  left_join(staende)
yes_cantons <- all_cantons %>%
  filter(result == "yes")
no_cantons <- all_cantons %>%
  filter(result == "no")
not_counted <- all_cantons %>%
  filter(is.na(result) == TRUE)

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
not_counted_list <- "-"
if (nrow(not_counted) > 0) {
  not_counted_list <- paste(not_counted$area_ID,collapse = ", ")  
}  

###GET EXTRAPOLATION
extrapolation_vorlage <- extrapolations %>%
  filter(votes_ID == vorlagen$id[v]) %>%
  arrange(desc(last_update)) %>%
  .[1,]

#Title
text <- paste0(text,'<p class="paragraph">',vorlagen$text[v],"</p>\n")

#Table
text <- paste0(text,"<table><tbody>\n",
               "<tr>",
               "<td>Kanton</td>",
               "<td>Ja</td>",
               "<td>%</td>",
               "<td>Nein</td>",
               "<td>%</td>",
               "<td>Bet. %</td></tr>\n")

for (c in 1:nrow(all_cantons)) {
if (is.na(all_cantons$result[c]) == FALSE) {
  
text <- paste0(text,
               "<tr>",
               "<td>",all_cantons$area_name_de[c],"</td>",
               "<td>",all_cantons$share_yes_votes[c],"</td>",
               "<td>",gsub("[.]",",",format(all_cantons$share_yes_percentage[c],nsmall=2)),"</td>",
               "<td>",all_cantons$share_no_votes[c],"</td>",
               "<td>",gsub("[.]",",",format(all_cantons$share_no_percentage[c],nsmall=2)),"</td>",
               "<td>",gsub("[.]",",",format(all_cantons$voter_participation[c],nsmall=2)),"</td></tr>\n")
} 
}  
text <- paste0(text,"</tbody></table>\n<table><tbody>\n",
"<tr><td>Stände: ",gsub("[.]",",",staende_yes)," Ja, ",gsub("[.]",",",staende_no)," Nein",
ifelse(staende_yes > 11.5," (Ständemehr erreicht)",""),
ifelse(staende_no >= 11.5," (am Ständemehr gescheitert)",""),
"</td></tr>\n",
"<tr><td>Ja-Kantone: ",yes_cantons_list,"</td></tr>\n",
"<tr><td>Nein-Kantone: ",no_cantons_list,"</td></tr>\n",
ifelse(not_counted_list == "-",
       "",
       paste0("<tr><td>Noch nicht ausgezählt: ",not_counted_list,"</td></tr>\n")),
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
                paste0('<located type="loctype:city" qcode="sdamarsgeo:BRN">\n<name>Bern</name>\n</located>'),
                vorlage)
vorlage <- gsub("INSERT_CATCHWORD","vot Tabelle",vorlage)
vorlage <- gsub("INSERT_HEADLINE",paste0(vorlagen$text[v],": Zwischenstand"),vorlage)
vorlage <- gsub("INSERT_LEAD"," ",vorlage)
vorlage <- gsub("INSERT_CATCHLINE","",vorlage)
vorlage <- gsub("INSERT_TEXT",text,vorlage)

#Datei speichern
setwd("./Output_Mars")
filename <- paste0(format(Sys.Date(),"%Y%m%d"),"_",format(Sys.time(),"%H"),"h_",vorlagen$id[v],"_overview_de.xml")
cat(vorlage, file = (con <- file(filename, "w", encoding="UTF-8"))); close(con)

Sys.sleep(5)
###FTP-Upload
ftpUpload(filename, paste0("ftp://awp-lena.sda-ats.ch/",filename),userpwd=Sys.getenv("ftp_sda"))

setwd("..")

}

#Send Mail
Subject <- paste0("***TEST***Neue Zwischenstandsmeldung erstellt")
Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
               "Neue Zwischenstands-Meldungen mit allen ausgezählten Kantonen zu jeder Vorlage wurde erstellt.\n\n",
               "Ihr findet die Meldungen im Mars im Input-Ordner Lena.\n\n",
               "Liebe Grüsse\n\nLENA")
send_notification(Subject,
                  Body,
                  paste0(DEFAULT_MAILS))  

#Set output to done
mydb <- connectDB(db_name = "sda_votes")  
sql_qry <- paste0("UPDATE output_news_intermediate SET news_intermediate = 'done' WHERE timestamp = '",output_news_intermediate$timestamp[1],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 




