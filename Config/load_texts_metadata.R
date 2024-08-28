#Load Texts
excel_sheets <- excel_sheets(paste0("Texte/Textbausteine_LENA_",abstimmung_date,".xlsx"))

#Votes Metadata
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM votes_metadata WHERE date = '",voting_date,"'"))
votes_metadata <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

votes_metadata_CH <- votes_metadata %>%
  filter(area_ID == "CH")

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

###Historical Data (if available)
data_hist <- fromJSON("./Data/sd-t-17-02-20100307-eidgAbstimmung.json", flatten = TRUE)
data_hist <- get_results(data_hist,3,level="communal")
data_hist <- data_hist %>%
  select(Gemeinde_Nr,
         Hist_Ja_Stimmen_In_Prozent = jaStimmenInProzent,
         Hist_Ja_Stimmen_Absolut = jaStimmenAbsolut,
         Hist_Nein_Stimmen_Absolut = neinStimmenAbsolut) %>%
  mutate(Hist_Nein_Stimmen_In_Prozent = 100 - Hist_Ja_Stimmen_In_Prozent)
data_hist <- na.omit(data_hist)

#Datawrapper-Codes
datawrapper_codes <- as.data.frame(read_excel("Data/metadaten_grafiken_eidgenössische_Abstimmungen.xlsx"))
datawrapper_codes_kantonal <- as.data.frame(read_excel("Data/metadaten_grafiken_kantonale_Abstimmungen.xlsx"))
datawrapper_codes_kantonal <- datawrapper_codes_kantonal[,c(1:5)]

datawrapper_auth(Sys.getenv("DW_KEY"), overwrite = TRUE)

