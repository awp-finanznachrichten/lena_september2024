#OECD_Mindeststeuer
data_OECD_Mindeststeuer <- read_csv("Output/OECD_Mindeststeuer_dw.csv")

data_OECD_Mindeststeuer <- data_OECD_Mindeststeuer %>%
  select(Gemeinde_Nr,
         Gemeinde_KT_d,
         Ja_Stimmen_In_Prozent,
         Nein_Stimmen_In_Prozent,
         Text_d)

colnames(data_OECD_Mindeststeuer) <- c("Gemeinde_Nr","Gemeinde_Name",
                                 "OECD_Mindeststeuer_Ja_Anteil","OECD_Mindeststeuer_Nein_Anteil",
                                 "OECD_Mindeststeuer_Text")

#Klima_Gesetz
data_Klima_Gesetz <- read_csv("Output/Klima_Gesetz_dw.csv")

data_Klima_Gesetz <- data_Klima_Gesetz %>%
  select(Gemeinde_Nr,
         Ja_Stimmen_In_Prozent,
         Nein_Stimmen_In_Prozent,
         Text_d)

colnames(data_Klima_Gesetz) <- c("Gemeinde_Nr","Klima_Gesetz_Ja_Anteil","Klima_Gesetz_Nein_Anteil",
                                 "Klima_Gesetz_Text")

#Covid_Gesetz
data_Covid_Gesetz <- read_csv("Output/Covid_Gesetz_dw.csv")

data_Covid_Gesetz <- data_Covid_Gesetz %>%
  select(Gemeinde_Nr,
         Ja_Stimmen_In_Prozent,
         Nein_Stimmen_In_Prozent,
         Text_d)

colnames(data_Covid_Gesetz) <- c("Gemeinde_Nr","Covid_Gesetz_Ja_Anteil","Covid_Gesetz_Nein_Anteil",
                                 "Covid_Gesetz_Text")


#Zusammenführen
datenfeed_all <- merge(data_OECD_Mindeststeuer,data_Klima_Gesetz)
datenfeed_all <- merge(datenfeed_all,data_Covid_Gesetz)

#Datenfeed speichern
write.csv(datenfeed_all,"Output/Datenfeed_NAU.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")


