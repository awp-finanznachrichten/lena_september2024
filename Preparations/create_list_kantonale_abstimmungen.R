#Set Working Path
MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20240922_LENA_Abstimmungen"
setwd(MAIN_PATH)

#Load Libraries and Functions
source("./Config/load_libraries_functions.R",encoding = "UTF-8")

###Set Constants###
source("./Config/set_constants.R",encoding = "UTF-8")

###Load texts and metadata###
source("./Config/load_texts_metadata.R",encoding = "UTF-8")

source("./Config/load_json_data.R",encoding = "UTF-8")

###Overviews Kantone
kantone_list <- json_data_kantone[["kantone"]]

###Dataframe für Liste
kantonale_vorlagen_list <- data.frame("Kanton","Vorlage_ID","Vorlage_d","Vorlage_f","Vorlage_i")
colnames(kantonale_vorlagen_list) <- c("Kanton","Vorlage_ID","Vorlage_d","Vorlage_f","Vorlage_i")

for (k in 1:nrow(kantone_list)) {

vorlagen <- kantone_list$vorlagen[[k]]

for (i in 1:nrow(vorlagen)) {
vorlage_titel <- vorlagen$vorlagenTitel[[i]]
vorlage_titel <- vorlage_titel %>%
    filter(nchar(text) > 5)

titel_de <- ""
titel_fr <- ""
titel_it <- ""

for (v in 1:nrow(vorlage_titel)) {
  if (vorlage_titel$langKey[v] == "de") {
  titel_de <- vorlage_titel$text[v]
  }
  if (vorlage_titel$langKey[v] == "fr") {
  titel_fr <- vorlage_titel$text[v]
  }
  if (vorlage_titel$langKey[v] == "it") {
  titel_it <- vorlage_titel$text[v]
  }
}


new_entry <- data.frame(kantone_list$geoLevelname[k],
                        vorlagen$vorlagenId[i],
                        titel_de,
                        titel_fr,
                        titel_it
)

colnames(new_entry) <- c("Kanton","Vorlage_ID","Vorlage_d","Vorlage_f","Vorlage_i")
kantonale_vorlagen_list <- rbind(kantonale_vorlagen_list,new_entry)  


}

  
}

#Daten Speichern
kantonale_vorlagen_list <- kantonale_vorlagen_list[-1,]
library(xlsx)
write.xlsx(kantonale_vorlagen_list,"./Data/kantonale_vorlagen_list.xlsx",row.names = FALSE)

