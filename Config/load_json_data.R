res <- GET(FEED_NATIONAL)
json_data <- fromJSON(rawToChar(res$content), flatten = TRUE)

res <- GET(FEED_CANTONAL)
json_data_kantone <- fromJSON(rawToChar(res$content), flatten = TRUE)

cat("Aktuelle Abstimmungsdaten geladen\n")


###Anzahl, Name und Nummer der Vorlagen von JSON einlesen

##Deutsch
vorlagen <- get_vorlagen(json_data,"de")
for (v in 1:nrow(vorlagen)) {
  vote_metadata <- votes_metadata %>%
    filter(votes_ID == vorlagen$id[v],
           area_ID == "CH")
  vorlagen$text[v] <- vote_metadata$title_de
}


#Französisch
vorlagen_fr <- get_vorlagen(json_data,"fr")
for (v in 1:nrow(vorlagen_fr)) {
  vote_metadata <- votes_metadata %>%
    filter(votes_ID == vorlagen$id[v],
           area_ID == "CH")
  vorlagen_fr$text[v] <- vote_metadata$title_fr
}

#Italienisch
vorlagen_it <- get_vorlagen(json_data,"it")
for (v in 1:nrow(vorlagen_it)) {
  vote_metadata <- votes_metadata %>%
    filter(votes_ID == vorlagen$id[v],
           area_ID == "CH")
  vorlagen_it$text[v] <- vote_metadata$title_it
}

#Kurznamen Vorlagen (Verwendet im File mit den Textbausteinen)
vorlagen_selection <- votes_metadata %>% 
  filter(area_ID == "CH")
vorlagen_short <- vorlagen_selection$spreadsheet

###Kurznamen und Nummern kantonale Vorlagen 
kantonal_selection <- votes_metadata %>% 
  filter(area_ID != "CH",
         lena_map == "yes",
         is.na(remarks) == TRUE)
kantonal_short <- kantonal_selection$spreadsheet


#Nummer in JSON 
kantonal_number <- c()
kantonal_add <- c()
if (nrow(kantonal_selection) > 0) {
for (k in 1:nrow(kantonal_selection)) {
kantonal_number <- c(kantonal_number,
                     which(json_data_kantone[["kantone"]][["geoLevelname"]] == kantonal_selection$area_ID[k]))
kantonal_add <- c(kantonal_add,
                  which(json_data_kantone[["kantone"]][["vorlagen"]][[which(json_data_kantone[["kantone"]][["geoLevelname"]] == kantonal_selection$area_ID[k])]][["vorlagenId"]] == kantonal_selection$votes_ID[k]))
}  
}

###Kurznamen und Nummern kantonale Vorlagen Spezialfaelle
kantonal_selection_special <- votes_metadata %>% 
  filter(area_ID != "CH",
         lena_map == "yes",
         remarks == "special",
         type == "initiative")
kantonal_short_special <- kantonal_selection_special$spreadsheet

#Nummer in JSON 
kantonal_number_special <- c()
kantonal_add_special <- c()
if (nrow(kantonal_selection_special) > 0) {
for (k in 1:nrow(kantonal_selection_special)) {
  kantonal_number_special <- c(kantonal_number_special,
                       which(json_data_kantone[["kantone"]][["geoLevelname"]] == kantonal_selection_special$area_ID[k]))
  kantonal_add_special <- c(kantonal_add_special,
                    which(json_data_kantone[["kantone"]][["vorlagen"]][[which(json_data_kantone[["kantone"]][["geoLevelname"]] == kantonal_selection_special$area_ID[k])]][["vorlagenId"]] == kantonal_selection_special$votes_ID[k]))
}  
}
