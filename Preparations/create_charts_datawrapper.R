#Set Working Path
MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20240922_LENA_Abstimmungen"
setwd(MAIN_PATH)

#Load Libraries and Functions
source("./Config/load_libraries_functions.R",encoding = "UTF-8")

###Set Constants###
source("./Config/set_constants.R",encoding = "UTF-8")

###Load texts and metadata###
source("./Config/load_texts_metadata.R",encoding = "UTF-8")

###Load JSON Data
source("./Config/load_json_data.R",encoding = "UTF-8")

Vorlagen_Titel <- as.data.frame(read_excel(paste0("Texte/Textbausteine_LENA_",abstimmung_date,".xlsx"), 
                                           sheet = "Vorlagen_Uebersicht"))

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

#Ids von Karten-Vorlagen
vorlagen_uebersicht <- c("O1i9P","Ocame","ot8Mm")
vorlagen_gemeinden <- c("EuC56","JJ03i","CPwql")
vorlagen_kantone <- c("HH2Hs","G7A2k","sobvY")
vorlagen_kantone_special_overview <- c("8QihT","SZmQU","7wXV4")
vorlagen_kantone_special_initiative <- c("xftvb","LS2ff","FAOHL")
vorlagen_kantone_special_gegenvorschlag <- c("NNXj8","FsoiR","qiUTe")
vorlagen_kantone_special_stichentscheid <- c("UfpM1","cDy1R","ygRKA")

#Titel aktuelle Vorlagen
vorlagen_all <- rbind(vorlagen,vorlagen_fr)
vorlagen_all <- rbind(vorlagen_all,vorlagen_it)

#Ordnerstruktur erstellen
team_id <- "6Gn1afus"
date_voting <- as.Date(json_data$abstimmtag,format="%Y%m%d")

main_folder <- dw_create_folder(name=paste0("Abstimmung ",day(date_voting),". ",monate_de[month(date_voting)]," ",year(date_voting)),organization_id = team_id)

folder_eid <- dw_create_folder("Eidgenössische Abstimmungen",parent_id = main_folder$id)
folder_kantonal <- dw_create_folder("Kantonale Abstimmungen",parent_id = main_folder$id)
folder_infografiken <- dw_create_folder("SDA Infografiken",parent_id = main_folder$id)

folder_uebersicht <- dw_create_folder("_Übersicht",parent_id = folder_eid$id)
folder_einzugsgebiete <- dw_create_folder("Einzugsgebiete",parent_id = folder_eid$id)
folder_kantone <- dw_create_folder("Kantone",parent_id = folder_eid$id)
folder_schweiz <- dw_create_folder("Schweiz",parent_id = folder_eid$id)

folder_gemeindeebene <- dw_create_folder("Gemeindeebene",parent_id = folder_schweiz$id)
folder_kantonsebene <- dw_create_folder("Kantonsebene",parent_id = folder_schweiz$id)

folder_kantone_uebersicht <- dw_create_folder("_Übersicht",parent_id = folder_kantonal$id)

#Save folders
all_folders <- c(main_folder,folder_eid,folder_kantonal,folder_infografiken,
                 folder_uebersicht,folder_einzugsgebiete,folder_kantone,folder_schweiz,
                 folder_gemeindeebene,
                 folder_kantonsebene,
                 folder_kantone_uebersicht)

#saveRDS(all_folders,file="./Preparations/all_folders.RDS")
#all_folders <- readRDS("./Preparations/all_folders.RDS")



###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")

#Uebersicht
titel_de <- paste0("Aktueller Stand der Abstimmungen vom ",day(date_voting),". ",monate_de[month(date_voting)]," ",year(date_voting))
titel_fr <- paste0("Etat actuel des votes au ",day(date_voting)," ",monate_fr[month(date_voting)]," ",year(date_voting))
titel_it <- paste0("Situazione attuale delle votazioni del ",day(date_voting)," ",monate_it[month(date_voting)]," ",year(date_voting))

titel_all <- c(titel_de,titel_fr,titel_it)

for (i in 1:3) {
data_chart <- dw_copy_chart(vorlagen_uebersicht[i])
dw_edit_chart(data_chart$id,
              title=titel_all[i],
              folderId = folder_uebersicht$id)
dw_publish_chart(data_chart$id)
metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)

new_entry <- data.frame("Uebersicht",
                        "alle",
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        metadata_chart$id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}

#Schweizkarten erstellen, Gemeinde und Kantone
for (v in 1:length(vorlagen_short)) {
  title_select <- c(v,v+length(vorlagen_short),v+length(vorlagen_short)+length(vorlagen_short))

  #Alle drei Sprachen
  for (i in 1:3) {
  #Gemeinden  
  data_chart <- dw_copy_chart(vorlagen_gemeinden[i])
  dw_edit_chart(data_chart$id,
                title=vorlagen_all$text[title_select[i]],
                folderId = folder_gemeindeebene$id,
                data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                 gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                 "/master/Output_Switzerland/",vorlagen_short[v],"_dw_",sprachen[i],".csv")))
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)

  new_entry <- data.frame("Schweizer Gemeinden",
                          vorlagen_short[v],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
  colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  
  #Kantone
  data_chart <- dw_copy_chart(vorlagen_kantone[i])
  dw_edit_chart(data_chart$id,
                title=vorlagen_all$text[title_select[i]],
                folderId = folder_kantonsebene$id,
                data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                 gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                 "/master/Output_Switzerland/",vorlagen_short[v],"_dw_kantone.csv")))
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
  
  new_entry <- data.frame("Schweizer Kantone",
                          vorlagen_short[v],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
  colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
}  

#Daten Speichern
grafiken_uebersicht <- grafiken_uebersicht[-1,]
library(xlsx)
write.xlsx(grafiken_uebersicht,"./Data/metadaten_grafiken.xlsx",row.names = FALSE)

###Kantonale Abstimmungen
grafiken_uebersicht <- data.frame("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")

for (k in 1:length(kantonal_short)) {
  #Get Title and Info
  vorlage_id <- json_data_kantone[["kantone"]][["vorlagen"]][[kantonal_number[k]]][["vorlagenId"]][kantonal_add[k]]

  Vorlagen_Info <- Vorlagen_Titel %>%
    filter(Vorlage_ID == vorlage_id)

if (is.na(Vorlagen_Info$Vorlage_d) == FALSE) {
  data_chart <- dw_copy_chart(vorlagen_gemeinden[1])
  created_folder <- dw_create_folder(paste0(kantonal_short[k],"_DE"),parent_id = 262397) 

  dw_edit_chart(data_chart$id,
                title=Vorlagen_Info$Vorlage_d,
                intro = "&nbsp;",
                annotate = "&nbsp;",
                folderId = created_folder$id,
                data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                 gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                 "/master/Output_Cantons/",kantonal_short[k],"_dw_",sprachen[1],".csv")),
                visualize=list("hide-empty-regions" = TRUE))
  
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
 
  new_entry <- data.frame("Kantonale Vorlage",
                          kantonal_short[k],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
  colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}
if (is.na(Vorlagen_Info$Vorlage_f) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_gemeinden[2])
    created_folder <- dw_create_folder(paste0(kantonal_short[k],"_FR"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_f,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short[k],"_dw_",sprachen[2],".csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage",
                            kantonal_short[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
if (is.na(Vorlagen_Info$Vorlage_i) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_gemeinden[3])
    created_folder <- dw_create_folder(paste0(kantonal_short[k],"_IT"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_i,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short[k],"_dw_",sprachen[3],".csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage",
                            kantonal_short[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
}

#Kantonale Abstimmungen Special
for (k in 1:length(kantonal_short_special)) {
  
  #Übersicht
  vorlage_id <- json_data_kantone[["kantone"]][["vorlagen"]][[kantonal_number_special[k]]][["vorlagenId"]][kantonal_add_special[k]]
  Vorlagen_Info <- Vorlagen_Titel %>%
    filter(Vorlage_ID == vorlage_id)
  
  if (is.na(Vorlagen_Info$Vorlage_d) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_overview[1])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Overview_DE"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_d,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[1],"_overview.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Overview",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_f) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_overview[2])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Overview_FR"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_f,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[2],"_overview.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Overview",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_i) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_initiative[3])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Overview_IT"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_i,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[3],"_overview.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Overview",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  
  
  
  #Initiative
  vorlage_id <- json_data_kantone[["kantone"]][["vorlagen"]][[kantonal_number_special[k]]][["vorlagenId"]][kantonal_add_special[k]]
  Vorlagen_Info <- Vorlagen_Titel %>%
    filter(Vorlage_ID == vorlage_id)

  if (is.na(Vorlagen_Info$Vorlage_d) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_initiative[1])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Initiative_DE"),parent_id = folder_kantonal$id) #"166825"

    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_d,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[1],"_initiative.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Initiative",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_f) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_initiative[2])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Initiative_FR"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_f,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[2],"_initiative.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Initiative",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_i) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_initiative[3])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Initiative_IT"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_i,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[3],"_initiative.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Initiative",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }

  #Gegenvorschlag
  vorlage_id <- json_data_kantone[["kantone"]][["vorlagen"]][[kantonal_number_special[k]]][["vorlagenId"]][kantonal_add_special[k]+1]
  Vorlagen_Info <- Vorlagen_Titel %>%
    filter(Vorlage_ID == vorlage_id)

  if (is.na(Vorlagen_Info$Vorlage_d) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_gegenvorschlag[1])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Gegenvorschlag_DE"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_d,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[1],"_gegenvorschlag.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Gegenvorschlag",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_f) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_gegenvorschlag[2])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Gegenvorschlag_FR"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_f,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[2],"_gegenvorschlag.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Gegenvorschlag",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_i) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_gegenvorschlag[3])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Gegenvorschlag_IT"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_i,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[3],"_gegenvorschlag.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Gegenvorschlag",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  
  #Stichentscheid
  vorlage_id <- json_data_kantone[["kantone"]][["vorlagen"]][[kantonal_number_special[k]]][["vorlagenId"]][kantonal_add_special[k]+2]
  Vorlagen_Info <- Vorlagen_Titel %>%
    filter(Vorlage_ID == vorlage_id)
  
  if (is.na(Vorlagen_Info$Vorlage_d) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_stichentscheid[1])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Stichentscheid_DE"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_d,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[1],"_stichentscheid.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Stichentscheid",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_f) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_stichentscheid[2])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Stichentscheid_FR"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_f,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[2],"_stichentscheid.csv")),
                  visualize=)
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Stichentscheid",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_i) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_stichentscheid[3])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Stichentscheid_IT"),parent_id = folder_kantonal$id) #"166825"
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_i,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[3],"_stichentscheid.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Stichentscheid",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
}

###Overviews Kantone
kantone_list <- json_data_kantone[["kantone"]]

for (k in 1:nrow(kantone_list)) {
vorlagen <- kantone_list$vorlagen[[k]]

vorlage_titel <- vorlagen$vorlagenTitel[[1]]
vorlage_titel <- vorlage_titel %>%
    filter(nchar(text) > 5,
           langKey != "rm")

for (v in 1:nrow(vorlage_titel)) {
  if (vorlage_titel$langKey[v] == "de") {
  titel <- paste0(kantone_list$geoLevelname[k],": Kantonale Abstimmungen vom ",day(date_voting),". ",monate_de[month(date_voting)]," ",year(date_voting))
  l <- 1
  }
  if (vorlage_titel$langKey[v] == "fr") {
  titel <- paste0(kantone_list$geoLevelname[k],": Votations cantonales du ",day(date_voting)," ",monate_fr[month(date_voting)]," ",year(date_voting))
  l <- 2
  }
  if (vorlage_titel$langKey[v] == "it") {
  titel <- paste0(kantone_list$geoLevelname[k],": Votazione cantonale del ",day(date_voting)," ",monate_it[month(date_voting)]," ",year(date_voting))
  l <- 3
  }

  data_chart <- dw_copy_chart(vorlagen_uebersicht[l])
  dw_edit_chart(data_chart$id,
                title=titel,
                folderId = folder_kantone_uebersicht$id) 
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
  
  new_entry <- data.frame("Uebersicht Kanton",
                          kantone_list$geoLevelname[k],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
  colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}  
}

#Daten Speichern
grafiken_uebersicht <- grafiken_uebersicht[-1,]
library(xlsx)
write.xlsx(grafiken_uebersicht,"./Data/metadaten_grafiken_kantonal.xlsx",row.names = FALSE)


#KANTONE AUTOMATISCH ERSTELLEN
grafiken_uebersicht <- data.frame("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")

for (c in 1:nrow(cantons_overview)) {
folder_kanton <- dw_create_folder(cantons_overview$area_ID[c],parent_id = folder_kantone$id)
for (v in 1:length(vorlagen_short)) {
  if (grepl("de",cantons_overview$languages[c]) == TRUE) {
    data_chart <- dw_copy_chart(vorlagen_gemeinden[1])
    dw_edit_chart(data_chart$id,
                  title=paste0(cantons_overview$area_ID[c],": ",Vorlagen_Titel$Vorlage_d[v]),
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = folder_kanton$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",cantons_overview$area_ID[c],"_",vorlagen_short[v],"_dw_de.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame(paste0("Kanton ",cantons_overview$area_ID[c]),
                            vorlagen_short[v],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (grepl("fr",cantons_overview$languages[c]) == TRUE) {
    data_chart <- dw_copy_chart(vorlagen_gemeinden[2])
    dw_edit_chart(data_chart$id,
                  title=paste0(cantons_overview$area_ID[c],": ",Vorlagen_Titel$Vorlage_f[v]),
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = folder_kanton$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",cantons_overview$area_ID[c],"_",vorlagen_short[v],"_dw_fr.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame(paste0("Kanton ",cantons_overview$area_ID[c]),
                            vorlagen_short[v],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (grepl("it",cantons_overview$languages[c]) == TRUE) {
    data_chart <- dw_copy_chart(vorlagen_gemeinden[3])
    dw_edit_chart(data_chart$id,
                  title=paste0(cantons_overview$area_ID[c],": ",Vorlagen_Titel$Vorlage_i[v]),
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = folder_kanton$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(date_voting)])),year(date_voting),
                                                   "/master/Output_Cantons/",cantons_overview$area_ID[c],"_",vorlagen_short[v],"_dw_it.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame(paste0("Kanton ",cantons_overview$area_ID[c]),
                            vorlagen_short[v],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
}
}

#Daten Speichern
grafiken_uebersicht <- grafiken_uebersicht[-1,]
library(xlsx)
write.xlsx(grafiken_uebersicht,"./Data/metadaten_grafiken_kantonskarten.xlsx",row.names = FALSE)
