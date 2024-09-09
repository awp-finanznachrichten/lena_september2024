#Set Working Path
MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20240922_LENA_Abstimmungen"
setwd(MAIN_PATH)

#Load Libraries and Functions
source("./Config/load_libraries_functions.R",encoding = "UTF-8")

###Set Constants###
source("./Config/set_constants.R",encoding = "UTF-8")

###Load texts and metadata###
source("./Config/load_texts_metadata.R",encoding = "UTF-8")

###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")

ids <- c("mkn6k","2I0SZ","F3wIY","Ro2H4",
         "uGnmU","y4CXn","2I4TB","q8Q4r",
        "iPDtu","pMGGE","VwGeQ","CosFC")

for (id in ids) {

metadata_chart <- dw_retrieve_chart_metadata(id)

new_entry <- data.frame("Krankenkassen",
                        "",
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        metadata_chart$id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}


#Daten Speichern
grafiken_uebersicht <- grafiken_uebersicht[-1,]
library(xlsx)
write.xlsx(grafiken_uebersicht,"./Data/metadaten_tabellen.xlsx",row.names = FALSE)
