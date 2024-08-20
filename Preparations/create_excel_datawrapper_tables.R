#Working Directory definieren
setwd("C:/Users/simon/OneDrive/LENA_Project/20240609_LENA_Abstimmungen")

###Funktionen laden
source("./Funktionen/functions_readin.R", encoding = "UTF-8")
source("./Funktionen/functions_storyfinder.R", encoding = "UTF-8")
source("./Funktionen/functions_storybuilder.R", encoding = "UTF-8")
source("./Funktionen/functions_output.R", encoding = "UTF-8")
source("./tools/Funktionen/Utils.R", encoding = "UTF-8")

###Config: Bibliotheken laden, Pfade/Links definieren, bereits vorhandene Daten laden
source("CONFIG.R",encoding = "UTF-8")

###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")

ids <- c("HqKKF","X5Ww6","TLmj6","FBOdI","hZor3","Zsjek","7uTf0","6NX3q",
        "BxRuI","LiDKo","X4eEL","vsJSX","17Sh7","6sLik","jbeAJ","1q77q",
        "M5Dfo","bK0IT","r4id3","FtseI","YoPZp","SRCbA","2GDkN","BQlYn")

for (id in ids) {

metadata_chart <- dw_retrieve_chart_metadata(id)

new_entry <- data.frame("Top/Flop Tabellen",
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
