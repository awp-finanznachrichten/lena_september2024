MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20240922_LENA_Abstimmungen"

#Working Directory definieren
setwd(MAIN_PATH)

###Funktionen laden
source("./Funktionen/functions_readin.R", encoding = "UTF-8")
source("./Funktionen/functions_github.R", encoding = "UTF-8")
source("./tools/Funktionen/Utils.R", encoding = "UTF-8")

repeat{
###Config: Bibliotheken laden, Pfade/Links definieren, bereits vorhandene Daten laden
source("CONFIG.R",encoding = "UTF-8")

#SRG Hochrechnungen
source("./Vot-Tool/SRG_API_Request.R", encoding = "UTF-8")

###Write Data in DB###
source("./Vot-Tool/nationale_abstimmungen_DB_entries.R", encoding = "UTF-8")

###Send Mail if Canton complete###
source("./Vot-Tool/nationale_abstimmungen_send_mail.R", encoding = "UTF-8")
  
#Abstimmung komplett?
source("./Vot-Tool/nationale_abstimmungen_report.R", encoding="UTF-8")  
  
Sys.sleep(5)
}  