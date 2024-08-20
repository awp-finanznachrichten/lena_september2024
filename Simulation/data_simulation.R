###National
json_data$schweiz$vorlagen$resultat.jaStimmenInProzent <- runif(nrow(vorlagen),0,100) 

for (v in 1:nrow(vorlagen) ) {
#Nationales Ergebnis
json_data$schweiz$vorlagen$kantone[[v]]$resultat.gebietAusgezaehlt <- TRUE
json_data$schweiz$vorlagen$kantone[[v]]$resultat.jaStimmenInProzent <- runif(26,0,100)

#Gemeinden
for (k in 1:26) {
  json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.gebietAusgezaehlt <- TRUE
  json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.jaStimmenAbsolut <- sample(0:10000,length(json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.jaStimmenAbsolut)) 
  json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.neinStimmenAbsolut <- sample(0:10000,length(json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.neinStimmenAbsolut)) 
  json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.gueltigeStimmen <- json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.jaStimmenAbsolut + json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.neinStimmenAbsolut
  json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.jaStimmenInProzent <-  json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.jaStimmenAbsolut*100/json_data$schweiz$vorlagen$kantone[[v]]$gemeinden[[k]]$resultat.gueltigeStimmen
}
}

#Kantonal
for (k in 1:length(json_data_kantone[["kantone"]][["vorlagen"]])) {
count_vorlagen <- length(json_data_kantone$kantone$vorlagen[[k]]$resultat.jaStimmenInProzent)  
json_data_kantone$kantone$vorlagen[[k]]$vorlageBeendet <- TRUE
json_data_kantone$kantone$vorlagen[[k]]$resultat.gebietAusgezaehlt <- TRUE
json_data_kantone$kantone$vorlagen[[k]]$resultat.jaStimmenInProzent <- runif(count_vorlagen,0,100)

for (v in 1:count_vorlagen) {

json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.gebietAusgezaehlt <- TRUE
json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.jaStimmenAbsolut <- sample(0:10000,length(json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.jaStimmenAbsolut)) 
json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.neinStimmenAbsolut <- sample(0:10000,length(json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.neinStimmenAbsolut)) 
json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.gueltigeStimmen <- json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.jaStimmenAbsolut + json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.neinStimmenAbsolut
json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.jaStimmenInProzent <-  json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.jaStimmenAbsolut*100/json_data_kantone$kantone$vorlagen[[k]]$gemeinden[[v]]$resultat.gueltigeStimmen
}  
}  


