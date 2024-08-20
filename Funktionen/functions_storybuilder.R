###Funktion für Texterstellung basierend auf Storyboard

build_texts <- function(dta) {
    
 for (i in 1:nrow(dta)) {
   
  #Einzelne Storyteile vom Storyboard extrahieren
  story_parts <- strsplit(dta$Storyboard[i],";")[[1]]

  
  #Texte einfügen für jeden Storyteil
  
  for (part in 1:length(story_parts)) {
  
  texts <- Textbausteine[Textbausteine$Text_ID == story_parts[part],]

  #Zufällige Variante auswählen bei mehreren Möglichkeiten
  
  #Seed mit Gemeinde-Nr -> So wird immer dieselbe Variante bei Gemeinde gewählt  
  set.seed(dta$Gemeinde_Nr[i])
  
  variante <- sample(1:nrow(texts),1)

  text <- texts[variante,]
  
  #Texte einfügen bei erstem Storyteil, bei weiteren Storyteilen Test verlängern
  
  if (part == 1) {
  
  dta$Text_d[i] <- text[,"Text_d"]
  dta$Text_f[i] <- text[,"Text_f"]
  dta$Text_i[i] <- text[,"Text_i"]

    } else {
  
      dta$Text_d[i] <- paste(dta$Text_d[i],text[,"Text_d"])
      dta$Text_f[i] <- paste(dta$Text_f[i],text[,"Text_f"])
      dta$Text_i[i] <- paste(dta$Text_i[i],text[,"Text_i"])
   
}


  }

 
}

return(dta)

}



###Funktion für Ersetzen von Variablen im Text
replace_variables <- function(dta) {

for (i in 1:nrow(dta)) {
  
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#Gemeinde_d",dta$Gemeinde_d[i])
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#Kanton_d",dta$Kanton_d[i])
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#JaStimmenInProzent",gsub("[.]",",",round(dta$Ja_Stimmen_In_Prozent[i],1)))
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#NeinStimmenInProzent",gsub("[.]",",",round(dta$Nein_Stimmen_In_Prozent[i],1)))
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#JaStimmenAbsolut",as.character(format(dta$Ja_Stimmen_Absolut[i],big.mark="'")))
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#NeinStimmenAbsolut",as.character(format(dta$Nein_Stimmen_Absolut[i],big.mark="'")))
  
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#Gemeinde_f",dta$Gemeinde_f[i]) 
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#Kanton_f",dta$Kanton_f[i])
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#JaStimmenInProzent",gsub("[.]",",",round(dta$Ja_Stimmen_In_Prozent[i],1)))
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#NeinStimmenInProzent",gsub("[.]",",",round(dta$Nein_Stimmen_In_Prozent[i],1)))
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#JaStimmenAbsolut",as.character(format(dta$Ja_Stimmen_Absolut[i],big.mark="'")))
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#NeinStimmenAbsolut",as.character(format(dta$Nein_Stimmen_Absolut[i],big.mark="'")))
  
  dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#Gemeinde_i",dta$Gemeinde_i[i]) 
  dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#Kanton_i",dta$Kanton_i[i])
  dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#JaStimmenInProzent",gsub("[.]",",",round(dta$Ja_Stimmen_In_Prozent[i],1)))
  dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#NeinStimmenInProzent",gsub("[.]",",",round(dta$Nein_Stimmen_In_Prozent[i],1)))
  dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#JaStimmenAbsolut",as.character(format(dta$Ja_Stimmen_Absolut[i],big.mark="'")))
  dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#NeinStimmenAbsolut",as.character(format(dta$Nein_Stimmen_Absolut[i],big.mark="'")))
  
  if ((hist_check == TRUE) & (hist_several_check == FALSE)) {
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#HistJaStimmenInProzent",gsub("[.]",",",round(dta$Hist_Ja_Stimmen_In_Prozent[i],1)))
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#HistNeinStimmenInProzent",gsub("[.]",",",round(dta$Hist_Nein_Stimmen_In_Prozent[i],1)))
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#HistJaStimmenAbsolut",as.character(format(dta$Hist_Ja_Stimmen_Absolut[i],big.mark="'")))
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#HistNeinStimmenAbsolut",as.character(format(dta$Hist_Nein_Stimmen_Absolut[i],big.mark="'")))

  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#HistJaStimmenInProzent",gsub("[.]",",",round(dta$Hist_Ja_Stimmen_In_Prozent[i],1)))
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#HistNeinStimmenInProzent",gsub("[.]",",",round(dta$Hist_Nein_Stimmen_In_Prozent[i],1)))
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#HistJaStimmenAbsolut",as.character(format(dta$Hist_Ja_Stimmen_Absolut[i],big.mark="'")))
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#HistNeinStimmenAbsolut",as.character(format(dta$Hist_Nein_Stimmen_Absolut[i],big.mark="'")))
  
  dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#HistJaStimmenInProzent",gsub("[.]",",",round(dta$Hist_Ja_Stimmen_In_Prozent[i],1)))
  dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#HistNeinStimmenInProzent",gsub("[.]",",",round(dta$Hist_Nein_Stimmen_In_Prozent[i],1)))
  dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#HistJaStimmenAbsolut",as.character(format(dta$Hist_Ja_Stimmen_Absolut[i],big.mark="'")))
  dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#HistNeinStimmenAbsolut",as.character(format(dta$Hist_Nein_Stimmen_Absolut[i],big.mark="'")))
  
  }
  
  if (other_check == TRUE) {
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#OtherVote_JaStimmenInProzent",gsub("[.]",",",round(dta$Other_Ja_Stimmen_In_Prozent[i],1)))
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#OtherVote_NeinStimmenInProzent",gsub("[.]",",",round(dta$Other_Nein_Stimmen_In_Prozent[i],1)))
  
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#OtherVote_JaStimmenInProzent",gsub("[.]",",",round(dta$Other_Ja_Stimmen_In_Prozent[i],1)))
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#OtherVote_NeinStimmenInProzent",gsub("[.]",",",round(dta$Other_Nein_Stimmen_In_Prozent[i],1)))
    
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#OtherVote_JaStimmenInProzent",gsub("[.]",",",round(dta$Other_Ja_Stimmen_In_Prozent[i],1)))
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#OtherVote_NeinStimmenInProzent",gsub("[.]",",",round(dta$Other_Nein_Stimmen_In_Prozent[i],1)))
    
  }
  
}
 
return(dta)   
}

excuse_my_french <- function(dta) {
  
  for (i in 1:nrow(dta)) {
    
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de A","d'A") 
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de E","d'E")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de I","d'I") 
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de O","d'O") 
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de U","d'U")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Yv","d'Yv")
  
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Les ","des ")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Le ","du ")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"à Les ","aux ")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"A Les ","Aux ")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"à Le ","au ")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"A Le ","Au ")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"du Vaud","de Le Vaud")
  
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Henniez","d'Henniez")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Hermance","d'Hermance")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Hermenches","d'Hermenches")

  
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Jura","canton du Jura")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Tessin","canton du Tessin")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"du canton de Valais","en Valais")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Argovie","canton d'Argovie")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Appenzell Rhodes-Extérieures","canton d'Appenzell Rhodes-Extérieures")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Appenzell Rhodes-Intérieures","canton d'Appenzell Rhodes-Intérieures")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Grisons","canton des Grisons")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Obwald","canton d'Obwald")
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Uri","canton d'Uri")
  
  }  
  
  
  return(dta)   
  
} 


###Ersetzen Variablen Spezialfälle
replace_variables_special <- function(dta) {
  
  for (i in 1:nrow(dta)) {
    
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#Gemeinde_d",dta$Gemeinde_d[i])
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#Kanton_d",dta$Kanton_d[i])
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#JaStimmenHauptvorlageInProzent",gsub("[.]",",",round(dta$Ja_Stimmen_In_Prozent[i],1)))
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#NeinStimmenHauptvorlageInProzent",gsub("[.]",",",round(dta$Nein_Stimmen_In_Prozent[i],1)))
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#JaStimmenHauptvorlageAbsolut",as.character(format(dta$Ja_Stimmen_Absolut[i],big.mark="'")))
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#NeinStimmenHauptvorlageAbsolut",as.character(format(dta$Nein_Stimmen_Absolut[i],big.mark="'")))
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#JaStimmenGegenvorschlagInProzent",gsub("[.]",",",round(dta$Ja_Prozent_Gegenvorschlag[i],1)))
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#NeinStimmenGegenvorschlagInProzent",gsub("[.]",",",round(dta$Nein_Prozent_Gegenvorschlag[i],1)))
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#JaStimmenGegenvorschlagAbsolut",as.character(format(dta$Ja_Absolut_Gegenvorschlag[i],big.mark="'")))
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#NeinStimmenGegenvorschlagAbsolut",as.character(format(dta$Nein_Absolut_Gegenvorschlag[i],big.mark="'")))
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#StichentscheidZustimmungHauptvorlageInProzent",gsub("[.]",",",round(dta$Stichentscheid_Zustimmung_Hauptvorlage[i],1)))
    dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#StichentscheidZustimmungGegenvorschlagInProzent",gsub("[.]",",",round(dta$Stichentscheid_Zustimmung_Gegenvorschlag[i],1)))
    
    
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#Gemeinde_f",dta$Gemeinde_f[i])
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#Kanton_f",dta$Kanton_f[i])
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#JaStimmenHauptvorlageInProzent",gsub("[.]",",",round(dta$Ja_Stimmen_In_Prozent[i],1)))
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#NeinStimmenHauptvorlageInProzent",gsub("[.]",",",round(dta$Nein_Stimmen_In_Prozent[i],1)))
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#JaStimmenHauptvorlageAbsolut",as.character(format(dta$Ja_Stimmen_Absolut[i],big.mark="'")))
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#NeinStimmenHauptvorlageAbsolut",as.character(format(dta$Nein_Stimmen_Absolut[i],big.mark="'")))
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#JaStimmenGegenvorschlagInProzent",gsub("[.]",",",round(dta$Ja_Prozent_Gegenvorschlag[i],1)))
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#NeinStimmenGegenvorschlagInProzent",gsub("[.]",",",round(dta$Nein_Prozent_Gegenvorschlag[i],1)))
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#JaStimmenGegenvorschlagAbsolut",as.character(format(dta$Ja_Absolut_Gegenvorschlag[i],big.mark="'")))
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#NeinStimmenGegenvorschlagAbsolut",as.character(format(dta$Nein_Absolut_Gegenvorschlag[i],big.mark="'")))
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#StichentscheidZustimmungHauptvorlageInProzent",gsub("[.]",",",round(dta$Stichentscheid_Zustimmung_Hauptvorlage[i],1)))
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#StichentscheidZustimmungGegenvorschlagInProzent",gsub("[.]",",",round(dta$Stichentscheid_Zustimmung_Gegenvorschlag[i],1)))
    
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#Gemeinde_i",dta$Gemeinde_i[i])
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#Kanton_i",dta$Kanton_i[i])
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#JaStimmenHauptvorlageInProzent",gsub("[.]",",",round(dta$Ja_Stimmen_In_Prozent[i],1)))
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#NeinStimmenHauptvorlageInProzent",gsub("[.]",",",round(dta$Nein_Stimmen_In_Prozent[i],1)))
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#JaStimmenHauptvorlageAbsolut",as.character(format(dta$Ja_Stimmen_Absolut[i],big.mark="'")))
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#NeinStimmenHauptvorlageAbsolut",as.character(format(dta$Nein_Stimmen_Absolut[i],big.mark="'")))
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#JaStimmenGegenvorschlagInProzent",gsub("[.]",",",round(dta$Ja_Prozent_Gegenvorschlag[i],1)))
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#NeinStimmenGegenvorschlagInProzent",gsub("[.]",",",round(dta$Nein_Prozent_Gegenvorschlag[i],1)))
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#JaStimmenGegenvorschlagAbsolut",as.character(format(dta$Ja_Absolut_Gegenvorschlag[i],big.mark="'")))
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#NeinStimmenGegenvorschlagAbsolut",as.character(format(dta$Nein_Absolut_Gegenvorschlag[i],big.mark="'")))
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#StichentscheidZustimmungHauptvorlageInProzent",gsub("[.]",",",round(dta$Stichentscheid_Zustimmung_Hauptvorlage[i],1)))
    dta$Text_i[i] <- str_replace_all(dta$Text_i[i],"#StichentscheidZustimmungGegenvorschlagInProzent",gsub("[.]",",",round(dta$Stichentscheid_Zustimmung_Gegenvorschlag[i],1)))
    
  }
  
  return(dta)   
}



