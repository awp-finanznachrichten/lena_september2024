get_output_gemeinden <- function(dta,
                                 language,
                                 type = "normal") {

  if (language == "de") {
  output_dw <- dta %>%
    select(Gemeinde_Nr,Gemeinde_color,Ja_Stimmen_In_Prozent,Nein_Stimmen_In_Prozent,Gemeinde_KT_d,Text_d) %>%
    rename(ID = Gemeinde_Nr,
      Gemeinde_de = Gemeinde_KT_d,
           Text_de = Text_d)
  }
  if (language == "fr") {
    output_dw <- dta %>%
      select(Gemeinde_Nr,Gemeinde_color,Ja_Stimmen_In_Prozent,Nein_Stimmen_In_Prozent,Gemeinde_KT_f,Text_f) %>%
      rename(ID = Gemeinde_Nr,
        Gemeinde_fr = Gemeinde_KT_f,
             Text_fr = Text_f)
  }
  if (language == "it") {
    output_dw <- dta %>%
      select(Gemeinde_Nr,Gemeinde_color,Ja_Stimmen_In_Prozent,Nein_Stimmen_In_Prozent,Gemeinde_KT_i,Text_i) %>%
      rename(ID = Gemeinde_Nr,
        Gemeinde_it = Gemeinde_KT_i,
             Text_it = Text_i)
  }
  
  
  #Runden
  output_dw$Ja_Stimmen_In_Prozent <- round(output_dw$Ja_Stimmen_In_Prozent,1)
  output_dw$Nein_Stimmen_In_Prozent <- round(output_dw$Nein_Stimmen_In_Prozent,1)
  output_dw$Gemeinde_color <- round(output_dw$Gemeinde_color,1)

return(output_dw)  
}  

get_output_kantone <- function(dta) {

output_dw_kantone <- dta %>%
    select(Kantons_Nr,Kanton_d,Kanton_f,Kanton_i,Ja_Stimmen_In_Prozent_Kanton) %>%
    mutate(Nein_Stimmen_In_Prozent_Kanton = round(100-Ja_Stimmen_In_Prozent_Kanton,1),
           Ja_Stimmen_In_Prozent_Kanton = round(Ja_Stimmen_In_Prozent_Kanton,1),
           Kanton_color = 50,
           Gemeinden_overall = 0,
           Gemeinden_counted = 0,
           Legende = NA,
           Text_de = "",
           Text_fr = "",
           Text_it = "") %>%
    unique()

for (y in 1:nrow(output_dw_kantone)) {
  
  #Gemeinden zählen
  output_dw_kantone$Gemeinden_overall[y] <- nrow(dta[dta$Kantons_Nr == output_dw_kantone$Kantons_Nr[y],])
  output_dw_kantone$Gemeinden_counted[y] <- nrow(dta[dta$Kantons_Nr == output_dw_kantone$Kantons_Nr[y] & dta$Gebiet_Ausgezaehlt == TRUE,])
  
  if (output_dw_kantone$Gemeinden_counted[y] == 0) { 
    
    if (is.na(output_dw_kantone$Ja_Stimmen_In_Prozent_Kanton[y]) == TRUE) {
    
    output_dw_kantone$Ja_Stimmen_In_Prozent_Kanton[y] <- 0
    output_dw_kantone$Nein_Stimmen_In_Prozent_Kanton[y] <- 0
    output_dw_kantone$Kanton_color[y] <- 50
    output_dw_kantone$Legende[y] <- paste0(output_dw_kantone$Gemeinden_counted[y],"/",output_dw_kantone$Gemeinden_overall[y])
    output_dw_kantone$Text_de[y] <- "Es sind noch keine Gemeinden ausgezählt"
    output_dw_kantone$Text_fr[y] <- "Aucun résultat n'est encore connu."
    output_dw_kantone$Text_it[y] <- "Nessun risultato è ancora noto."
    
    } else {
      output_dw_kantone$Legende[y] <- paste0(output_dw_kantone$Gemeinden_counted[y],"/",output_dw_kantone$Gemeinden_overall[y])
      output_dw_kantone$Text_de[y] <- "Die brieflichen Stimmen sind ausgezählt."
      output_dw_kantone$Text_fr[y] <- "Les votes par correspondance ont été dépouillés."
      output_dw_kantone$Text_it[y] <- "I voti per corrispondenza sono stati scrutinati."
      
      if (output_dw_kantone$Ja_Stimmen_In_Prozent_Kanton[y] > 50) {
        output_dw_kantone$Kanton_color[y] <- 100
      } else {
        output_dw_kantone$Kanton_color[y] <- 0
      }  
      
    }  
    
  } else if (output_dw_kantone$Gemeinden_counted[y] < output_dw_kantone$Gemeinden_overall[y]) {
    
    output_dw_kantone$Legende[y] <- paste0(output_dw_kantone$Gemeinden_counted[y],"/",output_dw_kantone$Gemeinden_overall[y])
    output_dw_kantone$Text_de[y] <- paste0("Es sind <b>",output_dw_kantone$Gemeinden_counted[y]," von ",output_dw_kantone$Gemeinden_overall[y],"</b> Gemeinden ausgezählt.")
    output_dw_kantone$Text_fr[y] <- paste0("Le résultats de <b>",output_dw_kantone$Gemeinden_counted[y]," des ",output_dw_kantone$Gemeinden_overall[y]," communes sont connus.")
    output_dw_kantone$Text_it[y] <- paste0("I risultati di <b>",output_dw_kantone$Gemeinden_counted[y]," dei ",output_dw_kantone$Gemeinden_overall[y]," comuni sono noti.")
    
    if (output_dw_kantone$Ja_Stimmen_In_Prozent_Kanton[y] > 50) {
      output_dw_kantone$Kanton_color[y] <- 100
    } else {
      output_dw_kantone$Kanton_color[y] <- 0
    }  
    
  } else {
    
    output_dw_kantone$Kanton_color[y] <- output_dw_kantone$Ja_Stimmen_In_Prozent_Kanton[y]
    output_dw_kantone$Text_de[y] <- "Es sind alle Gemeinden ausgezählt."
    output_dw_kantone$Text_fr[y] <- "Les résultats de toutes les communes sont connus."
    output_dw_kantone$Text_it[y] <- "Tutti i comuni sono noti."

  }  
  
}
  
return(output_dw_kantone)


}
