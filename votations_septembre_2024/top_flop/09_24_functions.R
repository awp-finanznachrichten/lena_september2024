top_flop <- function(obj,order,langage) {
  
  obj <- obj %>%
    mutate(Nein_Stimmen_In_Prozent = 100-Ja_Stimmen_In_Prozent)
  
  if(order == "top")  {data_top <- obj %>%
    arrange(desc(Ja_Stimmen_In_Prozent)) %>%
    slice(1:50)
  }
  if(order == "flop") {data_flop <- obj %>%
    arrange(Ja_Stimmen_In_Prozent) %>%
    slice(1:50)
  }
  if(langage == "de" & order == "top") {output <- data_top %>%
    rename(Gemeinde = Gemeinde_KT_d,
           "Ja in Prozent" = Ja_Stimmen_In_Prozent) %>%
    select(Gemeinde,'Ja in Prozent')
  }
  if(langage == "de" & order == "flop") {output <- data_flop %>%
    rename(Gemeinde = Gemeinde_KT_d,
           "Nein in Prozent" = Nein_Stimmen_In_Prozent) %>%
    select(Gemeinde,'Nein in Prozent')
  }
  if(langage == "fr" & order == "top") {output <- data_top %>%
    rename(Commune = Gemeinde_KT_f,
           "Pourcentage de oui" = Ja_Stimmen_In_Prozent) %>%
    select(Commune,'Pourcentage de oui')
  }
  if(langage == "fr" & order == "flop") {output <- data_flop %>%
    rename(Commune = Gemeinde_KT_f,
           "Pourcentage de non" = Nein_Stimmen_In_Prozent) %>%
    select(Commune,'Pourcentage de non')
  }
  if(langage == "it" & order == "top") {output <- data_top %>%
    rename(Comune = Gemeinde_KT_i,
           "Percentuale di sì" = Ja_Stimmen_In_Prozent) %>%
    select(Comune,'Percentuale di sì')
  }
  if(langage == "it" & order == "flop") {output <- data_flop %>%
    rename(Comune = Gemeinde_KT_i,
           "Percentuale di no" = Nein_Stimmen_In_Prozent) %>%
    select(Comune,'Percentuale di no')
  }
  return(output)
}


plot_the_data_with_dwrp <- function(data,id,chart_title,chart_source) {
  
  dw_data_to_chart(data,id)
  
  dw_edit_chart(id,
                title = chart_title,
                source_name = chart_source)
  
  dw_publish_chart(id)
  
}
