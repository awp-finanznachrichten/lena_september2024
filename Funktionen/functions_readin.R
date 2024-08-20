#Meta-Infos zu Vorlagen aus JSON rauslesen

get_vorlagen <- function(dta_raw, sprache="de") {
  
  vorlagen_data <- dta_raw$schweiz$vorlagen$vorlagenTitel
  cat(paste0("Es wurden folgende ",length(vorlagen_data)," Abstimmungsvorlagen gefunden:\n"))
  
  vorlagen <- as.data.frame(vorlagen_data[[1]]) %>%
    filter(langKey ==  sprache) %>%
    mutate(nummer = 1)
  
  vorlagen$id <- dta_raw$schweiz$vorlagen$vorlagenId[1]
  
  for (i in 2:length(vorlagen_data)) {
    
    vorlagen_new <- as.data.frame(vorlagen_data[[i]]) %>%
      filter(langKey == sprache) %>%
      mutate(nummer = i)
    
    vorlagen_new$id <- dta_raw$schweiz$vorlagen$vorlagenId[i]
    
    vorlagen <- rbind(vorlagen,vorlagen_new)
    
  }  
  
  cat(vorlagen$text,sep="\n")
  
  return(vorlagen)
  
}


#Meta-Infos zu kantonalen Vorlagen aus JSON rauslesen

get_vorlagen_kantonal <- function(dta_raw, sprache="de") {
  
  vorlagen_data <- dta_raw$schweiz$vorlagen$vorlagenTitel
  cat(paste0("Es wurden folgende ",length(vorlagen_data)," Abstimmungsvorlagen gefunden:\n"))
  
  vorlagen <- as.data.frame(vorlagen_data[[1]]) %>%
    filter(langKey ==  sprache) %>%
    mutate(nummer = 1)
  
  vorlagen$id <- dta_raw$schweiz$vorlagen$vorlagenId[1]
  
  for (i in 2:length(vorlagen_data)) {
    
    vorlagen_new <- as.data.frame(vorlagen_data[[i]]) %>%
      filter(langKey == sprache) %>%
      mutate(nummer = i)
    
    vorlagen_new$id <- dta_raw$schweiz$vorlagen$vorlagenId[i]
    
    vorlagen <- rbind(vorlagen,vorlagen_new)
    
  }  
  
  cat(vorlagen$text,sep="\n")
  
  return(vorlagen)
  
}



#Resultate aus JSON-File lesen
get_results <- function(dta_raw,
                        object_nr = 1,
                        level = "communal",
                        format = F,
                        clean_comm = T,
                        add_ct_nr = T) {
  
  if(level == "national") {
    out <- dta_raw$schweiz$vorlagen[object_nr,] %>%
      select(-kantone)
    if(format) {
      names(out) <- gsub("resultat.", "", names(out))
      out <- format_data_ch(out)
    }
  }
  
  if(level == "cantonal") {
    out <- dta_raw$schweiz$vorlagen$kantone[[object_nr]] %>%
      mutate(Kantons_Nr = as.integer(geoLevelnummer)) %>%
      select(-gemeinden, -bezirke, -geoLevelnummer)
    if(format) {
      names(out) <- gsub("resultat.", "", names(out))
      out <- format_data_k(out)
    }
  }
  
  if(level == "district") {
    out <- map_df(dta_raw$schweiz$vorlagen$kantone[[object_nr]]$bezirke, function(x) x) %>%
      mutate(Bezirk_Nr = as.integer(geoLevelnummer)) %>%
      select(-geoLevelnummer)
  }
  
  if(level == "communal") {
    out <- map_df(dta_raw$schweiz$vorlagen$kantone[[object_nr]]$gemeinden, function(x) x) %>%
      mutate(Gemeinde_Nr = as.integer(geoLevelnummer)) %>%
      select(-geoLevelnummer)
    if(format) {
      names(out) <- gsub("resultat.", "", names(out))
      if(clean_comm) {
        out <- treat_gemeinden(out)
        out <- out %>% arrange(Gemeinde_Nr)
      }
      out <- format_data_g(out)
    }
    if(add_ct_nr) {
      out <- out %>% left_join(meta_gmd_kt, by = c("Gemeinde_Nr"="Gemeinde_Nr"))
    }
  }
  names(out) <- gsub("resultat.", "", names(out))
  names(out) <- gsub("staende.", "", names(out))

  return(out)
}

#Gemeindedaten formatieren
format_data_g <- function(results) {
  out <- results %>%
    select(Gemeinde_Nr,
           Ja_Stimmen_In_Prozent = jaStimmenInProzent,
           Ja_Stimmen_Absolut = jaStimmenAbsolut,
           Nein_Stimmen_Absolut = neinStimmenAbsolut,
           Stimmbeteiligung_In_Prozent = stimmbeteiligungInProzent,
           Eingelegte_Stimmzettel = eingelegteStimmzettel,
           Anzahl_Stimmberechtigte = anzahlStimmberechtigte,
           Gueltige_Stimmen = gueltigeStimmen,
           Gebiet_Ausgezaehlt = gebietAusgezaehlt,
           Kanton_Short,
           Gemeinde_d,
           Gemeinde_f,
           Gemeinde_i,
           Gemeinde_KT_d,
           Gemeinde_KT_f,
           Gemeinde_KT_i,
           Kanton_d,
           Kanton_f,
           Kanton_i,
           Kantons_Nr)
  return(out)
}




# Gemeindedaten säubern: Auslandschweizer aussortieren, Zürich/Winti zusammenfassen
treat_gemeinden <- function(res_comm) {
  # Auslandschweizer-"Gemeinden" aussortieren
  out <- res_comm %>% filter(Gemeinde_Nr < 9000 & Gemeinde_Nr <= 10000)
  # Zürich zusammenfassen
  zurich <- res_comm %>% 
    filter(Gemeinde_Nr == 10261 |
             Gemeinde_Nr == 20261 |
             Gemeinde_Nr == 30261 |
             Gemeinde_Nr == 40261 |
             Gemeinde_Nr == 50261 |
             Gemeinde_Nr == 60261 |
             Gemeinde_Nr == 70261 |
             Gemeinde_Nr == 80261 |
             Gemeinde_Nr == 90261) %>%
    group_by(geoLevelParentnummer) %>%
    summarise(geoLevelname = "Z?rich",
              gebietAusgezaehlt = ifelse(sum(gebietAusgezaehlt) == 9, T, F),
              jaStimmenAbsolut = sum(jaStimmenAbsolut),
              neinStimmenAbsolut = sum(neinStimmenAbsolut),
              jaStimmenInProzent = 100*jaStimmenAbsolut/(neinStimmenAbsolut+jaStimmenAbsolut),
              eingelegteStimmzettel = sum(eingelegteStimmzettel),
              anzahlStimmberechtigte = sum(anzahlStimmberechtigte),
              gueltigeStimmen = sum(gueltigeStimmen),
              stimmbeteiligungInProzent = 100*eingelegteStimmzettel/anzahlStimmberechtigte,
              Gemeinde_Nr = 261) %>%
    ungroup() %>%
    mutate(geoLevelParentnummer = "112")
  # Winterthur zusammenfassen
  winti <- res_comm %>% 
    filter(Gemeinde_Nr == 10230 |
             Gemeinde_Nr == 20230 |
             Gemeinde_Nr == 30230 |
             Gemeinde_Nr == 40230 |
             Gemeinde_Nr == 50230 |
             Gemeinde_Nr == 60230 |
             Gemeinde_Nr == 70230) %>%
    group_by(geoLevelParentnummer) %>%
    summarise(geoLevelname = "Winterthur",
              gebietAusgezaehlt = ifelse(sum(gebietAusgezaehlt) == 7, T, F),
              jaStimmenAbsolut = sum(jaStimmenAbsolut),
              neinStimmenAbsolut = sum(neinStimmenAbsolut),
              jaStimmenInProzent = 100*jaStimmenAbsolut/(neinStimmenAbsolut+jaStimmenAbsolut),
              eingelegteStimmzettel = sum(eingelegteStimmzettel),
              anzahlStimmberechtigte = sum(anzahlStimmberechtigte),
              gueltigeStimmen = sum(gueltigeStimmen),
              stimmbeteiligungInProzent = 100*eingelegteStimmzettel/anzahlStimmberechtigte,
              Gemeinde_Nr = 230) %>%
    ungroup() %>%
    mutate(geoLevelParentnummer = "110")
  
  out <- bind_rows(out, zurich)
  out <- bind_rows(out, winti)
}

augment_raw_data <- function(dta_raw) {

  dta <- dta_raw %>%
    mutate(Ja_Nein = ifelse(Ja_Stimmen_Absolut > Nein_Stimmen_Absolut, "Ja", "Nein")) %>%
    mutate(Oui_Non = ifelse(Ja_Stimmen_Absolut > Nein_Stimmen_Absolut, "oui", "non")) %>%
    mutate(Nein_Stimmen_In_Prozent = 100 - Ja_Stimmen_In_Prozent) %>%
    mutate(Gemeinde_color = Ja_Stimmen_In_Prozent) %>%
    mutate(Unentschieden = ifelse(Ja_Stimmen_Absolut == Nein_Stimmen_Absolut, TRUE, FALSE)) %>%
    mutate(Einstimmig_Ja = ifelse(Ja_Stimmen_In_Prozent == 100, TRUE, FALSE)) %>%
    mutate(Einstimmig_Nein = ifelse(Ja_Stimmen_In_Prozent == 0, TRUE, FALSE)) %>%
    mutate(kleine_Gemeinde = ifelse(Eingelegte_Stimmzettel <= 100, TRUE, FALSE))
  
  return(dta)
}


#Historische Daten formatieren
format_data_hist <- function(results) {
  out <- results %>%
    select(Gemeinde_Nr = `Gemeinde-Nummer`,
           Hist_Ja_Stimmen_In_Prozent = `Ja in Prozent`,
           Hist_Ja_Stimmen_Absolut = `Ja-Stimmen`,
           Hist_Nein_Stimmen_Absolut = `Nein-Stimmen`) %>%
    mutate(Hist_Nein_Stimmen_In_Prozent = 100 - Hist_Ja_Stimmen_In_Prozent)
  
  out <- na.omit(out)
  return(out)
}

#Resultate der kantonalen Abstimmungen aus JSON-File lesen
get_results_kantonal <- function(dta_raw,
                                 kanton_nr = 1,
                                 abst_nr = 1,
                                 level = "communal",
                                 format = F,
                                 clean_comm = T,
                                 add_ct_nr = T) {
  
  
  if(level == "communal") {
    out <- map_df(dta_raw$kantone$vorlagen[[kanton_nr]]$gemeinden[[abst_nr]], function(x) x) %>%
      mutate(Gemeinde_Nr = as.integer(geoLevelnummer)) %>%
      select(-geoLevelnummer)
    if(format) {
      names(out) <- gsub("resultat.", "", names(out))
      if(clean_comm) {
        out <- treat_gemeinden(out)
        out <- out %>% arrange(Gemeinde_Nr)
      }
      out <- format_data_g(out)
    }
    if(add_ct_nr) {
      out <- out %>% left_join(meta_gmd_kt, by = c("Gemeinde_Nr"="Gemeinde_Nr"))
    }
    
    
    names(out) <- gsub("resultat.", "", names(out))
    names(out) <- gsub("staende.", "", names(out))
    
  }
  
  if(level == "kantonal") {
    
    out <- as.numeric(dta_raw$kantone$vorlagen[[kanton_nr]]$resultat.jaStimmenInProzent[[abst_nr]])
    
  }
  
  
  return(out)
}




cat("Funktionen geladen\n")
