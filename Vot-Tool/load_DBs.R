###GET CURRENT RESULTS ###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, "SELECT * FROM cantons_results")
cantons_results <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

cantons_results <- cantons_results %>%
  filter(grepl(paste(vorlagen$id,collapse="|"),votes_ID) == TRUE)


###GET OUTPUT OVERVIEW###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM output_overview WHERE date = '",voting_date,"' AND area_ID != 'CH' AND voting_type = 'national'"))
output_overview <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

###GET OUTPUT OVERVIEW NATIONAL###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM output_overview WHERE date = '",voting_date,"' AND area_ID = 'CH'"))
output_overview_national <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

###GET OUTPUT NEWS INTERMEDIATE###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, "SELECT * FROM output_news_intermediate WHERE news_intermediate = 'pending'")
output_news_intermediate <- DBI::fetch(rs,n=-1)
dbDisconnectAll()

###GET OUTPUT FLASHES###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, "SELECT * FROM output_flashes")
output_flashes <- DBI::fetch(rs,n=-1)
dbDisconnectAll()


###GET EXTRAPOLATIONS###
mydb <- connectDB(db_name="sda_votes")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM extrapolations"))
extrapolations <- DBI::fetch(rs,n=-1)
dbDisconnectAll()