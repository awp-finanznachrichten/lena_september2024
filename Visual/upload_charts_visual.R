library(RCurl)

###Set path with the charts
setwd("C:/Users/simon/OneDrive/SDA_eidgenoessische_abstimmungen/20240922_LENA_Abstimmungen/Grafiken")

###Get Folders
folders <- list.dirs(recursive = FALSE)

###Upload chart in each folder
for (folder in folders) {
#Set wd
setwd(folder)

#Get name of .zip file
zipfile_name <- list.files(pattern=".zip")

#Upload data
ftp_adress <- paste0("ftp://ftp.keystone.ch/",zipfile_name)
ftpUpload(zipfile_name, ftp_adress,userpwd="keyg_in:5r6368vz")

#set wd back
setwd("..")

print(paste0("Chart ",zipfile_name," uploaded successfully"))
Sys.sleep(5)
}