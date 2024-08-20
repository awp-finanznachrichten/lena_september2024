library(readxl)
organspender_korrelation <- na.omit(read_excel("Data/organe_korrelation.xlsx"))

(cor(organspender_korrelation$Organspender,organspender_korrelation$Transplantation))^2

