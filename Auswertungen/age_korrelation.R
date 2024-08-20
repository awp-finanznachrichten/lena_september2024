library(readxl)
library(ggpubr)
age_korrelation <- na.omit(read_excel("Data/age_korrelation.xlsx"))

(cor(age_korrelation$age,age_korrelation$vote_result))^2
cor.test(age_korrelation$age,age_korrelation$vote_result,paired = TRUE)

ggscatter(age_korrelation, x = "age", y = "vote_result", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Anteil ü65-Jährige", ylab = "Ja-Anteil 13. AHV-Rente")


