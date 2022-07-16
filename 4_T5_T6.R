library(psych)
library(dplyr)
library(texreg)
library(MASS)

data_h <- read.table("policyp3.txt")
data_h$egalitaC <- factor(data_h$egalitaC)
data_h$neolibeC <- factor(data_h$neolibeC)

result3 <- polr(egalitaC~age+age2+women+umari+dmari+
                  high+low+jiei+iemp+nojob+jobseek+uwhite+blue+
                  lhincome+
                  aging,data_h)
result4 <- polr(neolibeC~age+age2+women+umari+dmari+
                  high+low+jiei+iemp+nojob+jobseek+uwhite+blue+
                  lhincome+
                  aging, data_h)
result5 <- polr(egalitaC~age+age2+women+umari+dmari+
                  high+low+jiei+iemp+nojob+jobseek+uwhite+blue+
                  lhincome+
                  aging:lhincome,data_h)
result6 <- polr(neolibeC~age+age2+women+umari+dmari+
                  high+low+jiei+iemp+nojob+jobseek+uwhite+blue+
                  lhincome+
                  aging:lhincome, data_h)


Table5 <- screenreg(list(result4, result6),digits = 3)
Table5

Table6 <- screenreg(list(result3, result5),digits = 3)
Table6

