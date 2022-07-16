library(psych)
library(dplyr)
library(texreg)
library(MASS)

data <- read.table("policyp.txt")
data_h <- data.frame(egalita=data$egalita, neolibe=data$neolibe, 
                     aging=data$aging, dense=data$dense,unempm=data$unempm,
                     migrant=data$migrant, finance=data$finance,area5=data$area5,
                     age=data$age, age2=data$age2, women=data$women,
                     high=data$high, middle=data$middle, low=data$low, 
                     mari=data$mari, umari=data$umari, dmari=data$dmari, 
                     jiei=data$jiei, emp=data$emp, iemp=data$iemp,  
                     uwhite= data$uwhite, lwhite=data$lwhite, blue=data$blue, nojob= data$nojob, 
                     jobseek= data$jobseek, hincome=data$hincome, lhincome=data$lhincome)

data_h <- data_h[order(data_h$area5),]

gid <- matrix(1, nrow(data_h),1)
for(i in 2:nrow(data_h)){
  if(data_h$area5[i]==data_h$area5[i-1]){gid[i]<-gid[i-1]}else{gid[i]<-gid[i-1]+1}
}

data_h$age <- data_h$age/100
data_h$age2 <- data_h$age2/10000
data_h$egalitaC <- factor(data_h$egalita)
data_h$neolibeC <- factor(data_h$neolibe)

result1 <- polr(egalitaC~age+age2+women+umari+dmari+
                  high+low+jiei+iemp+nojob+jobseek+uwhite+blue+
                  lhincome+
                  aging:lhincome,data_h)
result2 <- polr(neolibeC~age+age2+women+umari+dmari+
                  high+low+jiei+iemp+nojob+jobseek+uwhite+blue+
                  lhincome+
                  aging:lhincome, data_h)


tableA2 <- screenreg(list(result1, result2),digits = 3)
tableA2



