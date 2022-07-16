library(mixor)

data_h <- read.table("policyp3.txt")

result1 <- mixor(egalita~aging+age+age2+women+umari+dmari+
                  high+low+lhincome+
                  jiei+iemp+nojob+jobseek+uwhite+blue, data_h, id=gid, link = "logit")
summary(result1)

cm <- matrix(c(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
                   ), ncol = 4)
Contrasts(result1, contrast.matrix = cm)

result2 <- mixor(neolibe~aging+age+age2+women+umari+dmari+
                  high+low+lhincome+
                  jiei+iemp+nojob+jobseek+uwhite+blue, data_h, id=gid, link = "logit")
summary(result2)

cm <- matrix(c(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
), ncol = 4)
Contrasts(result2, contrast.matrix = cm)

AIC.mixor(result1);AIC.mixor(result2)
BIC.mixor(result1); BIC.mixor(result2)

result3 <- mixor(egalita~aging:lhincome+age+age2+women+umari+dmari+
                   high+low+lhincome+
                   jiei+iemp+nojob+jobseek+uwhite+blue, data_h, id=gid, link = "logit")
summary(result3)

cm <- matrix(c(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
), ncol = 4)
Contrasts(result3, contrast.matrix = cm)

result4 <- mixor(neolibe~aging:lhincome+age+age2+women+umari+dmari+
                   high+low+lhincome+
                   jiei+iemp+nojob+jobseek+uwhite+blue, data_h, id=gid, link = "logit")
summary(result4)

cm <- matrix(c(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
               -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
), ncol = 4)
Contrasts(result4, contrast.matrix = cm)

AIC.mixor(result3);AIC.mixor(result4)
BIC.mixor(result3);BIC.mixor(result4)

