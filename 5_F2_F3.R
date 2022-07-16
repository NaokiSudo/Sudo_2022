library(dplyr)
library(ggplot2)
library(MASS)
library(effects)

data_h <- read.table("policyp3.txt")
data_h$egalitaC <- factor(data_h$egalita)
data_h$neolibeC <- factor(data_h$neolibe)

cutpoint <- quantile(data_h$lhincome, c(0.50))

data_h1 <- subset(data_h, lhincome<=cutpoint)
data_h2 <- subset(data_h, lhincome >cutpoint)

result1 <- polr(egalitaC~age+age2+women+umari+dmari+
                  high+low+jiei+iemp+nojob+jobseek+uwhite+blue+
                  lhincome+
                  aging,data_h1)
result2 <- polr(egalitaC~age+age2+women+umari+dmari+
                  high+low+jiei+iemp+nojob+jobseek+uwhite+blue+
                  lhincome+
                  aging,data_h2)

result3 <- polr(neolibeC~age+age2+women+umari+dmari+
                  high+low+jiei+iemp+nojob+jobseek+uwhite+blue+
                  lhincome+
                  aging, data_h1)
result4 <- polr(neolibeC~age+age2+women+umari+dmari+
                  high+low+jiei+iemp+nojob+jobseek+uwhite+blue+
                  lhincome+
                  aging, data_h2)

g.eff1 <- Effect(focal.predictors = c("aging"), result1)
effect1 <- data.frame(g.eff1)

g.eff2 <- Effect(focal.predictors = c("aging") , result2)
effect2 <- data.frame(g.eff2)

g.eff3 <- Effect(focal.predictors = c("aging"), result3)
effect3 <- data.frame(g.eff3)

g.eff4 <- Effect(focal.predictors = c("aging") , result4)
effect4 <- data.frame(g.eff4)

dataR <- rbind(effect1, effect2)
dataR$Income_level <- "Low"
dataR$Income_level[6:10] <- "High"

dataF <- rbind(effect3, effect4)
dataF$Income_level <- "Low"
dataF$Income_level[6:10] <- "High"

f5 <- ggplot(dataR, aes(aging, prob.X5, 
                        ymin=prob.X5-1.96*se.prob.X5, 
                        ymax=prob.X5+1.96*se.prob.X5, 
                        shape=Income_level,
                        color=Income_level))+
  geom_pointrange()+geom_line()+theme(legend.position = "none")
f5 <- f5 + labs(x="Portion of elderly population (%)",
           subtitle="Redisribution=5", y="Probability")

f4 <- ggplot(dataR, aes(aging, prob.X4, 
                        ymin=prob.X4-1.96*se.prob.X4, 
                        ymax=prob.X4+1.96*se.prob.X4, 
                        shape=Income_level,
                        color=Income_level))+
  geom_pointrange()+geom_line()+theme(legend.position = "none")
f4 <- f4 + labs(x="Portion of elderly population (%)", 
            subtitle="Redisribution=4", y="Probability")

f3 <- ggplot(dataR, aes(aging, prob.X3, 
                        ymin=prob.X3-1.96*se.prob.X3, 
                        ymax=prob.X3+1.96*se.prob.X3, 
                        shape=Income_level,
                        color=Income_level))+
  geom_pointrange()+geom_line()+theme(legend.position = "none")
f3 <- f3 + labs(x="Portion of elderly population (%)", 
  subtitle="Redisribution=3", y="Probability")

f2 <- ggplot(dataR, aes(aging, prob.X2, 
                        ymin=prob.X2-1.96*se.prob.X2, 
                        ymax=prob.X2+1.96*se.prob.X2, 
                        shape=Income_level,
                        color=Income_level))+
  geom_pointrange()+geom_line()+theme(legend.position = "none")
f2 <- f2 + labs(x="Portion of elderly population (%)",
  subtitle="Redisribution=2", y="Probability")

f1 <- ggplot(dataR, aes(aging, prob.X1, 
                        ymin=prob.X1-1.96*se.prob.X1, 
                        ymax=prob.X1+1.96*se.prob.X1, 
                        shape=Income_level,
                        color=Income_level))+
  geom_pointrange()+geom_line()+theme(legend.position = "none")
f1 <- f1 + labs(x="Portion of elderly population (%)", 
  subtitle="Redisribution=1", y="Probability")

g5 <- ggplot(dataF, aes(aging, prob.X5, 
                        ymin=prob.X5-1.96*se.prob.X5, 
                        ymax=prob.X5+1.96*se.prob.X5, 
                        shape=Income_level,
                        color=Income_level))+
  geom_pointrange()+geom_line()+theme(legend.position = "none")
g5 <- g5 + labs(x="Portion of elderly population (%)",
  subtitle="Free Competition=5", y="Probability")

g4 <- ggplot(dataF, aes(aging, prob.X4, 
                        ymin=prob.X4-1.96*se.prob.X4, 
                        ymax=prob.X4+1.96*se.prob.X4, 
                        shape=Income_level,
                        color=Income_level))+
  geom_pointrange()+geom_line()+theme(legend.position = "none")
g4 <- g4 + labs(x="Portion of elderly population (%)", 
  subtitle="Free Competition=4", y="Probability")

g3 <- ggplot(dataF, aes(aging, prob.X3, 
                        ymin=prob.X3-1.96*se.prob.X3, 
                        ymax=prob.X3+1.96*se.prob.X3, 
                        shape=Income_level,
                        color=Income_level))+
  geom_pointrange()+geom_line()+theme(legend.position = "none")
g3 <- g3 + labs(x="Portion of elderly population (%)",
  subtitle="Free Competition=3", y="Probability")

g2 <- ggplot(dataF, aes(aging, prob.X2, 
                        ymin=prob.X2-1.96*se.prob.X2, 
                        ymax=prob.X2+1.96*se.prob.X2, 
                        shape=Income_level,
                        color=Income_level))+
  geom_pointrange()+geom_line()+theme(legend.position = "none")
g2 <- g2 + labs(x="Portion of elderly population (%)", 
  subtitle="Free Competition=2", y="Probability")

g1 <- ggplot(dataF, aes(aging, prob.X1, 
                        ymin=prob.X1-1.96*se.prob.X1, 
                        ymax=prob.X1+1.96*se.prob.X1, 
                        shape=Income_level,
                        color=Income_level))+
  geom_pointrange()+geom_line()+theme(legend.position = "none")
g1 <- g1 + labs(x="Portion of elderly population (%)", 
  subtitle="Free Competition=1", y="Probability")

gridExtra::grid.arrange(g5,g4,g3,g2,g1) #Figure 2
gridExtra::grid.arrange(f5,f4,f3,f2,f1) #Figure 3
