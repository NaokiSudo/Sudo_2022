data1 <- read.table("policyp.txt")
data2 <- read.table("policyp2.txt")

library(psych)
library(dplyr)
library(ggplot2)

data1 <- select(data1, lhincome)
data2 <- select(data2, lhincome)

data3 <- cbind(data1, data2)

describe(data3, skew = F) # Table 1

f1 <- ggplot(data1, aes(lhincome))+geom_density(color = "red", 
                                                linetype="dashed")
f1 <- f1 + geom_density(aes(data2$lhincome), color="blue")
f1 <- f1 + xlab("Household Income(log)")

f1 # Figure 1
