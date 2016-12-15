#Power Analysis and species probability of detection


##Load Packages and data
#load packages
library(unmarked)
library(caret)
library(ggplot2)
library(texreg)
#load each species model
best2.My.Th2 <- readRDS("best2.My.Th2.rds")
best2.Ta.Br2 <- readRDS("best2.Ta.Br2.rds")
best2.Pa.He2 <- readRDS("best2.Pa.He2.rds")
best2.My.Yu2 <- readRDS("best2.My.Yu2.rds")
best2.My.Lu2 <- readRDS("best2.My.Lu2.rds")
best2.My.Ev2 <- readRDS("best2.My.Ev2.rds")
best2.My.Ca2 <- readRDS("best2.My.Ca2.rds")
best2.My.Ci2 <- readRDS("best2.My.Ci2.rds")
best2.La.No2 <- readRDS("best2.La.No2.rds")
best2.La.Ci2 <- readRDS("best2.La.Ci2.rds")
best2.La.Bl2 <- readRDS("best2.La.Bl2.rds")
best2.Eu.Ma2 <- readRDS("best2.Eu.Ma2.rds")
best2.Eu.Pe2 <- readRDS("best2.Eu.Pe2.rds")
best2.Ep.Fu2 <- readRDS("best2.Ep.Fu2.rds")
best2.An.Pa2 <- readRDS("best2.An.Pa2.rds")
best2.My.Vo2 <- readRDS("best2.My.Vo2.rds")
best2.Co.To2 <- readRDS("best2.Co.To2.rds")


modelos <- list(best2.An.Pa2, best2.Ep.Fu2, best2.Eu.Pe2, best2.Eu.Ma2, best2.La.Bl2, best2.La.Ci2, best2.La.No2, best2.My.Ci2, best2.My.Ca2, best2.My.Ev2, best2.My.Lu2, best2.My.Yu2, best2.Pa.He2, best2.Ta.Br2, best2.My.Th2, best2.My.Vo2, best2.Co.To2)
especies <- c("ANPA", "EPFU", "EUPE", "EUMA", "LABL", "LACI", "LANO", "MYCI",  "MYCA", "MYEV", "MYLU", "MYYU", "PAHE", "TABR","MYTH", "MYVO", "COTO")
#load prprocessing and variables
preprocov <- readRDS("preprocov.rds")
sampling.cov <- readRDS("sampling.cov.rds")
preproDaily <- readRDS("preproDaily.rds")
Dailycov <- readRDS("Dailycov.rds")

#generate a dataframe filed with means called output
means <- colMeans(sampling.cov, na.rm = TRUE)
output <- matrix(ncol=ncol(sampling.cov), nrow=65)

for(i in 1:ncol(sampling.cov)){
  output[,i] <-  rep(means[i], times = 65)
  
}
output <- data.frame(output)
colnames(output) <- colnames(sampling.cov)

output2 <- output[1,]

#generate a dataframe filed with means for detection variables called output

meansDet <- predict(preproDaily, Dailycov)
meansDet <- colMeans(meansDet, na.rm = TRUE)
Julian<-mean(meansDet[1:3])
Max.hum<-mean(meansDet[4:6])
Max.temp<-mean(meansDet[7:9])
Mean.hum<-mean(meansDet[10:12])
Mean.temp<-mean(meansDet[13:15])
Min.hum<-mean(meansDet[16:18])
Min.temp<-mean(meansDet[19:21])
Sd.hum<-mean(meansDet[22:24])
Sd.temp<-mean(meansDet[25:27])

meansDet<- data.frame(Julian,Max.hum, Max.temp, Mean.hum,Mean.temp, Min.hum, Min.temp, Sd.hum, Sd.temp)
colnames(meansDet) <- c("Julian", "Maxhum","Maxtemp","Meanhum", "Meantemp","Minhum","Mintemp","sdhum","sdtemp")

MeanProb <- data.frame(spp= especies, PSI = rep(NA, 17), P = rep(NA, 17), Days = rep(NA, 17))
#Myotis thysanodes
for(i in 1:length(especies)){
MeanProb$PSI[i] <- round(predict(modelos[[i]], type = "state", predict(preprocov, output2))$Predicted,2)
MeanProb$PSI_SE[i] <- round(predict(modelos[[i]], type = "state", predict(preprocov, output2))$SE,2)
MeanProb$P[i] <- round(predict(modelos[[i]], type = "det", meansDet)$Predicted,2)
MeanProb$P_SE[i] <- round(predict(modelos[[i]], type = "det", meansDet)$SE,2)
}

MeanProb

s = rep(1, 17)
a = rep(0, 17)
for (i in 1:NROW(MeanProb)){
while (a[i] < 0.95) {
  a[i] <- (1-(1-MeanProb$P[i])^s[i])
  s[i] = s[i]+1
  }
  MeanProb$Days[i] <- s[i]
}

MeanProb
write.csv(MeanProb, "MeanProb.csv")

########################
Grandtotal <- read.csv("~/LastBat/Grandtotal.csv")
Grandtotal <- Grandtotal[,-1]
BatOccu <- Grandtotal[,2:52]
                         
presence <- function (pres, spp) 
{
  secuencia <- c(1:spp) * (ncol(pres)/spp)
  secuencia2 <- secuencia - (secuencia[1] - 1)
  presabs <- list()
  data <- list()
  n <- list()
  for (i in 1:length(secuencia)) {
    data[[i]] <- c(secuencia2[i]:secuencia[i])
    data[[i]] <- pres[, data[[i]]]
    presabs[[i]] <- rowSums(data[[i]], na.rm = TRUE)
    presabs[[i]] <- ifelse(presabs[[i]] > 0, 1, 0)
    presabs = as.data.frame(presabs)
    colnames(presabs) = paste("species", c(1:ncol(presabs)), sep = ".")
    n <- colSums(presabs)
    
  }
  
  result <- list(presabs = presabs, n = n)
  return(result)
}

a<-presence(BatOccu, 17)
number.per.site<-a$n

names(number.per.site) <- c("Myyu","Myca","Myci","Myvo","Mylu","Pahe","Labo","Myev","Anpa","Epfu","Lano","Myth","Tabr","Laci","Coto","Euma","Eupe")

write.csv(data.frame(number.per.site), "Number_per_site.csv")