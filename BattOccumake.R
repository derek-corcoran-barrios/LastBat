setwd("C:/Users/corcoranbarriosd/Documents/LastBat")


###list og batchsummaryfiles
ID.of.files<-gsub("i", "",substr(grep("BatchSummary", list.files(recursive = T), value=TRUE), 1, 7))
ID.of.files<-gsub("I", "", ID.of.files)
ID.of.files<-gsub("/", "", ID.of.files)

##Extract the batchsummaryfiles night number
yap <- strsplit(grep("BatchSummary", list.files(recursive = T), value=TRUE), "/")
Rates <- sapply(yap, "[", 2)
num <- as.numeric(gsub("^.*[Nn].*([0-9]+).*$", "\\1", Rates))
#name the files acording to ID and night number
order.of.files <- paste(ID.of.files, num, sep = "n")

ab <- list()

for (i in 1:length(grep("BatchSummary", list.files(recursive = T), value=TRUE))){
  #read all files
  ab[[i]]<-read.delim(grep("BatchSummary", list.files(recursive = T), value=TRUE)[[i]], comment.char="#")[1:2,-19]
  #acces the consensus only
  ab[[i]] <- ab[[i]][-1,-1]
  #turn values to 1 and 0 for presence absense
  ab[[i]] <- data.frame(ifelse(ab[[i]] == 0, 0, 1))
  #add the id according to each row
  ab[[i]]$ID <- order.of.files[i]
}

#transform the previus list into a dataframe
df <- do.call("rbind", ab)




#select n1 values

str(df[grepl("n3",df[,18]),])

BatOccu2<- data.frame(ID = unique(ID.of.files), Myyu1 = df[grepl("n1",df[,18]),1], Myyu2 = df[grepl("n2",df[,18]),1], Myyu3 = df[grepl("n3",df[,18]),1], 
                                                Myca1 = df[grepl("n1",df[,18]),2], Myca2 = df[grepl("n2",df[,18]),2], Myca3 = df[grepl("n3",df[,18]),2],
                                                Myci1 = df[grepl("n1",df[,18]),3], Myci2 = df[grepl("n2",df[,18]),3], Myci3 = df[grepl("n3",df[,18]),3],
                                                Myvo1 = df[grepl("n1",df[,18]),4], Myvo2 = df[grepl("n2",df[,18]),4], Myvo3 = df[grepl("n3",df[,18]),4], 
                                                Mylu1 = df[grepl("n1",df[,18]),5], Mylu2 = df[grepl("n2",df[,18]),5], Mylu3 = df[grepl("n3",df[,18]),5],
                                                Pahe1 = df[grepl("n1",df[,18]),6], Pahe2 = df[grepl("n2",df[,18]),6], Pahe3 = df[grepl("n3",df[,18]),6],
                                                Labl1 = df[grepl("n1",df[,18]),7], Labl2 = df[grepl("n2",df[,18]),7], Labl3 = df[grepl("n3",df[,18]),7],
                                                Myev1 = df[grepl("n1",df[,18]),8], Myev2 = df[grepl("n2",df[,18]),8], Myev3 = df[grepl("n3",df[,18]),8], 
                                                Anpa1 = df[grepl("n1",df[,18]),9], Anpa2 = df[grepl("n2",df[,18]),9], Anpa3 = df[grepl("n3",df[,18]),9],
                                                Epfu1 = df[grepl("n1",df[,18]),10], Epfu2 = df[grepl("n2",df[,18]),10], Epfu3 = df[grepl("n3",df[,18]),10],
                                                Lano1 = df[grepl("n1",df[,18]),11], Lano2 = df[grepl("n2",df[,18]),11], Lano3 = df[grepl("n3",df[,18]),11],
                                                Myth1 = df[grepl("n1",df[,18]),12], Myth2 = df[grepl("n2",df[,18]),12], Myth3 = df[grepl("n3",df[,18]),12],
                                                Tabr1 = df[grepl("n1",df[,18]),13], Tabr2 = df[grepl("n2",df[,18]),13], Tabr3 = df[grepl("n3",df[,18]),13],
                                                Laci1 = df[grepl("n1",df[,18]),14], Laci2 = df[grepl("n2",df[,18]),14], Laci3 = df[grepl("n3",df[,18]),14], 
                                                Coto1 = df[grepl("n1",df[,18]),15], Coto2 = df[grepl("n2",df[,18]),15], Coto3 = df[grepl("n3",df[,18]),15],
                                                Euma1 = df[grepl("n1",df[,18]),16], Euma2 = df[grepl("n2",df[,18]),16], Euma3 = df[grepl("n3",df[,18]),16],
                                                Eupe1 = df[grepl("n1",df[,18]),17], Eupe2 = df[grepl("n2",df[,18]),17], Eupe3 = df[grepl("n3",df[,18]),17])


BatOccu <- read.csv("~/new_bats/Rnew_bats/BatOccu.csv")

BatOccu <- rbind(BatOccu, BatOccu2)

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

a<-presence(BatOccu[,-1], 17)
number.per.site<-a$n

names(number.per.site) <- c("Myyu","Myca","Myci","Myvo","Mylu","Pahe","Labo","Myev","Anpa","Epfu","Lano","Myth","Tabr","Laci","Coto","Euma","Eupe")

##### basal Area 

BasalArea <- read.csv("~/Jacqui/BasalArea.csv")

library(dplyr)
BasalArea<-mutate(BasalArea, Area = (((DIAMETER*0.01)/2)^2)*pi)
BasalArea<-summarise(group_by(BasalArea, ID), AreaPerHa=sum(Area)*10, Average = mean(Area), Sd = sd(Area, na.rm = TRUE), count = n())
BasalArea[is.na(BasalArea)] <- 0

####Results
RESULTS <- read.csv("~/Jacqui/RESULTS.csv")
RESULTS <- mutate(RESULTS, CANOPY.COVER=((1.04*CANOPY.COVER.N)+(1.04*CANOPY.COVER.E)+(1.04*CANOPY.COVER.W)+(1.04*CANOPY.COVER.S)+(CANOPY.COVER.C*1.04))/5)
RESULTS<-RESULTS[,-c(6:10)]

RESULTS <- mutate(RESULTS, WOODY= 100*((WOOD.N+WOOD.E+WOOD.S+WOOD.W)/(4*length(RESULTS[,1]))))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, HERBACIOUS= 100*((HERB.N+HERB.E+HERB.S+HERB.W)/(4*length(RESULTS[,1]))))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, GRASS= 100*((GRASS.N+GRASS.E+GRASS.S+GRASS.W)/(4*length(RESULTS[,1]))))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, NAKED.SOIL= 100*((NAKED.SOIL.N+NAKED.SOIL.E+NAKED.SOIL.S+NAKED.SOIL.W)/(4*length(RESULTS[,1]))))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, ROCKY= 100*((ROCKY.SCREE.N+ROCKY.SCREE.E+ROCKY.SCREE.S+ROCKY.SCREE.W)/(4*length(RESULTS[,1]))))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, DOWN.WOOD= 100*((DOWN.WOOD.N+DOWN.WOOD.E+DOWN.WOOD.S+DOWN.WOOD.W)/(4*length(RESULTS[,1]))))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, LEAF.LITTER= 100*((LEAF.LITTER.N+LEAF.LITTER.E+LEAF.LITTER.S+LEAF.LITTER.W)/(4*length(RESULTS[,1]))))
RESULTS<-RESULTS[,-c(6:9)]

##### Join databases

total <- merge(BatOccu,BasalArea,by="ID", all = TRUE)

total <- merge(total,RESULTS,by="ID", all = TRUE)




######TempHum


setwd("C:/Users/corcoranbarriosd/Documents/LastBat/T&H")
#get the name of all humidity files
Humname <- list.files(pattern="*Hum.csv")
#read all the humidity files
Humfiles <- lapply(Humname, read.csv)
#change humname to be the ID of the site
Humname <- gsub("Hum.csv",x =  Humname, "")
#Add the ID of the site to the Humfiles
for (i in 1:length(Humfiles)){
  Humfiles[[i]]$ID <- rep(Humname[i], times = nrow(Humfiles[[i]]))
}


Temname <- list.files(pattern="*Tem.csv")
Temfiles <- lapply(Temname, read.csv)
Temname <- gsub("Tem.csv",x =  Temname, "")
for (i in 1:length(Temfiles)){
  Temfiles[[i]]$ID <- rep(Temname[i], times = nrow(Temfiles[[i]]))
}
setwd("C:/Users/corcoranbarriosd/Documents/LastBat")


####Get your dates right

library(lubridate)

for (i in 1:length(Temfiles)){
  Temfiles[[i]]$Date.Time <- dmy_hms(as.character(Temfiles[[i]]$Date.Time), truncated = 1)

}

Temfiles <- do.call("rbind", Temfiles)

for (i in 1:length(Humfiles)){
  Humfiles[[i]]$Date.Time <- dmy_hms(as.character(Humfiles[[i]]$Date.Time), truncated = 1)
  
}

Humfiles <- do.call("rbind", Humfiles)

total$START.DATE <- mdy(as.character(total$START.DATE))

total$END.DATE <- mdy(as.character(total$END.DATE))

total$Start.1 <- total$START.DATE
total$End.1 <- total$END.DATE - days (2)
total$Start.2 <- total$Start.1 + days (1)
total$End.2 <- total$End.1 + days (1)
total$Start.3 <- total$Start.2 + days (1)
total$End.3 <- total$End.2 + days (1)

#filter by dates
#DAY1
Humsummaryday1 <- list()

for (i in 1:length(total$ID)){
  Humsummaryday1[[i]] <- Humfiles %>%   filter(Humfiles$ID == total$ID[i]) %>% filter(Date.Time >= (total$Start.1[i]+ hours(19) + minutes(30))) %>% filter(Date.Time <= (total$End.1[i]+ hours(6)+ minutes(30)))  
}

Humsummaryday1 <- do.call("rbind", Humsummaryday1)
Humsummaryday1 <- summarise(group_by(Humsummaryday1, ID), meanhum1 = mean(Value), maxhum1 = max(Value), minhum1 = min(Value), sdhum1 = sd(Value),Julian1 = mean(as.numeric(julian(Date.Time, origin = ymd("2015-01-01")))))
Humsummaryday1$Julian1 <- ifelse(Humsummaryday1$Julian1 > 365, (Humsummaryday1$Julian1 - 365), Humsummaryday1$Julian1)

#Day2

Humsummaryday2 <- list()

for (i in 1:length(total$ID)){
  Humsummaryday2[[i]] <- Humfiles %>%   filter(Humfiles$ID == total$ID[i]) %>% filter(Date.Time >= (total$Start.2[i]+ hours(19) + minutes(30))) %>% filter(Date.Time <= (total$End.2[i]+ hours(6)+ minutes(30)))  
}

Humsummaryday2 <- do.call("rbind", Humsummaryday2)
Humsummaryday2 <- summarise(group_by(Humsummaryday2, ID), meanhum2 = mean(Value), maxhum2 = max(Value), minhum2 = min(Value), sdhum2 = sd(Value),Julian2 = mean(as.numeric(julian(Date.Time, origin = ymd("2015-01-01")))))
Humsummaryday2$Julian2 <- ifelse(Humsummaryday2$Julian2 > 365, (Humsummaryday2$Julian2 - 365), Humsummaryday2$Julian2)


Humsummary <- merge(Humsummaryday1, Humsummaryday2, by = "ID")

#Day3

Humsummaryday3 <- list()

for (i in 1:length(total$ID)){
  Humsummaryday3[[i]] <- Humfiles %>%   filter(Humfiles$ID == total$ID[i]) %>% filter(Date.Time >= (total$Start.3[i]+ hours(19) + minutes(30))) %>% filter(Date.Time <= (total$End.3[i]+ hours(6)+ minutes(30)))  
}



Humsummaryday3 <- do.call("rbind", Humsummaryday3)

Humsummaryday3 <- summarise(group_by(Humsummaryday3, ID), meanhum3 = mean(Value), maxhum3 = max(Value), minhum3 = min(Value), sdhum3 = sd(Value),Julian3 = mean(as.numeric(julian(Date.Time, origin = ymd("2015-01-01")))))
Humsummaryday3$Julian3 <- ifelse(Humsummaryday3$Julian3 > 365, (Humsummaryday3$Julian3 - 365), Humsummaryday3$Julian3)
Humsummary <- merge(Humsummary, Humsummaryday3, by = "ID")
######Temperature

Temsummaryday1 <- list()

for (i in 1:length(total$ID)){
  Temsummaryday1[[i]] <- Temfiles %>%   filter(Temfiles$ID == total$ID[i]) %>% filter(Date.Time >= (total$Start.1[i]+ hours(19) + minutes(30))) %>% filter(Date.Time <= (total$End.1[i]+ hours(6)+ minutes(30)))  
}

Temsummaryday1 <- do.call("rbind", Temsummaryday1)
Temsummaryday1 <- summarise(group_by(Temsummaryday1, ID), meanTem1 = mean(Value), maxTem1 = max(Value), minTem1 = min(Value), sdTem1 = sd(Value))

#Day2

Temsummaryday2 <- list()

for (i in 1:length(total$ID)){
  Temsummaryday2[[i]] <- Temfiles %>%   filter(Temfiles$ID == total$ID[i]) %>% filter(Date.Time >= (total$Start.2[i]+ hours(19) + minutes(30))) %>% filter(Date.Time <= (total$End.2[i]+ hours(6)+ minutes(30)))  
}

Temsummaryday2 <- do.call("rbind", Temsummaryday2)
Temsummaryday2 <- summarise(group_by(Temsummaryday2, ID), meanTem2 = mean(Value), maxTem2 = max(Value), minTem2 = min(Value), sdTem2 = sd(Value))

Temsummary <- merge(Temsummaryday1, Temsummaryday2, by = "ID")

#Day3

Temsummaryday3 <- list()

for (i in 1:length(total$ID)){
  Temsummaryday3[[i]] <- Temfiles %>%   filter(Temfiles$ID == total$ID[i]) %>% filter(Date.Time >= (total$Start.3[i]+ hours(19) + minutes(30))) %>% filter(Date.Time <= (total$End.3[i]+ hours(6)+ minutes(30)))  
}



Temsummaryday3 <- do.call("rbind", Temsummaryday3)

Temsummaryday3 <- summarise(group_by(Temsummaryday3, ID), meanTem3 = mean(Value), maxTem3 = max(Value), minTem3 = min(Value), sdTem3 = sd(Value))
Temsummary <- merge(Temsummary, Temsummaryday3, by = "ID")

#Add all together

TemHumSummary <- merge(Temsummary, Humsummary, by = "ID")

#Order by name
TemHumSummary <- TemHumSummary[ , order(names(TemHumSummary))]
library(raster)
library(rgdal)
Predictors_Stack <- readRDS("Predictors_Stack.rds")

total <- cbind(total, extract(Predictors_Stack,total[,58:57]))

#Grandtotal

Grandtotal <- merge(total, TemHumSummary, by = "ID", all = TRUE)

write.csv(Grandtotal, "Grandtotal.csv")
