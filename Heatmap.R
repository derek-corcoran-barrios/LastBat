occupancytable2 <- read.csv("~/LastBat/occupancytable2.csv")
rownames(occupancytable2) <- occupancytable2[,2]
library(ggplot2)


library(reshape2)
occupancytable2 <- occupancytable2[-18,-c(1,2,9)]
colnames(occupancytable2)<- c("Burn Basal", "Burn Basal^2", "Burn Canopy", "Burn Canopy^2", "Fire Distance", "Forest Distance")
occupancytable2$species <- rownames(occupancytable2)
dfm <- melt(occupancytable2, id.vars="species")
dfm$sign <- ifelse(dfm$value > 0 , "+" , ifelse(dfm$value < 0, "-", ""))


p <- ggplot(dfm, aes(x=variable, y=species)) +  geom_tile(aes(fill=value), colour = "black") + scale_fill_gradient2(low = "blue", high = 'red', name="Strength") +
theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+ geom_text(aes(label=sign), color = 'black')
p

Detectiontable2 <- read.csv("~/LastBat/Detectiontable2.csv")
rownames(Detectiontable2) <- Detectiontable2[,2]
Detectiontable2 <- Detectiontable2[-18,-c(1,2,12)]
Detectiontable2$species <- rownames(Detectiontable2)
dfm2 <- melt(Detectiontable2, id.vars="species")
dfm2$sign <- ifelse(dfm2$value > 0 , "+" , ifelse(dfm2$value < 0, "-", ""))
p <- ggplot(dfm2, aes(x=variable, y=species)) +  geom_tile(aes(fill=value), colour = "black") + scale_fill_gradient2(low = "blue", high = 'red') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+ geom_text(aes(label=sign), color = 'black')
p


#####################################################################
################for paper##############################################
################################################################

occupancytable2 <- read.csv("~/LastBat/occupancytable2.csv")
rownames(occupancytable2) <- occupancytable2[,2]
library(ggplot2)


library(reshape2)
occupancytable2 <- occupancytable2[-18,-c(1,2,9)]
colnames(occupancytable2)<- c("Burn Basal", "Burn Basal^2", "Burn Canopy", "Burn Canopy^2", "Fire Distance", "Forest Distance")
occupancytable2$species <- rownames(occupancytable2)
dfm <- melt(occupancytable2, id.vars="species")
dfm$sign <- ifelse(dfm$value > 0 , "+" , ifelse(dfm$value < 0, "-", ""))


p <- ggplot(dfm, aes(x=variable, y=species))

p <- p +  geom_tile(aes(fill=value), colour = "black") 
p <- p + scale_fill_gradient2(low = 'gray23', high = 'gray23', name="Strength", na.value = "white", mid = "grey80") 
p <- p + theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))
p <- p + geom_text(aes(label=sign), color = 'black') 
p


Detectiontable2 <- read.csv("~/LastBat/Detectiontable2.csv")
rownames(Detectiontable2) <- Detectiontable2[,2]
Detectiontable2 <- Detectiontable2[-18,-c(1,2,12)]
Detectiontable2$species <- rownames(Detectiontable2)
dfm2 <- melt(Detectiontable2, id.vars="species")
dfm2$sign <- ifelse(dfm2$value > 0 , "+" , ifelse(dfm2$value < 0, "-", ""))
p <- ggplot(dfm2, aes(x=variable, y=species)) +  geom_tile(aes(fill=value)) +  scale_fill_gradient2(low = "gray100", high = 'gray38', name="Strength") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+ geom_text(aes(label=sign), color = 'black')
p



