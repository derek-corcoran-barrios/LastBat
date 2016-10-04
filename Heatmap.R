occupancytable2 <- read.csv("~/LastBat/occupancytable2.csv", row.names=1)

library(ggplot2)


library(reshape2)
occupancytable2 <- occupancytable2[-18,-7]
colnames(occupancytable2)<- c("Burn Basal", "Burn Basal^2", "Burn Canopy", "Burn Canopy^2", "Fire Distance", "Forest Distance")
occupancytable2$species <- rownames(occupancytable2)
dfm <- melt(occupancytable2, id.vars="species")
dfm$sign <- ifelse(dfm$value > 0 , "+" , ifelse(dfm$value < 0, "-", ""))


p <- ggplot(dfm, aes(x=variable, y=species)) +  geom_tile(aes(fill=value), colour = "black") + scale_fill_gradient2(low = "blue", high = 'red') +
theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+ geom_text(aes(label=sign))
p

Detectiontable2 <- read.csv("~/LastBat/Detectiontable2.csv")
rownames(Detectiontable2) <- Detectiontable2[,2]
Detectiontable2 <- Detectiontable2[-18,-c(1,2,12)]
Detectiontable2$species <- rownames(Detectiontable2)
dfm2 <- melt(Detectiontable2, id.vars="species")
dfm2$sign <- ifelse(dfm2$value > 0 , "+" , ifelse(dfm2$value < 0, "-", ""))
p <- ggplot(dfm2, aes(x=variable, y=species)) +  geom_tile(aes(fill=value), colour = "black") + scale_fill_gradient2(low = "blue", high = 'red') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+ geom_text(aes(label=sign))
p