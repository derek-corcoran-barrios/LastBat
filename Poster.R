library(ggmap)
plumas <- get_map(location = "Quincy, ca", zoom = 9)


library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(reshape2)
library(ggthemr)
library(lubridate)


#HeatMap detection

occupancytable2 <- read.csv("~/LastBat/occupancytable2.csv")
rownames(occupancytable2) <- occupancytable2[,2]

occupancytable2 <- occupancytable2[-18,-c(1,2,9)]
colnames(occupancytable2)<- c("Burn Basal", "Burn Basal^2", "Burn Canopy", "Burn Canopy^2", "Fire Distance", "Forest Distance")
occupancytable2$species <- rownames(occupancytable2)
dfm <- melt(occupancytable2, id.vars="species")
dfm$sign <- ifelse(dfm$value > 0 , "+" , ifelse(dfm$value < 0, "-", ""))


p <- ggplot(dfm, aes(x=variable, y=species)) +  geom_tile(aes(fill=value), colour = "black") + scale_fill_gradient2(low = "blue", high = 'red', name="Strength") + theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))+ geom_text(aes(label=sign), color = 'black')

###Barplot

summarised_df1 <- readRDS("summarised_df1.rds")

ggthemr("chalk", type="outer", layout="clear", spacing=1)

mean_fire <- summarised_df1$mean[rep(seq(from = 2, to = 34, by = 2), each = 2)]
limits <- aes(ymax = mean + std_error, ymin=mean-std_error)
g<-ggplot(summarised_df1,aes(y = mean, x = reorder(variable, -mean_fire), fill = FIRE))
g<-g+geom_bar(stat = "identity",position = position_dodge(), size = 1.5)+ scale_fill_manual(values=c('#999999','#E69F00'))
g<-g+geom_errorbar(limits,width=0.25,position = position_dodge(0.9)) + ylab("Occupancy") + xlab("Species")



#Sampling map

datasp <-  read.csv("~/LastBat/Grandtotal.csv")
datasp$Start.1 <- ymd(as.character(datasp$Start.1))
datasp$YEAR <- as.factor(as.character(year(datasp$Start.1)))

Plumap <- ggmap(plumas) + scale_x_continuous(limits = c(-121.6945, -120.0407), expand = c(0, 0)) + scale_y_continuous(limits = c(39.36708, 40.31977)) + geom_point(aes(x = LONG, y = LAT, colour = YEAR), data = datasp, size = 3, alpha = 0.5) + scale_color_manual(values = c("yellow", "red"))




#TEXTS

Title <- textGrob(paste("Burn, Baby, Burn: Effects of Wildfire on Bat Species Occupancy", "Probability in the Sierra Nevada Mountains", sep ="\n"),gp=gpar(fontsize=20))

Background <- textGrob("Background")

intro <- textGrob(paste("Increasing wildfire frequency and severity in the Western United States", "emphasizes the importance of understanding how fire-mediated habitat changes affect", "biodiversity. In particular, it is unknown how bat habitat use is affected by fire intensity", "and its spatial structure.", 
"We measured fire intensity as differenced normalized burn ratios (DNBR)", "and the distance from and to the limit of the burned area within the forest matrix, and evaluated how this factors affected", "the occupancy of 17 bat species in northern California.", sep = "\n"))
#ARRANGE

lay <- rbind(c(1,1,1,1,1),
             c(5,5,NA,4,4),
             c(6,6,NA,4,4),
             c(6,6,NA,4,4),
             c(6,6,NA,NA,NA),
             c(2,2,3,3,3),
             c(2,2,3,3,3))

main=textGrob(paste("titleLine1", "titleLine2", sep = "\n"),gp=gpar(fontsize=20))
##############Posterize

grid.arrange(Title, Plumap, g, p, Background, intro, layout_matrix = lay)
