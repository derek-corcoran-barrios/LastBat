library(foreign)
fire <- read.dbf("C:/Users/corcoranbarriosd/Desktop/fh_all_1980_2015.dbf")
library(dplyr)

fireCA <- filter(fire, STATE == "California" | STATE == "Oregon")

library(lubridate)

fireCA$STARTDATED <- year(ymd(fireCA$STARTDATED))


fireCA <- data.frame(Year = fireCA$STARTDATED, Area = fireCA$TOTALACRES)

fireCA <- group_by(fireCA, Year)

fireCA <- summarise(fireCA, Total = sum(Area))

library(ggplot2)

fireCA <- fireCA[!is.na(fireCA$Year),]

p<- ggplot(fireCA, aes(x = Year, y = Total)) + geom_point()+ stat_smooth(method = "lm", formula = y ~  I(log(x)), size = 1)+scale_y_log10(breaks = c(10000,100000, 1000000), labels = c("10,000","100,000","1,000,000")) + ylab("Acres") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(), 
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

View(fireCA)

lm(Total~Year, fireCA)

exp.model <-lm(Total ~ log(Year), fireCA)



require(XML)
data <- xmlParse("C:/Users/corcoranbarriosd/Desktop/original.xml")
xml_data <- xmlToList(data)
xmlToDataFrame(nodes=getNodeSet(data1,"//data"))

install.packages("forecast")
library(forecast)
trend_beer = ma(fireCA$Total, order = 6, centre = T)
plot(as.ts(fireCA$Total))
lines(trend_beer)

str(trend_beer)

df<- data.frame(year = fireCA$Year, trend = as.numeric(trend_beer))
ggplot(df, aes(x = year, y =  trend)) + geom_line() + geom_point(data = fireCA,aes(x = Year, y =Total)) + geom_smooth(method = 'lm')+ theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = theme_blank(), 
  panel.grid.major = theme_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)

png('tr_tst2.png',width=800,height=500,units="px",bg = "transparent")
print(p)
dev.off()



data(mtcars)

p<- ggplot(mtcars, aes(x= as.factor(gear), y = mpg)) + geom_violin(aes(fill = as.factor(gear))) + geom_jitter() + ylim(c(0,40))+ xlab("gears") +scale_fill_manual(values=c('aquamarine3','chartreuse1','goldenrod1'),name = "Gears") + theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(), 
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

png('tr_tst2.png',width=300,height=300,units="px",bg = "transparent")
print(p)
dev.off()
#########################################map#####################################

fire <- readGDAL("C:/Users/corcoranbarriosd/Desktop/MI_rcp85/MI_rcp85_2041_2060.tif")

fire<- raster(fire)
PNF <- readRDS("PNF.rds")
crs(PNF) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
fire <- projectRaster(fire, proj4string(PNF))
library(maps)
library(maptools)
fire <- crop(fire, e)
fire2 <- fire
fire2[fire2 < 98.5] <- NA
plot(fire2)
map('state', fill = FALSE, xlim = c(-125, -114), ylim = c(32.2, 42.5), xlab = "lon", ylab = "lat", add = TRUE, lwd=2)
plot(PNF, add = TRUE, legend = FALSE, col = "red")


fire3 <- fire
fire3[fire3 < 99] <- NA
plot(fire3, col = rev(heat.colors(100)),legend.args=list(text='Fire vulnerability', side=4, font=2, line=2.5, cex=0.8))
map('state', fill = FALSE, xlim = c(-125, -114), ylim = c(32.2, 42.5), xlab = "lon", ylab = "lat", add = TRUE, lwd=2)
plot(PNF, add = TRUE, legend = FALSE, col = "darkgreen")



#####################################################################

df<- data.frame(NULL)
for(i in 1:17){
  df[i,1] <- cellStats(firebats[[i]], "sd")
  df[i,2] <- cellStats(firebats[[i]], "max")
  df[i,3] <- cellStats(firebats[[i]], "min")
}

colnames(df) <- c("sd", "max", "min")
df$names<-names(firebats)
df$order <- c(1:17)
df$diff <- (df$max - df$min)
df<- arrange(df, desc(sd))

firebats2<- subset(firebats, df$order)


library(viridis)
levelplot(firebats2, col.regions = magma(99), colorkey = list(space = "bottom"))
