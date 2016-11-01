library(ggmap)
library(raster)
PNF <- readRDS("PNF.rds")
rtp <- rasterToPolygons(PNF)
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join

rtpFort <- fortify(rtp, data = rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')
g<- get_map(location='California', zoom=6, maptype = "terrain",
                 source='google',color='color')

ggmap(g) + inset_raster(as.raster(PNF), xmin = PNF@extent[1], xmax = PNF@extent[2],
                        ymin = PNF@extent[3], ymax = PNF@extent[4])

delme <- gmap(x = projectExtent(PNF, crs = CRS("+init=epsg:3857")), type = "hybrid", 
              zoom = 5)
plot(delme)

map('state', fill = FALSE, xlim = c(-125, -100), xlab = "lon", ylab = "lat")
map.axes(cex.axis=0.8)
plot(PNF, add = TRUE, col = "red", legend = FALSE)

library(maps)
library(rgdal)
ALT <-raster("C:/Users/corcoranbarriosd/Desktop/ALT/USA1_alt.grd") 


ALT[ALT < 1800] <- NA
plot(ALT) 
crs(PNF) <- " +proj=longlat +ellps=WGS84" 


map('state', fill = FALSE, xlim = c(-125, -114), ylim = c(32.2, 42.5), xlab = "lon", ylab = "lat")
map.axes(cex.axis=1)
plot(PNF, add = TRUE, col = "grey", legend = FALSE)
map.scale(x=-124, y=34, ratio=FALSE, relwidth=0.3)
library(GISTools)  
north.arrow(xb=-116, yb=41, len=0.22, lab="N")  




# The input file geodatabase
fgdb = "C:/Users/corcoranbarriosd/Desktop/SNV_Raster.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list = ogrListLayers(fgdb)
print(fc_list)


