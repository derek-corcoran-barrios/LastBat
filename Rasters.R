library(raster)

##Fire
#1
Anpa_FireLayer <- readRDS("Anpa_FireLayer.rds")
crs(Anpa_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Anpa_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Anpa_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#2
Coto_FireLayer <- readRDS("Coto_FireLayer.rds")
crs(Coto_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Coto_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Coto_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#3
Epfu_FireLayer <- readRDS("Epfu_FireLayer.rds")
crs(Epfu_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Epfu_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Epfu_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#4
Euma_FireLayer <- readRDS("Euma_FireLayer.rds")
crs(Euma_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Euma_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Euma_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#5
Eupe_FireLayer <- readRDS("Eupe_FireLayer.rds")
crs(Eupe_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Eupe_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Eupe_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#6
Labl_FireLayer <- readRDS("Labl_FireLayer.rds")
crs(Labl_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Labl_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Labl_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#7
Laci_FireLayer <- readRDS("Laci_FireLayer.rds")
crs(Laci_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Laci_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Laci_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#8
Lano_FireLayer <- readRDS("Lano_FireLayer.rds")
crs(Lano_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Lano_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Lano_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#9
Myca_FireLayer <- readRDS("Myca_FireLayer.rds")
crs(Myca_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Myca_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Myca_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#10
Myci_FireLayer <- readRDS("Myci_FireLayer.rds")
crs(Myci_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Myci_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Myci_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#11
Myev_FireLayer <- readRDS("Myev_FireLayer.rds")
crs(Myev_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Myev_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Myev_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#12
MyLu_FireLayer <- readRDS("MyLu_FireLayer.rds")
crs(MyLu_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(MyLu_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/MyLu_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#13
Myth2_FireLayer <- readRDS("Myth2_FireLayer.rds")
crs(Myth2_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Myth2_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Myth_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#14
MyVo_FireLayer <- readRDS("MyVo_FireLayer.rds")
crs(MyVo_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(MyVo_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/MyVo_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#15
Myyu_Firelayer <- readRDS("Myyu_Firelayer.rds")
crs(Myyu_Firelayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Myyu_Firelayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Myyu_Firelayer.asc", overwrite=TRUE, prj = TRUE)
#16
Pahe_FireLayer <- readRDS("Pahe_FireLayer.rds")
crs(Pahe_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Pahe_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Pahe_FireLayer.asc", overwrite=TRUE, prj = TRUE)
#17
Tabr_FireLayer <- readRDS("Tabr_FireLayer.rds")
crs(Tabr_FireLayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Tabr_FireLayer, "C:/Users/corcoranbarriosd/Desktop/Layers/fire/Tabr_FireLayer.asc", overwrite=TRUE, prj = TRUE)

############################Full

#1
Anpa_fulllayer <- readRDS("Anpa_fulllayer.rds")
crs(Anpa_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Anpa_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Anpa_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#2
Coto_fulllayer <- readRDS("Coto_fulllayer.rds")
crs(Coto_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Coto_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Coto_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#3
Epfu_fulllayer <- readRDS("Epfu_fulllayer.rds")
crs(Epfu_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Epfu_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Epfu_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#4
Euma_fulllayer <- readRDS("Euma_fulllayer.rds")
crs(Euma_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Euma_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Euma_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#5
Eupe_fulllayer <- readRDS("Eupe_fulllayer.rds")
crs(Eupe_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Eupe_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Eupe_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#6
Labl_fulllayer <- readRDS("Labl_fulllayer.rds")
crs(Labl_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Labl_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Labl_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#7
Laci_fulllayer <- readRDS("Laci_fulllayer.rds")
crs(Laci_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Laci_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Laci_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#8
Lano_fulllayer <- readRDS("Lano_fulllayer.rds")
crs(Lano_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Lano_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Lano_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#9
Myca_fulllayer <- readRDS("Myca_fulllayer.rds")
crs(Myca_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Myca_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Myca_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#10
Myci_fulllayer <- readRDS("Myci_fulllayer.rds")
crs(Myci_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Myci_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Myci_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#11
Myev_fulllayer <- readRDS("Myev_fulllayer.rds")
crs(Myev_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Myev_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Myev_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#12
MyLu_fulllayer <- readRDS("MyLu_fulllayer.rds")
crs(MyLu_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(MyLu_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/MyLu_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#13
Myth2_fulllayer <- readRDS("Myth2_fulllayer.rds")
crs(Myth2_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Myth2_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Myth2_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#14
MyVo_fulllayer <- readRDS("MyVo_fulllayer.rds")
crs(MyVo_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(MyVo_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/MyVo_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#15
Myyu_fulllayer <- readRDS("Myyu_fulllayer.rds")
crs(Myyu_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Myyu_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Myyu_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#16
Pahe_fulllayer <- readRDS("Pahe_fulllayer.rds")
crs(Pahe_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Pahe_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Pahe_fulllayer.asc", overwrite=TRUE, prj = TRUE)
#17
Tabr_fulllayer <- readRDS("Tabr_fulllayer.rds")
crs(Tabr_fulllayer) <- "+proj=longlat +datum=WGS84 +towgs84=0,0,0"
writeRaster(Tabr_fulllayer, "C:/Users/corcoranbarriosd/Desktop/Layers/full/Tabr_fulllayer.asc", overwrite=TRUE, prj = TRUE)