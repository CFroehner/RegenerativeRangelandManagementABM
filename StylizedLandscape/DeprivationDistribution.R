Dep <- raster::raster("StylizedLandscape/povmap-grdi-v1.tif")
AOI<-read_sf("StylizedLandscape/AOI.shp")
Dep<-raster::crop(Dep,AOI)
Dep<-raster::as.data.frame(Dep,xy=T)
Dep<-Dep[complete.cases(Dep), ] 
names(Dep)[3]<-"Deprivaton"