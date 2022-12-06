#load libraries

library(rgdal); library(raster)

#list work directories

wd_israel <- 'G:/My Drive/PHD/chapter 2  functional diversity/Eilat_FD/ISR_adm'


#load israeli map

israel <- readOGR('ISR_adm1', dsn = wd_israel)

#crop south

isr <- crop(israel, extent(34.26801, 35.90094, 29.49708, 29.57))

plot(isr, col = 'khaki1', bg = 'lightblue')

#load species richness

setwd(wd_sps_rich)

species_richness <- readRDS('')

species_richness

#create spatialPointDataFrame

sps_rich_sp <- species_richness

sps_rich_sp <- sps_rich_sp[-which(is.na(sps_rich_sp$lon)),]

coordinates(sps_rich_sp) <- ~lon+lat

#plot points

plot(isr)
plot(sps_rich_sp, col = "magenta", cex = 0.5, pch = 19, add=T)


###################

#create empty raster

r_eilat <- raster(xmn = 34.86, xmx = 34.98, ymn = 29.48, ymx = 29.57, #extent
                  res = 0.0004, val = 0)


#create ID raster

ID_raster <- r_eilat
ID_raster[] <- c(1:length(ID_raster))

#create land raster
land_raster <- mask(ID_raster, isr) #crop ID raster by shp 
land_raster[!is.na(land_raster[])] <- 9999
land_raster[is.na(land_raster[])] <- 0
land_raster[which(land_raster[] == 9999)] <- NA

#identify observation site cells
pts_IDs <- extract(ID_raster, sps_rich_sp)
rich_ID <- as.data.frame(cbind(sps_rich_sp@data, raster_ID = pts_IDs))

#group

rich_ID<- rich_ID %>% group_by(raster_ID) %>% summarise("mean_rich" = mean(species_richness))

rich_ID2 <- as.data.frame(rich_ID)

#include values in raster

land_raster[rich_ID$raster_ID] <- rich_ID$mean_rich

plot(land_raster)


