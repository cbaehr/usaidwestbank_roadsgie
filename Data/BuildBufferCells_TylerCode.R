rm(list=ls(all=TRUE))

setwd("/Users/tylerfrazier/Tresors/aiddata/west_bank")

# install.packages("sp", dependencies = TRUE)
# install.packages("rgdal", dependencies = TRUE)
# install.packages("raster", dependencies = TRUE)
# install.packages("rgeos", dependencies = TRUE)
# install.packages("maptools", dependencies = TRUE)
# install.packages("spatstat", dependencies = TRUE)
# install.packages("readxl", dependencies = TRUE)
# install.packages("stringdist", dependencies = TRUE)

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(spatstat)
library(readxl)
library(stringdist)

# Import Night Time Lights, Crop to Israel - West Bank and Reproject

#ntl13 <- raster("/Users/tylerfrazier/Tresors/large_file_storage/aiddata/night_time_lights/F182013.v4c_web.stable_lights.avg_vis.tif")
ntl13 <- raster("/Users/rbtrichler/Downloads/F182013.v4/F182013.v4c_web.stable_lights.avg_vis.tif")

#prj_orig <- proj4string(ntl13)

#prj <- "+proj=utm +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
prj <- "+proj=utm +zone=36R +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#orig
lights<- crop(ntl13, extent(34, 36, 31, 33))
#lights<- crop(ntl13, extent(34, 37, 31, 33))


lights <- projectRaster(lights, crs = prj)

# Import Roads and Spatial Transform

setwd("/Users/rbtrichler/Documents/AidData/Git Repos/usaidwestbank_roadsgie/Data")

roads <- readOGR(dsn="Shapefiles", layer="INPIIRoadsProjects_Line_modified", stringsAsFactors=FALSE, verbose=FALSE)

proj4string(roads)
#prj_orig <- proj4string(roads)

roads <- spTransform(roads, CRS = prj)
#roads <- spTransform(roads, CRS = prj_orig)

plot(lights)
plot(roads, add = TRUE)

# Join Roads Data from Table based on Road Name (using string distance)

#roads_raw <- as.data.frame(read_xlsx("/Users/tylerfrazier/Tresors/aiddata/west_bank/data/nsd_road_data.xlsx"))
roads_raw <- read.csv("/Users/rbtrichler/Documents/AidData/Git Repos/usaidwestbank_roadsgie/Data/INPIICSV_RoadsShapefile_Reconcile _comments_clean.csv")
#delete extra row (should only be 59 obs)
roads_raw1<-roads_raw[-60,]
roads_raw<- roads_raw1

#this should be the field "NAME_"
name <- roads_raw[,9]
#date <- as.character(roads_raw[,15])
#this should be the field "ACTUAL_COM"
date <- as.character(roads_raw[,16])

roads_data <- cbind.data.frame(name, date, stringsAsFactors = FALSE)

roads@data[,c(1:2, 4:7)] <- NULL
row.names(roads@data) <- 1:nrow(roads@data)
roads@data$index <- 1:nrow(roads@data)

d <- data.frame(index = 1, Name = 1, name = 1, date = 1)

for(i in 1:nrow(roads_data)){
  a <- roads_data[which.min(stringdist(roads@data$Name[i], roads_data$name)), 1:2]
  b <- roads@data[i,2:1]
  c <- cbind.data.frame(b,a)
  d <- rbind.data.frame(d,c)
}

joined_data <- d[-1,-2]

roads_merged <- merge(roads, joined_data, by = "index")

# Create buffer crop & mask lights

buffer <- gBuffer(roads_merged, byid=TRUE, id = roads_merged@data$index, width = 5000)

plot(roads_merged)
plot(buffer, add = TRUE)

win <- as(buffer,"owin")

rds <- as.psp(roads_merged)
plot(rds)
rds_d <- distmap(rds)
plot(rds_d)
rds_d_r <- raster(rds_d)
rds_d_r_m <- mask(rds_d_r, buffer)

plot(rds_d_r_m)



lights_crop <- crop(lights, buffer)
lights_mask <- mask(lights_crop, buffer)

x <- cellFromPolygon(lights_mask, buffer)

d <- data.frame()

for(i in 1:length(x)){
  a <- length(x[[i]])
  print(a)
  d <- rbind.data.frame(d,a)
}

names(d) <- "value"

e <- data.frame()

for(i in 1:length(x)){
  a <- as.data.frame(rep(i, d$value[i]))
  #print(a)
  e <- rbind.data.frame(e,a)
}

names(e) <- "buffer_id"

x <- unlist(x)

l <- cbind.data.frame(e, cell_id = x)

buffer@data[3] <- NULL

s <- merge(l, buffer@data, by.x = "buffer_id", by.y = "index")

s_wide <- reshape(s, idvar = "cell_id", timevar = "buffer_id", direction = "wide")


y <- rasterToPolygons(lights_mask)

cell_id <- cellFromPolygon(lights_mask, buffer)
cell_id <- unique(unlist(cell_id))

z <- spCbind(y,cell_id)

z@data <- merge(z@data, s_wide, by = "cell_id")

z@data[2] <- NULL


#lights_buffer <- extract(lights_mask, buffer, cellnumbers=TRUE, df=TRUE)
#lights_wide <- reshape(lights_buffer, idvar = "cell", timevar = "ID", direction = "wide")
#lights_buffer <- extract(lights, buffer, ID = buffer@data$index, cellnumbers=TRUE, df=TRUE)
#lights_wide <- reshape(lights_buffer, idvar = "cell", timevar = "ID", direction = "wide")
#save(lights_wide, lights_buffer, buffer, roads_merged, file = "west_bank.RData")

#setwd("/Users/tylerfrazier/Tresors/aiddata/west_bank/")
setwd("/Users/rbtrichler/Documents/AidData/Git Repos/usaidwestbank_roadsgie/Data")

lights <- projectRaster(lights, crs = prj_orig)

#z <- spTransform(z, CRS = prj_orig)
z <- spTransform(z, CRS = prj)

writeOGR(buffer, dsn = "shapefiles", layer = "buffer_TylerCode", driver="ESRI Shapefile")
writeOGR(roads_merged, dsn = "shapefiles", layer = "roads_merged_TylerCode", driver="ESRI Shapefile")

writeRaster(lights, "lights_newprj2", format = "GTiff")

writeRaster(lights_mask, "lights_masked", format = "GTiff")

writeOGR(z, dsn = "shapefiles", layer = "poly_raster_data_merge2_TylerCode", driver="ESRI Shapefile")


