
#rebuild West_Bank_Dataset_Building with new Seth extracts (rather than materials from Tyler)

#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
#setwd("C:/Users/jflak/OneDrive/Github/usaidwestbank_roadsgie/Data")
setwd("/Users/rbtrichler/Documents/AidData/Git Repos/usaidwestbank_roadsgie")

#needed packages
library(sf)
library(readxl)
library(stringdist)
library(plyr)
library(devtools)
library(maptools)
devtools::install_github("shuyang1987/multilevelMatching")
library(multilevelMatching)
# devtools::install_github("itpir/SCI@master")
# library(SCI)

#input cell-level extracts file
cells<-read.csv("Data/road_grid_750_extracts.csv")

##--
##Create list of road segment/buffer names, ids, and final completion dates
##--

## get road/buffer ids from buffer shapefile used to create our sample of cells
#input buffers geojson and convert to dataframe
#buffers<-rgdal::readOGR("Data/data_geojsons/road_buffer_5000_clip.geojson","OGRGeoJSON")
buffers<-"Data/data_geojsons/road_buffer_5000_clip.geojson"
buffers<-st_read(buffers)
#rename OBJECTID_1 (correct id for road segments) to road_id and remove other id fields
#OBJECTID_1 includes 1-52, 55-61 (skips 53 and 54 for some reason, but should be 59 road segments total)
names(buffers)[names(buffers)=='OBJECTID_1']<-'road_id'
buffers<-buffers[c(2,4:9)]
#extract geometry and convert to dataframe
buffers_geo<-st_geometry(buffers)
st_geometry(buffers)<-NULL
buffers_geo <- st_set_geometry(as.data.frame(buffers$road_id),buffers_geo)
#create unique list of names from buffers
road_names_list <- unique(c(levels(buffers$Name)))

##merge road/buffer ids with INPII csv (from Mission; includes completion dates)
#input INPII csv and eliminate blank extra row so 59 obs total
inpii_data <- read.csv("Data/INPIICSV_RoadsShapefile_Reconcile _comments_clean.csv",na.strings=c("","NA"))
inpii_data<-inpii_data[!is.na(inpii_data$Name_INPIIRoadsProject_Line),]

#creates a list of the indices of the (fuzzy) matching names in INP II and road_names_list
matched_list <- amatch(inpii_data[["Name_INPIIRoadsProject_Line"]], road_names_list, maxDist = 50)

#creates new colum in INP II data to exactly match the names in buffer data in order to merge
inpii_data$road_name <- as.character(road_names_list[matched_list])

#merge buffer data and INPII data together by road_name
inpii_data1<-merge(x=inpii_data,y=buffers,by.x="road_name",by.y="Name",type="inner")
inpii_data<-inpii_data1
#subset to list of road/buffers, ids, completion dates and export to GitRepo????


