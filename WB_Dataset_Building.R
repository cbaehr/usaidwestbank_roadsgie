
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


##-------
##Create list of road segment/buffer names, ids, and final completion dates
##-------

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

#-------
# Create grid cell level wide dataset
#-------

## Read in geo(query) cell-level extracts
#id_1, name_1, dist_1 (etc) columns identify 5 km buffer that each cells falls into, where the number at the end is the buffer id from buffers$road_id 
#will use buffer ids to create treatment vars
wb_cells<-read.csv("Data/road_grid_750_extracts.csv")

## Rename covariates for use in analytical models (less complicated names)
#break out max and mean ndvi separately in order to rename
max_ndvi<-wb_cells[c(1,343:402)]
colnames(max_ndvi)<-sub("ltdr_avhrr_monthly_ndvi.","maxl_",colnames(max_ndvi))
colnames(max_ndvi) <- gsub(".max", "", colnames(max_ndvi), fixed=TRUE)

mean_ndvi<-wb_cells[c(1,283:342)]
colnames(mean_ndvi)<-sub("ltdr_avhrr_monthly_ndvi.","meanl_",colnames(mean_ndvi))
colnames(mean_ndvi) <- gsub(".mean", "", colnames(mean_ndvi), fixed=TRUE)
#merge max and mean ndvi back in to wb_cells
ndvi<-merge(max_ndvi,mean_ndvi)
wb_cells<-wb_cells[,-grep("ltdr_avhrr_monthly_ndvi",colnames(wb_cells))]
wb_cells1<-merge(wb_cells,ndvi)
wb_cells<-wb_cells1

#rename yearly ndvi
colnames(wb_cells) <- sub("ltdr_avhrr_yearly_ndvi.","meanl_",colnames(wb_cells))
#remove ".mean" at end of covariates
colnames(wb_cells) <- gsub(".mean", "", colnames(wb_cells), fixed=TRUE)
colnames(wb_cells) <- gsub(".na", "", colnames(wb_cells), fixed=TRUE)
#rename
colnames(wb_cells) <- sub("dist_to_water","waterdist",colnames(wb_cells))
colnames(wb_cells) <- sub("dist_to_groads","roaddist",colnames(wb_cells))
colnames(wb_cells) <- sub("srtm_elevation_500m","elevation",colnames(wb_cells))
colnames(wb_cells) <- sub("srtm_slope_500m","slope",colnames(wb_cells))
colnames(wb_cells) <- sub("accessibility_map","urbtravtime",colnames(wb_cells))
colnames(wb_cells) <- sub("gpw_v[0-9]_density.","pop_",colnames(wb_cells))
#temp
colnames(wb_cells) <- sub("udel_air_temp_v4_01_yearly_mean.","MeanT_",colnames(wb_cells))
colnames(wb_cells) <- sub("udel_air_temp_v4_01_yearly_min.","MinT_",colnames(wb_cells))
colnames(wb_cells) <- sub("udel_air_temp_v4_01_yearly_max.","MaxT_",colnames(wb_cells))
#precip
colnames(wb_cells) <- sub("udel_precip_v4_01_yearly_mean.","MeanP_",colnames(wb_cells))
colnames(wb_cells) <- sub("udel_precip_v4_01_yearly_min.","MinP_",colnames(wb_cells))
colnames(wb_cells) <- sub("udel_precip_v4_01_yearly_max.","MaxP_",colnames(wb_cells))
#ntl
colnames(wb_cells) <- sub("ntl_yearly.","",colnames(wb_cells))
colnames(wb_cells) <- sub("ntl_monthly.","",colnames(wb_cells))
#dmsp ntl
colnames(wb_cells) <- sub("v4composites_calibrated_201709.","dmsp_",colnames(wb_cells))

#-------
# Create Treatment Variables
#-------
#changes "name" columns in wb_cells to "road" columns
colnames(wb_cells) <- gsub("name_", "road_", colnames(wb_cells))









