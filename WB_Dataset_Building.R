
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
#fixes ACTUAL_COM column in inpii_data to be formatted as date (for later use)
inpii_data[["ACTUAL_COM"]][inpii_data[["ACTUAL_COM"]] == 9999] <- NA
inpii_data$ACTUAL_COM <- as.Date(inpii_data$ACTUAL_COM, format = "%m/%d/%y")
inpii_data[["ACTUAL_COM"]][is.na(inpii_data[["ACTUAL_COM"]])] <- "9999-01-01"

#subset to list of road/buffers, ids, completion dates and export to GitRepo????

#-------
# Create grid cell level wide dataset
#-------

## Read in geo(query) cell-level extracts
#id_1, name_1, dist_1 (etc) columns identify 5 km buffer that each cells falls into, where the number at the end is the buffer id from buffers$road_id 
#will use buffer ids to create treatment vars
wb_cells<-read.csv("Data/road_grid_750_extracts.csv",na.strings=c("","NA"))

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

# create new dataset "x" (name makes it easier to use old code) that includes only cell_id, and road id/name/dist columns
id<-paste(colnames(wb_cells)[grep("id",colnames(wb_cells))])
dist<-paste(colnames(wb_cells)[grep("*dist_",colnames(wb_cells))])
road<-paste(colnames(wb_cells)[grep("*road_",colnames(wb_cells))])

trtvars <- c(id,road,dist)
x <- wb_cells[trtvars]

#creates separate road_name and distance dataframes, deletes the other respective variable from the dataframes
x_road_name <- x[, -grep("id_", colnames(x))]
x_road_name <- x_road_name[, -grep("dist", colnames(x_road_name))]

x_dist <- x[,-grep("id_",colnames(x))]
x_dist <- x_dist[,-grep("road",colnames(x_dist))]

x_id <- x[,-grep("dist_",colnames(x))]
x_id <- x_id[,-grep("road",colnames(x_id))]

#shifts the data left in each dataframe so that the rightmost columns are all NAs, and renames the colnames to the original names
x_road_name_left = as.data.frame(t(apply(x_road_name, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(x_road_name_left) <- colnames(x_road_name)

x_dist_left = as.data.frame(t(apply(x_dist, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(x_dist_left) <- colnames(x_dist)

x_id_left = as.data.frame(t(apply(x_id, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(x_id_left) <- colnames(x_id)

# #Double check data shift
# #should be no NAs in column 1
# sum(is.na(x_dist_left$dist_1))
# sum(is.na(x_road_name_left$road_1))
# sum(is.na(x_id_left$id_1))
# #numbers of NA should match in column 2
# sum(is.na(x_dist_left$dist_2))
# sum(is.na(x_road_name_left$road_2))
# sum(is.na(x_id_left$id_2))
# #should have non-NA values in column 9 but not in column 10
# sum(is.na(x_dist_left$dist_9))
# sum(is.na(x_road_name_left$road_9))
# sum(is.na(x_id_left$id_9))
# sum(is.na(x_dist_left$dist_10))
# sum(is.na(x_road_name_left$road_10))
# sum(is.na(x_id_left$id_10))

#Merges the three left-shifted dataframes and orders the columns at the same time - also deletes all columns 10 and greater (they're all NAs)
#Must sort components first to be sure rows are in the same order
x_id_left<-x_id_left[order(x_id_left$cell_id),]
x_dist_left<-x_dist_left[order(x_dist_left$cell_id),]
x_road_name_left<-x_road_name_left[order(x_road_name_left$cell_id),]

x_left <- cbind.data.frame(x_id_left$cell_id, x_id_left$id_1, x_road_name_left$road_1,x_dist_left$dist_1,
                           x_id_left$id_2, x_road_name_left$road_2, x_dist_left$dist_2,
                           x_id_left$id_3, x_road_name_left$road_3, x_dist_left$dist_3,
                           x_id_left$id_4, x_road_name_left$road_4, x_dist_left$dist_4,
                           x_id_left$id_5, x_road_name_left$road_5, x_dist_left$dist_5,
                           x_id_left$id_6, x_road_name_left$road_6, x_dist_left$dist_6,
                           x_id_left$id_7, x_road_name_left$road_7, x_dist_left$dist_7,
                           x_id_left$id_8, x_road_name_left$road_8, x_dist_left$dist_8,
                           x_id_left$id_9, x_road_name_left$road_9, x_dist_left$dist_9)
#Check column bind by manually looking at a few random cell ids

#renames the colnames to the original names
colnames(x_left) <- c("cell_id", "id_1", "road_1","dist_1",
                      "id_2", "road_2","dist_2",
                      "id_3", "road_3","dist_3",
                      "id_4", "road_4","dist_4",
                      "id_5", "road_5","dist_5",
                      "id_6", "road_6","dist_6",
                      "id_7", "road_7","dist_7",
                      "id_8", "road_8","dist_8",
                      "id_9","road_9","dist_9")
x_merged<-x_left

#merge in completion date for each set of road columns 1-9 to reflect date that road improvements completed for that road segment
#subset inpii_data to road_id and actual completion date, to be used as treatment date
#date is in YYYY-MM-DD (formatted earlier in script)
date<-inpii_data[,c("road_id","ACTUAL_COM")]

#merge in completion date based on id_1 for first set of road columns, set in date format

for (i in 1:9)
{
x_merged<-merge(x_merged, date,by.x=(paste0("id_",i)),by.y="road_id",all.x=TRUE)
names(x_merged)[names(x_merged)=='ACTUAL_COM']<-paste0("date_",i)
x_merged[[paste0("date_", i)]] <- as.Date(x_merged[[paste0("date_", i)]])
}

#create new column that concatenates date and column number for date
#column number will be used later for order of treatment dates
for (i in 1:9)
{
  x_merged[[paste0("date_colnum",i)]]<-paste(x_merged[[paste0("date_",i)]],i,sep=";")
}  

#creates treatment date column for every buffer that a cell falls into, from 1 buffer to 9 buffers
#NA if there is no 2nd, 3rd, 4th, etc. buffer
#date_ columns are not necessarily in descending order
#code uses specific position of date + column number fields in the dataframe, so update code if positions changes
x_merged$datenum_trt1 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[1])
x_merged$datenum_trt2 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[2])
x_merged$datenum_trt3 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[3])
x_merged$datenum_trt4 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[4])
x_merged$datenum_trt5 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[5])
x_merged$datenum_trt6 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[6])
x_merged$datenum_trt7 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[7])
x_merged$datenum_trt8 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[8])
x_merged$datenum_trt9 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[9])

#separates the date+column number fields, now in descending order, in to separate date field and column field
#can use column number to join with road name and distance
x_merged<-x_merged %>% separate(datenum_trt1,c("date_trt","col_trt"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt2,c("date_trt2","col_trt2"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt3,c("date_trt3","col_trt3"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt4,c("date_trt4","col_trt4"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt5,c("date_trt5","col_trt5"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt6,c("date_trt6","col_trt6"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt7,c("date_trt7","col_trt7"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt8,c("date_trt8","col_trt8"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt9,c("date_trt9","col_trt9"),";",remove=FALSE)

###NEED TO REPLACE NAs as actual NA, not the character "NA", and then delete the datenum combo columns 


##SCRATCH BEGINS
#x_merged$trt_col<-strsplit(x_merged$date_trt,";")
#x_merged %>% separate(date_trt,c("trt_date","trt_col"),";")

# x_merged %>% separate(date_trt,c("trt_date","trt_col"),";")
# before %>%
#   separate(type, c("foo", "bar"), "_and_")
# 
# x_merged$test<-sapply(colnames(x_merged),1,function(name){paste0(name)},simplify=F)
# x_merged$test<-apply(x_merged[c(29:37)],1,function(x) {paste(names(x_merged))},rev(sort(x,decreasing=TRUE))[1])
# 
# x_merged$test<-apply(x_merged[c(29:37)],1,function(x) {paste0(rank(x),sep=",")})
# 
# a<-x_merged[29:37]
# x_merged$test<-paste0(rank(a),sep=",")
# 
# 
# x_merged$trt_col<-
# df2 <- as.data.frame(sapply(colnames(df),
#                             function(name){ paste(name,df[,name],sep=", ")},
#                             simplify=F)

## NEED TO FIGURE OUT; currently pastes number from final column
# #loop-ified code to create a variable "trt_col" which contains the number of the treatment columns (1-9)
# #is usually 1, but date columns are not always in chronological order
# #for(i in 1:9)
# #{
#   x_merged[["trt_col"]][x_merged[["date_trt"]] == x_merged[[paste0("date_", i)]]] <- i
#   x_merged[["trt2_col"]][x_merged[["date_trt2"]] == x_merged[[paste0("date_", i)]]] <- i
#   x_merged[["trt3_col"]][x_merged[["date_trt3"]] == x_merged[[paste0("date_", i)]]] <- i
#   x_merged[["trt4_col"]][x_merged[["date_trt4"]] == x_merged[[paste0("date_", i)]]] <- i
#   x_merged[["trt5_col"]][x_merged[["date_trt5"]] == x_merged[[paste0("date_", i)]]] <- i
#   x_merged[["trt6_col"]][x_merged[["date_trt6"]] == x_merged[[paste0("date_", i)]]] <- i
#   x_merged[["trt7_col"]][x_merged[["date_trt7"]] == x_merged[[paste0("date_", i)]]] <- i
#   x_merged[["trt8_col"]][x_merged[["date_trt8"]] == x_merged[[paste0("date_", i)]]] <- i
#   x_merged[["trt9_col"]][x_merged[["date_trt9"]] == x_merged[[paste0("date_", i)]]] <- i
#   
# #}

### SCRATCH ENDS


#loop-ified code to change the road name columns from factor to character format
for(i in 1:9)
{
  x_merged[[paste0("road_", i)]] <- as.character(x_merged[[paste0("road_", i)]])
}








