
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

#renames the colnames to the original names, using letters instead of numbers to group
colnames(x_left) <- c("cell_id", "id_a", "road_a","dist_a",
                      "id_b", "road_b","dist_b",
                      "id_c", "road_c","dist_c",
                      "id_d", "road_d","dist_d",
                      "id_e", "road_e","dist_e",
                      "id_f", "road_f","dist_f",
                      "id_g", "road_g","dist_g",
                      "id_h", "road_h","dist_h",
                      "id_j","road_j","dist_j")
x_merged<-x_left

##merge in completion date for each set of road columns 1-9 to reflect date that road improvements completed for that road segment
#subset inpii_data to road_id and actual completion date, to be used as treatment date
#date is in YYYY-MM-DD (formatted earlier in script)
date<-inpii_data[,c("road_id","ACTUAL_COM")]

#merge in completion date based on id_1 for first set of road columns, set in date format
buffergrp<-c("a","b","c","d","e","f","g","h","j")
for (i in buffergrp)
{
x_merged<-merge(x_merged, date,by.x=(paste0("id_",i)),by.y="road_id",all.x=TRUE)
names(x_merged)[names(x_merged)=='ACTUAL_COM']<-paste0("date_",i)
x_merged[[paste0("date_", i)]] <- as.Date(x_merged[[paste0("date_", i)]])
}

#create new column that concatenates date;column number for the associated date
#column number will be used later for order of treatment dates
#since a-i columns are not necessarily in date order, the earliest treatment date could be in col b,c, etc instead of col. a
for (i in buffergrp)
{
  x_merged[[paste0("date_colnum",i)]]<-paste(x_merged[[paste0("date_",i)]],i,sep=";")
}  

#creates treatment date column for every buffer that a cell falls into, from buffer a to j (9 buffers is the max for one cell)
#date_[a-j] columns are not necessarily in chronological order, so this identifies the earliest date from up to 9 date columns for each cell
# includes the original a-j column letter as part of the value in order to identify which buffer group the date belongs to (will separate and use later)
#code uses specific position of date + column letter fields in the dataframe, so update code if positions changes
# 1 is earliest treatment date, 9 is latest treatment date
x_merged$datenum_trt1 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[1])
x_merged$datenum_trt2 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[2])
x_merged$datenum_trt3 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[3])
x_merged$datenum_trt4 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[4])
x_merged$datenum_trt5 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[5])
x_merged$datenum_trt6 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[6])
x_merged$datenum_trt7 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[7])
x_merged$datenum_trt8 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[8])
x_merged$datenum_trt9 <- apply(x_merged[c(38:46)],1,function(x) rev(sort(x,decreasing=TRUE))[9])

#separates the date+column letter fields, now in descending order, in to separate date field and column field
#can use column letter to join with road name and distance information
x_merged<-x_merged %>% separate(datenum_trt1,c("date_trt1","col_trt1"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt2,c("date_trt2","col_trt2"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt3,c("date_trt3","col_trt3"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt4,c("date_trt4","col_trt4"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt5,c("date_trt5","col_trt5"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt6,c("date_trt6","col_trt6"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt7,c("date_trt7","col_trt7"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt8,c("date_trt8","col_trt8"),";",remove=FALSE)
x_merged<-x_merged %>% separate(datenum_trt9,c("date_trt9","col_trt9"),";",remove=FALSE)

#x_merged has series of columns with _[1-9] that identify the road buffers that a single cell is part of (9 is max)
# x_merged$date_[a-j] date that the road in each buffer a-j improvements were completed 
# x_merged$date_colnum concatenates completion date and a-j buffer column letter that the completion date is associated with in order to put in descending order
# x_merged$datenum_trt[1-9] series of columns that puts the date of improvements in chronological order, earliest to latest, with associated a-j buffer column letter attached at end
# x_merged$date_trt[1-9] and col_trt[1-9] un-concatenate datenum_trt to give one column with the dates in earliest(1) to latest(9) order and another column with the a-j buffer column that it originally came from
# can use col_trt to join in the buffer name, id, and distance information with the chronological order treatment date information (to determine distance to treated buffer for each separate treatment date)
# some of these columns will not be used in final dataset and will be deleted later

#replace "NA" in date_trt with NA values rather than string "NA"
for (i in 1:9)
{
  x_merged[[paste0("date_trt",i)]][x_merged[[paste0("date_trt",i)]]=="NA"]<- NA
}
#delete unused columns, "date_colnum" and "datenum_trt"
x_merged1<-x_merged
x_merged1<-x_merged1[,-grep("date_colnum",colnames(x_merged1))]
x_merged1<-x_merged1[,-grep("datenum_trt",colnames(x_merged1))]
x_merged<-x_merged1

#fix formatting of date columns to be recognizable as dates and split into month, day, year
#loop-ified code to create the treatment road distances

for(i in buffergrp)
{
  x_merged[["dist_trt1"]][x_merged[["col_trt1"]]==i]<-x_merged[[paste0("dist_",i)]][x_merged[["col_trt1"]]==i]
  x_merged[["dist_trt2"]][x_merged[["col_trt2"]]==i]<-x_merged[[paste0("dist_",i)]][x_merged[["col_trt2"]]==i]
  x_merged[["dist_trt3"]][x_merged[["col_trt3"]]==i]<-x_merged[[paste0("dist_",i)]][x_merged[["col_trt3"]]==i]
  x_merged[["dist_trt4"]][x_merged[["col_trt4"]]==i]<-x_merged[[paste0("dist_",i)]][x_merged[["col_trt4"]]==i]
  x_merged[["dist_trt5"]][x_merged[["col_trt5"]]==i]<-x_merged[[paste0("dist_",i)]][x_merged[["col_trt5"]]==i]
  x_merged[["dist_trt6"]][x_merged[["col_trt6"]]==i]<-x_merged[[paste0("dist_",i)]][x_merged[["col_trt6"]]==i]
  x_merged[["dist_trt7"]][x_merged[["col_trt7"]]==i]<-x_merged[[paste0("dist_",i)]][x_merged[["col_trt7"]]==i]
  x_merged[["dist_trt8"]][x_merged[["col_trt8"]]==i]<-x_merged[[paste0("dist_",i)]][x_merged[["col_trt8"]]==i]
  x_merged[["dist_trt9"]][x_merged[["col_trt9"]]==i]<-x_merged[[paste0("dist_",i)]][x_merged[["col_trt9"]]==i]
}

#loop-ified code to put the treatment date columns in standard date format for easier manipulation
temp_col_list <- c("trt1", "trt2", "trt3", "trt4", "trt5", "trt6", "trt7", "trt8","trt9")
for(i in temp_col_list)
{
  x_merged[[paste0("date_", i)]] <- as.Date(x_merged[[paste0("date_", i)]])
  #x_merged[[paste0("date_", i, "_d")]] = as.numeric(format(x_merged[[paste0("date_", i)]], format = "%d"))
  x_merged[[paste0("date_", i, "_m")]] = as.numeric(format(x_merged[[paste0("date_", i)]], format = "%m"))
  x_merged[[paste0("date_", i, "_y")]] = as.numeric(format(x_merged[[paste0("date_", i)]], format = "%Y"))
  x_merged[[paste0("date_", i, "_ym")]] = as.numeric(format(x_merged[[paste0("date_", i)]], format = "%Y%m"))
}

#-------
# Prepare Wide Dataset for Panel Conversion
#-------

#drop buffer id, name, and distance columns from wb_cells
#information replaced by "trt" columns in x_merged
wb_cells2<-wb_cells[,-grep("id_",colnames(wb_cells))]
wb_cells2<-wb_cells2[,-grep("road_",colnames(wb_cells2))]
wb_cells2<-wb_cells2[,-grep("dist_",colnames(wb_cells2))]
wb_cells<-wb_cells2

#merge x_merged back in with wb_cells
wb_cells<-merge(wb_cells,x_merged)

#add municipality info for cell
# read in muni data and drop extra columns
muni_shp <- st_read("Data/cells_localities_join.shp")
muni<-as.data.frame(muni_shp)
#drop out "id" field created by QGIS and geometry
muni <- muni[,-(1)]
muni <- muni[,-grep("geometry",names(muni))]
#merge
wb_cells3 <- join(x=wb_cells, y=muni, by="cell_id", type="left")

wb_cells<-wb_cells3


#------------
# Create Panel Dataset
#------------

#monthly viirs starts April 2012
#ndvi monthly
#pop every 5 years - can't use since it takes ntl into account
#temp and precip end in 2014 - can't use those either

#Drop out unneeded variables
#For vars that change over time, only using monthly vars starting April 2012 through Dec 2016
#Drop out temp, precip, yearly ndvi; keep yearly viirs and dmsp for robustness checks
# Drop monthly maxl and meanl for jan/feb/mar 2012
wb_reshape <- wb_cells
wb_reshape1 <- wb_reshape[,-c(22:53,133:285,343:345)]

#Update final version of wb_reshape
wb_reshape <- wb_reshape1

#Order variables by name/time to allow reshape to work properly
wb_reshape<-wb_reshape[,order(names(wb_reshape))]

#Identify variables where values will change monthly in panel dataset
MaxL<-grep("maxl_",names(wb_reshape))
MeanL<-grep("meanl_",names(wb_reshape))
Viirs<-grep("viirs_",names(wb_reshape))

all_reshape <- c(MaxL,MeanL,Viirs)
wb_panel <- reshape(wb_reshape, varying=all_reshape, direction="long",idvar="cell_id",sep="_",timevar="Month")

#check panel construction
View(wb_panel[1:100])
View(wb_panel[100:177])

wb_panel_ch <- wb_panel[wb_panel$cell_id<220,]
ch_vars<-c("cell_id","Month","maxl","meanl","viirs")
wb_panel_ch <- wb_panel_ch[ch_vars]
wb_reshape_ch<-wb_reshape[wb_reshape$cell_id<305,]
View(wb_panel_ch[100:177])
View(wb_reshape[100:200])
View(wb_reshape[200:300])


#create dichotomous treatment variable
# #create yearmonth treatment values
# wb_panel$date_trt_m<-formatC(wb_panel$date_trt_m, width=2, format="d",flag="0")
# wb_panel$date_trt_ym<-as.numeric(paste(wb_panel$date_trt_y,wb_panel$date_trt_m,sep=""))
#set trt=1 if month is equal to or after date_treat_ym
wb_panel$trt1<-NA
wb_panel$trt1[which(wb_panel$Month<wb_panel$date_trt1_ym)]<-0
wb_panel$trt1[which(wb_panel$Month>=wb_panel$date_trt1_ym)]<-1
View(wb_panel[100:178])
#create dichotomous treatment variable for multiple treatments (i.e. cell present in more than one buffer)
#create from 2 to 8 buffers
#format month as 2 digits
temp_col_list <- c("trt2", "trt3", "trt4", "trt5", "trt6", "trt7", "trt8","trt9")
for(i in temp_col_list)
{
  #create treatment var for buffers 2-9
  wb_panel[i]<-NA
  wb_panel[i]<-ifelse(is.na(wb_panel[[paste0("date_",i,"_ym")]]),0,
                      ifelse(wb_panel$Month>=wb_panel[[paste0("date_",i,"_ym")]],1,0))
}
#check: treatment columns should not have any NAs (should only be 0 or 1)
#for trt 4, should be =0 for 233,191 obs and =1 for 2333 obs 
summary(wb_panel$trt4)
table(wb_panel$trt4)
table(wb_panel$date_trt4_ym)


#create slim version for analysis

trt<-paste(colnames(wb_panel)[grep("*trt",colnames(wb_panel))])
id<-paste(colnames(wb_panel)[grep("*_id",colnames(wb_panel))])
date<-paste(colnames(wb_panel)[grep("*date",colnames(wb_panel))])
dist<-paste(colnames(wb_panel)[grep("*dist",colnames(wb_panel))])
road<-paste(colnames(wb_panel)[grep("*road",colnames(wb_panel))])
extra<-c("Month","maxl","meanl","viirs","PCBS_CO")

slimvars<-c(trt,id,date,dist,road,extra)
wb_panel_slim <- wb_panel[slimvars]

write.csv(wb_panel_slim,"/Users/rbtrichler/Box Sync/usaidwestbank_roadsgie/Data/wb_panel_slim_750m.csv")


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





