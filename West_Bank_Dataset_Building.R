#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("C:/Users/jflak/OneDrive/Github/usaidwestbank_roadsgie/Data")
setwd("/Users/rbtrichler/Documents/AidData/Git Repos/usaidwestbank_roadsgie/Data")

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


# ##Loads old data (just for viewing if necessary), the "x" and "workable" data
# #loads "workable" and "x" dataframes
# #load("Data_Old/WB_Question.RData")
# load("Data_Old/WB_wide.RData")
# #creates x_old from the old x file
# old_x <- x

#reads the data from the files
shpfile <- "poly_raster_data_merge2.shp"
x <- st_read(shpfile)
buffer_shpfile <- "buffer.shp"
buffer_data <- st_read(buffer_shpfile)
inpii_data <- read_excel("INPIICSV_RoadsShapefile_Reconcile _comments_clean.xlsx")

#extract the geometry from x and coerce x to a dataframe
x_geometry <- st_geometry(x)
st_geometry(x) <- NULL
x_geometry <- st_set_geometry(as.data.frame(x$cell_id), x_geometry)

#merge in distance from each cell to road segment for which it falls within buffer
#distroad includes name and date (can use to check if matches with "x"), but geometry extracted from "x" so will merge distroad in
distroad <- read.csv("cell_dist_extract.csv")
distroad_short <- distroad[,-grep("date",colnames(distroad))]
distroad_short <- distroad_short[,-grep("Name",colnames(distroad_short))]
x1 <- merge(x, distroad_short, by="cell_id")
x <- x1

#changes "Name" columns in x to "road_name" columns
colnames(x) <- gsub("Name", "road_name", colnames(x))

#changes colnames of x to have "." instead of "_" before the number for each column
colnames(x) <- gsub("_([0-9])", ".\\1", colnames(x))

#creates separate date and road_name dataframes, deletes the other respective variable from the dataframes
x_road_name <- x[, -grep("date", colnames(x))]
x_road_name <- x_road_name[, -grep("dist", colnames(x_road_name))]

x_date <- x[, -grep("road_name", colnames(x))]
x_date <- x_date[, -grep("dist", colnames(x_date))]

x_dist <- x[,-grep("date",colnames(x))]
x_dist <- x_dist[,-grep("road_name",colnames(x_dist))]

#shifts the data left in each dataframe so that the rightmost columns are all NAs, and renames the colnames to the original names
x_road_name_left = as.data.frame(t(apply(x_road_name, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(x_road_name_left) <- colnames(x_road_name)

x_date_left = as.data.frame(t(apply(x_date, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(x_date_left) <- colnames(x_date)

x_dist_left = as.data.frame(t(apply(x_dist, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(x_dist_left) <- colnames(x_dist)

# #Double check data shift
# #numbers of NA should match
# #should have values in column 8 but not in column 9
# sum(is.na(x_dist_left$dist.2))
# sum(is.na(x_road_name_left$road_name.2))
# sum(is.na(x_dist_left$dist.8))
# sum(is.na(x_road_name_left$road_name.8))
# sum(is.na(x_dist_left$dist.9))
# sum(is.na(x_road_name_left$road_name.9))

#Merges the three left-shifted dataframes and orders the columns at the same time - also deletes all columns 9 and greater (they're all NAs)
#Must sort components first to be sure rows are in the same order
x_date_left<-x_date_left[order(x_date_left$cell_id),]
x_dist_left<-x_dist_left[order(x_dist_left$cell_id),]
x_road_name_left<-x_road_name_left[order(x_road_name_left$cell_id),]

x_left <- cbind.data.frame(x_date_left$cell_id, x_date_left$date.1, x_road_name_left$road_name.1,x_dist_left$dist.1,
                           x_date_left$date.2, x_road_name_left$road_name.2, x_dist_left$dist.2,
                           x_date_left$date.3, x_road_name_left$road_name.3, x_dist_left$dist.3,
                           x_date_left$date.4, x_road_name_left$road_name.4, x_dist_left$dist.4,
                           x_date_left$date.5, x_road_name_left$road_name.5, x_dist_left$dist.5,
                           x_date_left$date.6, x_road_name_left$road_name.6, x_dist_left$dist.6,
                           x_date_left$date.7, x_road_name_left$road_name.7, x_dist_left$dist.7,
                           x_date_left$date.8, x_road_name_left$road_name.8, x_dist_left$dist.8)
#Check column bind by manually looking at a few random cell ids

#renames the colnames to the original names
colnames(x_left) <- c("cell_id", "date.1", "road_name.1","dist.1",
                      "date.2", "road_name.2","dist.2",
                      "date.3", "road_name.3","dist.3",
                      "date.4", "road_name.4","dist.4",
                      "date.5", "road_name.5","dist.5",
                      "date.6", "road_name.6","dist.6",
                      "date.7", "road_name.7","dist.7",
                      "date.8", "road_name.8","dist.8")

#extract the geometry from buffer_data and coerce buffer_data to a dataframe
buffer_geometry <- st_geometry(buffer_data)
st_geometry(buffer_data) <- NULL
buffer_geometry <- st_set_geometry(as.data.frame(buffer_data$index), buffer_geometry)

#takes out the two columns we need from buffer_data, makes a dataframe from them
work_set <- cbind.data.frame(buffer_data$Name, buffer_data$index)
colnames(work_set) <- c("road_name", "buffer_id")

##The below code uses fuzzy matching to fix the names in work_set and x_left (later x_merged) to be exactly the same
#creates a list of the unique road names that are in at least one of road_name 1,2,3 etc.
road_names_list <- unique(c(levels(x_left[["road_name.1"]]), levels(x_left[["road_name.2"]]), levels(x_left[["road_name.3"]]), levels(x_left[["road_name.4"]]), 
                            levels(x_left[["road_name.5"]]), levels(x_left[["road_name.6"]]), levels(x_left[["road_name.7"]]), levels(x_left[["road_name.8"]])))

#creates a list of the indices of the (fuzzy) matching names in work_set and road_names_list
matched_list <- amatch(work_set[["road_name"]], road_names_list, maxDist = 50)

#changes the road_names in work_set (originally from buffer_data) to be the names in x_left (originally from poly_raster_data_merge2)
work_set[["road_name"]] <- as.character(road_names_list[matched_list])

#loop-ified code to create new dataframes corresponding to the 1-8 columns, rename them accordingly
for(i in 1:8)
{
  assign(paste0("work_set", i), work_set)
  assign(paste0("work_set", i), setNames(eval(parse(text = paste0("work_set", i))), c(paste0("road_name.", i), paste0("buffer_id.", i))))
}

#loop-ified code to merge the work_sets into the x_left dataframe (creating x_merged) to put in the buffer_IDs for each column 1-8
x_merged <- x_left
x_merged$row_num <- 1:nrow(x_merged)
for(i in 1:8)
{
  x_merged <- join(x = x_merged, y = eval(as.name(paste0("work_set", i))), by = paste0("road_name.", i), type = "left")
}

#loop-ified code to change the date columns from factor to character format
for(i in 1:8)
{
  x_merged[, paste0("date.", i)] <- as.character(x_merged[, paste0("date.", i)])
}

#loop-ified code to create a 9999 date marker for each date that is "NA" but has a road_name - marker is 9999-01-0x
for(i in 1:8)
{
  x_merged[paste0("date.", i)][is.na(x_merged[paste0("date.", i)]) & !is.na(x_merged[paste0("road_name.", i)])] <- paste0("9999-01-0", i)
}

#loop-ified code to change the date variable columns to date format for easier manipulation
for(i in 1:8)
{
  x_merged[[paste0("date.", i)]] <- as.Date(x_merged[[paste0("date.", i)]], format = "%Y-%m-%d", origin = "1900-01-01")
}

#creates second treatment date column
test<-x_merged
x_merged$date_trt<-apply(test[c(2,5,8,11,14,17,20,23)],1,function(x) rev(sort(x,decreasing=TRUE))[1])
x_merged$date_trt2 <- apply(test[c(2,5,8,11,14,17,20,23)],1,function(x) rev(sort(x,decreasing=TRUE))[2])
x_merged$date_trt3 <- apply(test[c(2,5,8,11,14,17,20,23)],1,function(x) rev(sort(x,decreasing=TRUE))[3])
x_merged$date_trt4 <- apply(test[c(2,5,8,11,14,17,20,23)],1,function(x) rev(sort(x,decreasing=TRUE))[4])
x_merged$date_trt5 <- apply(test[c(2,5,8,11,14,17,20,23)],1,function(x) rev(sort(x,decreasing=TRUE))[5])
x_merged$date_trt6 <- apply(test[c(2,5,8,11,14,17,20,23)],1,function(x) rev(sort(x,decreasing=TRUE))[6])
x_merged$date_trt7 <- apply(test[c(2,5,8,11,14,17,20,23)],1,function(x) rev(sort(x,decreasing=TRUE))[7])
x_merged$date_trt8 <- apply(test[c(2,5,8,11,14,17,20,23)],1,function(x) rev(sort(x,decreasing=TRUE))[8])
# #check: below should return all NAs
# test$trt9 <- apply(test[c(2,5,8,11,14,17,20,23)],1,function(x) rev(sort(x,decreasing=TRUE))[9])
# count(is.na(test$trt9))

#loop-ified code to change the date variable columns to date format for easier manipulation
for(i in 1:8)
{
  x_merged[[paste0("date.", i)]] <- as.Date(x_merged[[paste0("date.", i)]], format = "%Y-%m-%d", origin = "1900-01-01")
}

# #creates the treatment date column
# x_merged$date.treat <- pmin(x_merged$date.1, x_merged$date.2, x_merged$date.3, x_merged$date.4,
#                                 x_merged$date.5, x_merged$date.6, x_merged$date.7, x_merged$date.8, na.rm = TRUE)

#loop-ified code to create a variable "treat_col" which contains the number of the treatment columns (1-8)
for(i in 1:8)
{
  x_merged[["trt_col"]][x_merged[["date_trt"]] == x_merged[[paste0("date.", i)]]] <- i
  x_merged[["trt2_col"]][x_merged[["date_trt2"]] == x_merged[[paste0("date.", i)]]] <- i
  x_merged[["trt3_col"]][x_merged[["date_trt3"]] == x_merged[[paste0("date.", i)]]] <- i
  x_merged[["trt4_col"]][x_merged[["date_trt4"]] == x_merged[[paste0("date.", i)]]] <- i
  x_merged[["trt5_col"]][x_merged[["date_trt5"]] == x_merged[[paste0("date.", i)]]] <- i
  x_merged[["trt6_col"]][x_merged[["date_trt6"]] == x_merged[[paste0("date.", i)]]] <- i
  x_merged[["trt7_col"]][x_merged[["date_trt7"]] == x_merged[[paste0("date.", i)]]] <- i
  x_merged[["trt8_col"]][x_merged[["date_trt8"]] == x_merged[[paste0("date.", i)]]] <- i

}

#loop-ified code to change the road name columns from factor to character format
for(i in 1:8)
{
  x_merged[[paste0("road_name.", i)]] <- as.character(x_merged[[paste0("road_name.", i)]])
}

#loop-ified code to create the treatment road_name column
for(i in 1:8)
{
  x_merged[["road_name.treat"]][x_merged[["trt_col"]] == i] <- x_merged[[paste0("road_name.", i)]][x_merged[["trt_col"]] == i]
}

#loop-ified code to create the treatment buffer_id
for(i in 1:8)
{
  x_merged[["buffer_id.treat"]][x_merged[["trt_col"]] == i] <- x_merged[[paste0("buffer_id.", i)]][x_merged[["trt_col"]] == i]
}

#loop-ified code to create the treatment road distance
for(i in 1:8)
{
  x_merged[["dist.treat"]][x_merged[["trt_col"]]== i] <- x_merged[[paste0("dist.",i)]][x_merged[["trt_col"]]==i]
}

#loop-ified code to put the treatment date columns in standard date format for easier manipulation
temp_col_list <- c("trt", "trt2", "trt3", "trt4", "trt5", "trt6", "trt7", "trt8")
for(i in temp_col_list)
{
  x_merged[[paste0("date_", i)]] <- as.Date(x_merged[[paste0("date_", i)]], format = "%Y-%m-%d", origin = "1900-01-01")
}

#loop-ified code to split the date columns into day, month, year
temp_col_list <- c("trt", "trt2", "trt3", "trt4", "trt5", "trt6", "trt7", "trt8")
for(i in temp_col_list)
{
  x_merged[[paste0("date_", i, "_d")]] = as.numeric(format(x_merged[[paste0("date_", i)]], format = "%d"))
  x_merged[[paste0("date_", i, "_m")]] = as.numeric(format(x_merged[[paste0("date_", i)]], format = "%m"))
  x_merged[[paste0("date_", i, "_y")]] = as.numeric(format(x_merged[[paste0("date_", i)]], format = "%Y"))
}

##SHOULD REORG THIS TO PUT THINGS IN ORDER
# #loop-ified code to reorder and trim the merged dataset to the columns we want in the correct order
# final_col_list <- c("row_num", "cell_id", "treat_col")
# for(i in temp_col_list)
# {
#   final_col_list <- c(final_col_list, paste0("buffer_id.", i), paste0("road_name.", i),
#                       paste0("dist.",i), paste0("date.", i),
#                       paste0("date.", i, ".d"), paste0("date.", i, ".m"), paste0("date.", i, ".y"))
# }
# x_merged <- x_merged[, final_col_list]


#fixes ACTUAL_COM column in inpii_data to match the rest of the columns and not be in the (off by 2 days) excel numeric format
inpii_data[["ACTUAL_COM"]][inpii_data[["ACTUAL_COM"]] == 9999] <- NA
inpii_data$ACTUAL_COM <- as.Date(inpii_data$ACTUAL_COM, format = "%Y-%m-%d", origin = "1899-12-30")
inpii_data[["ACTUAL_COM"]][is.na(inpii_data[["ACTUAL_COM"]])] <- "9999-01-01"


##The below code uses fuzzy matching to fix the names in inpii_data and x_merged to be exactly the same
#road_names_list was created earlier for the prior fuzzy matching/replacement

#creates a list of the indices of the (fuzzy) matching names in work_set and road_names_list
matched_list <- amatch(inpii_data[["Name_INPIIRoadsProject_Line"]], road_names_list, maxDist = 50)

#changes the road_names in work_set (originally from buffer_data) to be the names in x_left (originally from poly_raster_data_merge2)
inpii_data[["Name_INPIIRoadsProject_Line"]] <- as.character(road_names_list[matched_list])


#changes inpii_data column "Name_INPIIRoadsProject_Line" to "road_name.treat" so that inpii_data can be merged with x_merged
names(inpii_data)[names(inpii_data) == "Name_INPIIRoadsProject_Line"] <- "road_name.treat"

#merge inpii_data into x_merged
x_merged <- join(x = x_merged, y = inpii_data, by = "road_name.treat", type = "left")

#Double check that dataset was created correctly!

#Read in the covariate extract from geoquery and rename variables in preparation for merge
merge_wb_cells <- read.csv("merge_westbank_cells.csv")
#rename covariate columns for use in analytical models
#remove ".mean" at end of covariates
colnames(merge_wb_cells) <- gsub(".mean", "", colnames(merge_wb_cells), fixed=TRUE)
colnames(merge_wb_cells) <- gsub(".na", "", colnames(merge_wb_cells), fixed=TRUE)
#rename
colnames(merge_wb_cells) <- sub("dist_to_water","waterdist",colnames(merge_wb_cells))
colnames(merge_wb_cells) <- sub("dist_to_groads","roaddist",colnames(merge_wb_cells))
colnames(merge_wb_cells) <- sub("srtm_elevation_500m","elevation",colnames(merge_wb_cells))
colnames(merge_wb_cells) <- sub("srtm_slope_500m","slope",colnames(merge_wb_cells))
colnames(merge_wb_cells) <- sub("accessibility_map","urbtravtime",colnames(merge_wb_cells))
colnames(merge_wb_cells) <- sub("gpw_v[0-9]_density.","pop_",colnames(merge_wb_cells))
#max ndvi values from LTDR AVHRR
colnames(merge_wb_cells) <- sub("ltdr_avhrr_yearly_ndvi.","maxl_",colnames(merge_wb_cells))
#viirs ntl
colnames(merge_wb_cells) <- sub("ntl_yearly.","",colnames(merge_wb_cells))
colnames(merge_wb_cells) <- sub("ntl_monthly.","",colnames(merge_wb_cells))
#dmsp ntl
colnames(merge_wb_cells) <- sub("v4composites_calibrated.","dmsp_",colnames(merge_wb_cells))
#temp
colnames(merge_wb_cells) <- sub("udel_air_temp_v4_01_yearly_mean.","MeanT_",colnames(merge_wb_cells))
colnames(merge_wb_cells) <- sub("udel_air_temp_v4_01_yearly_min.","MinT_",colnames(merge_wb_cells))
colnames(merge_wb_cells) <- sub("udel_air_temp_v4_01_yearly_max.","MaxT_",colnames(merge_wb_cells))
#precip
colnames(merge_wb_cells) <- sub("udel_precip_v4_01_yearly_mean.","MeanP_",colnames(merge_wb_cells))
colnames(merge_wb_cells) <- sub("udel_precip_v4_01_yearly_min.","MinP_",colnames(merge_wb_cells))
colnames(merge_wb_cells) <- sub("udel_precip_v4_01_yearly_max.","MaxP_",colnames(merge_wb_cells))

#merge the covariates into x_merged (note: all cell_ids should match, leaving the same 4449 observations - if it doesn't look for errors)
x_merged[["cell_id"]] <- as.numeric(as.character(x_merged[["cell_id"]]))
x_merged1 <- join(x = x_merged, y = merge_wb_cells, by = "cell_id", type = "inner")

#merge in monthly ndvi (was provided in separate geoquery extract from above file)
ndvi<-read.csv("merge_westbank_cells_monthlyndvi.csv")
ndvi_max <- ndvi[,-(2:62)]
ndvi_mean <- ndvi[,-(63:122)]
ndvi_mean <-ndvi_mean[,-grep("(date_59)",names(ndvi_mean))]
#rename vars
colnames(ndvi_max)<-sub("ltdr_avhrr_monthly_ndvi.","maxl_",colnames(ndvi_max))
colnames(ndvi_max) <- gsub(".max", "", colnames(ndvi_max), fixed=TRUE)
colnames(ndvi_mean)<-sub("ltdr_avhrr_monthly_ndvi.","meanl_",colnames(ndvi_mean))
colnames(ndvi_mean) <- gsub(".mean", "", colnames(ndvi_mean), fixed=TRUE)
#merge
ndvi_monthly <- merge(ndvi_max, ndvi_mean, by="cell_id")
x_merged2 <- join(x=x_merged1, y=ndvi_monthly, by="cell_id",type ="inner")


##Drop cells that fall outside of West Bank admin boundaries
#read in file, convert to dataframe, and create column to identify cells once merged into larger dataset
exclude <- st_read("buffercells_nonWestBank.shp")
exclude <- as.data.frame(exclude)
exclude$excl_check <- 1
#exclude everything but id and excl_check column 
exclude_ids<-exclude[,-(2:120)]
#merge into x_merged 
x_merged3 <- join (x=x_merged2, y=exclude_ids, by="cell_id",type="full")
x_merged3$excl <- 0
x_merged3$excl[which(x_merged3$excl_check==1)]<-1

#drop out cells that fall outside of West Bank border (where excl_check=1)
#should be 4132 cells remaining, same number of vars
x_merged4 <- x_merged3[(x_merged3$excl==0),]

##merge in municipality info at cell level
# read in muni data and drop extra columns
muni_shp <- st_read("cells_localities_join.shp")
muni<-as.data.frame(muni_shp)
#drop out road segment info and just preserve muni info
muni <- muni[,-(2:119)]
muni <- muni[,-grep("geometry",names(muni))]
#merge
x_merged5 <- join(x=x_merged4, y=muni, by="cell_id", type="left")

#Saves updated form of x_merged as a dataframe
x_merged <- x_merged5


# #adds the geometry back into x_merged, making it an sf object again
# #first remove cells that fall outside of West Bank admin border using exclude_ids (created earlier)
# x_geometry_excl <- merge (x=x_geometry, y= exclude_ids, by.x="x.cell_id", by.y="cell_id", all=TRUE)
# x_geometry_excl <- x_geometry_excl[is.na(x_geometry_excl$excl_check),]
# x_geo_col <- st_geometry(x_geometry_excl)
# #add geometry back in
# x_merged6 <- st_set_geometry(x_merged5, x_geo_col)

##saves the finished merged shapefile
##NOT WORKING - CHANGES COLUMN NAMES
#st_write(x_merged6, "x_merged.shp", delete_layer=TRUE)


#----------
#Convert from wide-form to long-form panel dataset
#----------
#monthly viirs starts April 2012
#ndvi monthly
#pop every 5 years - can't use since it takes ntl into account
#temp and precip end in 2014 - can't use those either

#Drop out unneeded variables
#For vars that change over time, only using monthly vars starting April 2012 through Dec 2016
wb_reshape <- x_merged
wb_reshape1 <- wb_reshape[,-c(185:216,274:448,506:508)]
#Update final version of wb_reshape
wb_reshape <- wb_reshape1

#Order variables by name/time to allow reshape to work properly
wb_reshape<-wb_reshape[,order(names(wb_reshape))]

#Identify variables where values will change yearly in panel dataset
MaxL<-grep("maxl_",names(wb_reshape))
MeanL<-grep("meanl_",names(wb_reshape))
Viirs<-grep("viirs_",names(wb_reshape))

all_reshape <- c(MaxL,MeanL,Viirs)
wb_panel <- reshape(wb_reshape, varying=all_reshape, direction="long",idvar="cell_id",sep="_",timevar="Month")

#check panel construction
View(wb_panel[1:100])
View(wb_panel[100:200])
View(wb_panel[180:230])
wb_panel_ch <- wb_panel[wb_panel$cell_id<305,]
ch_vars<-c("cell_id","Month","maxl","meanl","viirs")
wb_panel_ch <- wb_panel_ch[ch_vars]
wb_reshape_ch<-wb_reshape[wb_reshape$cell_id<305,]
View(wb_reshape_ch[100:200])
View(wb_reshape_ch[280:380])

#create dichotomous treatment variable
#create yearmonth treatment values
wb_panel$date_trt_m<-formatC(wb_panel$date_trt_m, width=2, format="d",flag="0")
wb_panel$date_trt_ym<-as.numeric(paste(wb_panel$date_trt_y,wb_panel$date_trt_m,sep=""))
#set trt=1 if month is equal to or after date_treat_ym
wb_panel$trt<-NA
wb_panel$trt[which(wb_panel$Month<wb_panel$date_trt_ym)]<-0
wb_panel$trt[which(wb_panel$Month>=wb_panel$date_trt_ym)]<-1
View(wb_panel[200:232])
#create dichotomous treatment variable for multiple treatments (i.e. cell present in more than one buffer)
#create from 2 to 8 buffers
#format month as 2 digits
temp_col_list <- c("trt2", "trt3", "trt4", "trt5", "trt6", "trt7", "trt8")
for(i in temp_col_list)
{
  #format treatment month as 2 digits
  wb_panel[[paste0("date_",i, "_m")]] <- formatC(wb_panel[[paste0("date_", i,"_m")]], width=2,format="d",flag="0")
  #create date in year+month for all treatment columns
  wb_panel[[paste0("date_",i,"_ym")]]<-as.numeric(paste(wb_panel[[paste0("date_",i,"_y")]],wb_panel[[paste0("date_",i,"_m")]],sep=""))
  #create treatment var for buffers 2-8
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
wb_panel_slim <- wb_panel[c(11:73,138,161:169,206:215)]
write.csv(wb_panel_slim,"/Users/rbtrichler/Documents/AidData/wb_panel_slim.csv")



###SCRATCH


###Start of code to compare the old and new data
buffer_num <- seq(1, 59, 1)
compare_old_new <- data.frame(buffer_num)
compare_old_new["old_name"] <- NA
compare_old_new["new_name"] <- NA
compare_old_new["old_date"] <- NA
compare_old_new["new_date"] <- NA
compare_old_new["diff_name"] <- NA
compare_old_new["diff_date"] <- NA

for(i in 1:59)
{
  compare_old_new["old_name"][i,] <- sum(!is.na(old_x[[paste0("road_name.", i)]]))
  compare_old_new["old_date"][i,] <- sum(!is.na(old_x[[paste0("com_data.", i)]]))
  compare_old_new["new_name"][i,] <- sum(!is.na(x[[paste0("road_name.", i)]]))
  compare_old_new["new_date"][i,] <- sum(!is.na(x[[paste0("date.", i)]]))
  compare_old_new["diff_name"][i,] <- compare_old_new["old_name"][i,] - compare_old_new["new_name"][i,]
  compare_old_new["diff_date"][i,] <- compare_old_new["old_date"][i,] - compare_old_new["new_date"][i,]
}

total_diff <- sum(compare_old_new["diff_name"])


#Viewing some of the datasets
# View(merge_wb_cells[1:100])
# View(merge_wb_cells[101:200])
# View(merge_wb_cells[201:275])



mydata1 = mydata[,grepl("^INC",names(mydata))]
