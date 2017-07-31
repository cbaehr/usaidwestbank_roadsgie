#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("C:/Users/jflak/OneDrive/Github/usaidwestbank_roadsgie/Data")

#needed packages
library(sf)
library(rgdal)
library(maptools)
library(readxl)
library(stringdist)
library(plyr)

#Loads old data (just for viewing if necessary), the "x" and "workable" data
#loads "workable" and "x" dataframes
#load("Data_Old/WB_Question.RData")
#load("Data_Old/WB_wide.RData")

#reads the data from the files
shpfile <- "poly_raster_data_merge.shp"
#data_roads_shp <- readShapePoly(shpfile)
#x <- data_roads_shp@data
x <- st_read(shpfile)
buffer_shpfile <- "buffer.shp"
buffer_data <- st_read(buffer_shpfile)
inpii_data <- read_excel("INPIICSV_RoadsShapefile_Reconcile _comments_clean.xlsx")

#extract the geometry from x and coerce x to a dataframe
x_geometry <- st_geometry(x)
st_geometry(x) <- NULL
x_geometry <- st_set_geometry(as.data.frame(x$cell_id), x_geometry)
 
#changes "Name" columns in x to "road_name" columns
colnames(x) <- gsub("Name", "road_name", colnames(x))

#changes colnames of x to have "." instead of "_" before the number for each column
colnames(x) <- gsub("_([0-9])", ".\\1", colnames(x))

#creates separate date and road_name dataframes, deletes the other respective variable from the dataframes
x_road_name <- x[, -grep("date", colnames(x))]
x_date <- x[, -grep("road_name", colnames(x))]

#x_road_name <- x_road_name[, !names(x_road_name) %in% c("geometry")]

#shifts the data left in each dataframe so that the rightmost columns are all NAs, and renames the colnames to the original names
x_date_left = as.data.frame(t(apply(x_date, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(x_date_left) <- colnames(x_date)
x_road_name_left = as.data.frame(t(apply(x_road_name, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(x_road_name_left) <- colnames(x_road_name)

#Merges the two left-shifted dataframes and orders the columns at the same time - also deletes all columns 9 and greater (they're all NAs)
x_left <- cbind.data.frame(x_date_left$cell_id, x_date_left$date.1, x_road_name_left$road_name.1,
                           x_date_left$date.2, x_road_name_left$road_name.2,
                           x_date_left$date.3, x_road_name_left$road_name.3,
                           x_date_left$date.4, x_road_name_left$road_name.4,
                           x_date_left$date.5, x_road_name_left$road_name.5,
                           x_date_left$date.6, x_road_name_left$road_name.6,
                           x_date_left$date.7, x_road_name_left$road_name.7,
                           x_date_left$date.8, x_road_name_left$road_name.8)

#renames the colnames to the original names
colnames(x_left) <- c("cell_id", "date.1", "road_name.1",
                      "date.2", "road_name.2",
                      "date.3", "road_name.3",
                      "date.4", "road_name.4",
                      "date.5", "road_name.5",
                      "date.6", "road_name.6",
                      "date.7", "road_name.7",
                      "date.8", "road_name.8")

# #takes out the two columns we need from workable, makes a dataframe from them
# work_set <- cbind.data.frame(workable$road_name, workable$buffer_ID)
# colnames(work_set) <- c("road_name", "buffer_ID")
# work_set <- work_set[!duplicated(work_set), ]
# 
# #loop-ified code to create new dataframes corresponding to the 1-8 columns, rename them accordingly
# for(i in 1:8)
# {
#   assign(paste0("work_set", i), work_set)
#   assign(paste0("work_set", i), setNames(eval(parse(text = paste0("work_set", i))), c(paste0("road_name.", i), paste0("buffer_ID.", i))))
# }
# 
# #loop-ified code to merge the work_sets into the x_left dataframe (creating x_merged) to put in the buffer_IDs for each column 1-8
# x_merged <- x_left
# x_merged$row_num <- 1:nrow(x_merged)
# for(i in 1:8)
# {
#   x_merged <- join(x = x_merged, y = eval(as.name(paste0("work_set", i))), by = paste0("road_name.", i), type = "left")
# }
# 
# #loop-ified code to change the date columns from factor to character format
# for(i in 1:8)
# {
#   x_merged[, paste0("com_data.", i)] <- as.character(x_merged[, paste0("com_data.", i)])
# }





###Old Code###
# #creates separate com_data and road_name dataframes, deletes the other respective variable from the dataframes
# x_com_data <- x[, -grep("road_name", colnames(x))]
# x_road_name <- x[, -grep("com_data", colnames(x))]
# 
# #shifts the data left in each dataframe so that the rightmost columns are all NAs, and renames the colnames to the original names
# x_com_data_left = as.data.frame(t(apply(x_com_data, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
# colnames(x_com_data_left) <- colnames(x_com_data)
# x_road_name_left = as.data.frame(t(apply(x_road_name, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
# colnames(x_road_name_left) <- colnames(x_road_name)
# 
# #Merges the two left-shifted dataframes and orders the columns at the same time - also deletes all columns 9 and greater (they're all NAs)
# x_left <- cbind.data.frame(x_com_data_left$cell_ID, x_com_data_left$com_data.1, x_road_name_left$road_name.1,
#                            x_com_data_left$com_data.2, x_road_name_left$road_name.2,
#                            x_com_data_left$com_data.3, x_road_name_left$road_name.3,
#                            x_com_data_left$com_data.4, x_road_name_left$road_name.4,
#                            x_com_data_left$com_data.5, x_road_name_left$road_name.5,
#                            x_com_data_left$com_data.6, x_road_name_left$road_name.6,
#                            x_com_data_left$com_data.7, x_road_name_left$road_name.7,
#                            x_com_data_left$com_data.8, x_road_name_left$road_name.8)
# 
# #renames the colnames to the original names
# colnames(x_left) <- c("cell_ID", "com_data.1", "road_name.1",
#                       "com_data.2", "road_name.2",
#                       "com_data.3", "road_name.3",
#                       "com_data.4", "road_name.4",
#                       "com_data.5", "road_name.5",
#                       "com_data.6", "road_name.6",
#                       "com_data.7", "road_name.7",
#                       "com_data.8", "road_name.8")
# 
# #takes out the two columns we need from workable, makes a dataframe from them
# work_set <- cbind.data.frame(workable$road_name, workable$buffer_ID)
# colnames(work_set) <- c("road_name", "buffer_ID")
# work_set <- work_set[!duplicated(work_set), ]
# 
# #loop-ified code to create new dataframes corresponding to the 1-8 columns, rename them accordingly
# for(i in 1:8)
# {
#   assign(paste0("work_set", i), work_set)
#   assign(paste0("work_set", i), setNames(eval(parse(text = paste0("work_set", i))), c(paste0("road_name.", i), paste0("buffer_ID.", i))))
# }
# 
# #loop-ified code to merge the work_sets into the x_left dataframe (creating x_merged) to put in the buffer_IDs for each column 1-8
# x_merged <- x_left
# x_merged$row_num <- 1:nrow(x_merged)
# for(i in 1:8)
# {
#   x_merged <- join(x = x_merged, y = eval(as.name(paste0("work_set", i))), by = paste0("road_name.", i), type = "left")
# }
# 
# #loop-ified code to change the date columns from factor to character format
# for(i in 1:8)
# {
#   x_merged[, paste0("com_data.", i)] <- as.character(x_merged[, paste0("com_data.", i)])
# }
# 
# #loop-ified code to change the 9999 markers that indicate no project completed yet to "1/x/2020" so that they can be sorted with date functions
# for(i in 1:8)
# {
#   x_merged[[paste0("com_data.", i)]][x_merged[[paste0("com_data.", i)]] == "9999"] <- paste0("1/", i, "/9999")
# }
# 
# #loop-ified code to change the date variable columns to date format for easier manipulation
# for(i in 1:8)
# {
#   x_merged[[paste0("com_data.", i)]] <- as.Date(x_merged[[paste0("com_data.", i)]], format = "%m/%d/%Y", origin = "01/01/1900")
# }
# 
# #creates the treatment date column
# x_merged$com_data.treat <- pmin(x_merged$com_data.1, x_merged$com_data.2, x_merged$com_data.3, x_merged$com_data.4,
#                                 x_merged$com_data.5, x_merged$com_data.6, x_merged$com_data.7, x_merged$com_data.8, na.rm = TRUE)
# 
# #loop-ified code to create a variable "treat_col" which contains the number of the treatment column (1-8)
# for(i in 1:8)
# {
#   x_merged[["treat_col"]][x_merged[["com_data.treat"]] == x_merged[[paste0("com_data.", i)]]] <- i
# }
# 
# #loop-ified code to change the road name columns from factor to character format
# for(i in 1:8)
# {
#   x_merged[[paste0("road_name.", i)]] <- as.character(x_merged[[paste0("road_name.", i)]])
# }
# 
# #loop-ified code to create the treatment road_name column
# for(i in 1:8)
# {
#   x_merged[["road_name.treat"]][x_merged[["treat_col"]] == i] <- x_merged[[paste0("road_name.", i)]][x_merged[["treat_col"]] == i]
# }
# 
# #loop-ified code to create the treatment buffer_ID
# for(i in 1:8)
# {
#   x_merged[["buffer_ID.treat"]][x_merged[["treat_col"]] == i] <- x_merged[[paste0("buffer_ID.", i)]][x_merged[["treat_col"]] == i]
# }
# 
# #loop-ified code to split the date columns into day, month, year
# temp_col_list <- c("1", "2", "3", "4", "5", "6", "7", "8", "treat")
# for(i in temp_col_list)
# {
#   x_merged[[paste0("com_data.", i, ".d")]] <- as.numeric(format(x_merged[[paste0("com_data.", i)]], format = "%d"))
#   x_merged[[paste0("com_data.", i, ".m")]] <- as.numeric(format(x_merged[[paste0("com_data.", i)]], format = "%m"))
#   x_merged[[paste0("com_data.", i, ".y")]] <- as.numeric(format(x_merged[[paste0("com_data.", i)]], format = "%Y"))
# }
# 
# #reorders and trims the merged dataset to the columns we want in the correct order
# x_merged <- x_merged[, c("row_num", "cell_ID", "treat_col", "buffer_ID.treat", "road_name.treat", "com_data.treat.d", "com_data.treat.m", "com_data.treat.y",
#                          "buffer_ID.1", "road_name.1", "com_data.1.d", "com_data.1.m", "com_data.1.y",
#                          "buffer_ID.2", "road_name.2", "com_data.2.d", "com_data.2.m", "com_data.2.y",
#                          "buffer_ID.3", "road_name.3", "com_data.3.d", "com_data.3.m", "com_data.3.y",
#                          "buffer_ID.4", "road_name.4", "com_data.4.d", "com_data.4.m", "com_data.4.y",
#                          "buffer_ID.5", "road_name.5", "com_data.5.d", "com_data.5.m", "com_data.5.y",
#                          "buffer_ID.6", "road_name.6", "com_data.6.d", "com_data.6.m", "com_data.6.y",
#                          "buffer_ID.7", "road_name.7", "com_data.7.d", "com_data.7.m", "com_data.7.y",
#                          "buffer_ID.8", "road_name.8", "com_data.8.d", "com_data.8.m", "com_data.8.y")]
