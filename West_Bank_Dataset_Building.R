#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("C:/Users/jflak/OneDrive/Github/usaidwestbank_roadsgie/Data")

#needed packages
library(sf)
library(readxl)
library(stringdist)
library(plyr)

# #Loads old data (just for viewing if necessary), the "x" and "workable" data
# #loads "workable" and "x" dataframes
# load("Data_Old/WB_Question.RData")
# load("Data_Old/WB_wide.RData")

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
 
#changes "Name" columns in x to "road_name" columns
colnames(x) <- gsub("Name", "road_name", colnames(x))

#changes colnames of x to have "." instead of "_" before the number for each column
colnames(x) <- gsub("_([0-9])", ".\\1", colnames(x))

#creates separate date and road_name dataframes, deletes the other respective variable from the dataframes
x_road_name <- x[, -grep("date", colnames(x))]
x_date <- x[, -grep("road_name", colnames(x))]

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

#creates the treatment date column
x_merged$date.treat <- pmin(x_merged$date.1, x_merged$date.2, x_merged$date.3, x_merged$date.4,
                                x_merged$date.5, x_merged$date.6, x_merged$date.7, x_merged$date.8, na.rm = TRUE)

#loop-ified code to create a variable "treat_col" which contains the number of the treatment column (1-8)
for(i in 1:8)
{
  x_merged[["treat_col"]][x_merged[["date.treat"]] == x_merged[[paste0("date.", i)]]] <- i
}

#loop-ified code to change the road name columns from factor to character format
for(i in 1:8)
{
  x_merged[[paste0("road_name.", i)]] <- as.character(x_merged[[paste0("road_name.", i)]])
}

#loop-ified code to create the treatment road_name column
for(i in 1:8)
{
  x_merged[["road_name.treat"]][x_merged[["treat_col"]] == i] <- x_merged[[paste0("road_name.", i)]][x_merged[["treat_col"]] == i]
}

#loop-ified code to create the treatment buffer_id
for(i in 1:8)
{
  x_merged[["buffer_id.treat"]][x_merged[["treat_col"]] == i] <- x_merged[[paste0("buffer_id.", i)]][x_merged[["treat_col"]] == i]
}

#loop-ified code to split the date columns into day, month, year
temp_col_list <- c("treat", "1", "2", "3", "4", "5", "6", "7", "8")
for(i in temp_col_list)
{
  x_merged[[paste0("date.", i, ".d")]] <- as.numeric(format(x_merged[[paste0("date.", i)]], format = "%d"))
  x_merged[[paste0("date.", i, ".m")]] <- as.numeric(format(x_merged[[paste0("date.", i)]], format = "%m"))
  x_merged[[paste0("date.", i, ".y")]] <- as.numeric(format(x_merged[[paste0("date.", i)]], format = "%Y"))
}

#loop-ified code to reorder and trim the merged dataset to the columns we want in the correct order
final_col_list <- c("row_num", "cell_id", "treat_col")
for(i in temp_col_list)
{
  final_col_list <- c(final_col_list, paste0("buffer_id.", i), paste0("road_name.", i), paste0("date.", i),
                      paste0("date.", i, ".d"), paste0("date.", i, ".m"), paste0("date.", i, ".y"))
}
x_merged <- x_merged[, final_col_list]

#merge the covariates into x_merged
merge_wb_cells <- read.csv("merge_westbank_cells.csv")
x_merged[["cell_id"]] <- as.numeric(as.character(x_merged[["cell_id"]]))
x_merged <- join(x = x_merged, y = merge_wb_cells, by = "cell_id", type = "inner")

# #adds the geometry back into x_merged, making it an sf object again
# x_geo_col <- st_geometry(x_geometry)
# x_merged <- st_set_geometry(x_merged, x_geo_col)
# 
# #saves the finished merged shapefile
# st_write(x_merged, "x_merged.shp")
