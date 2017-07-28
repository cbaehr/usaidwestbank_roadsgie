#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("C:/Users/jflak/OneDrive/Github/usaidwestbank_roadsgie/Data")

#needed packages
library(readxl)
library(stringdist)
library(sf)

#reads the data from the files
shpfile = "poly_raster_data_merge.shp"
data_roads_shp = st_read(shpfile)
