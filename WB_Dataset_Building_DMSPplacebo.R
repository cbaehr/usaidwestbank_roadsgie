
# build placebo panel for West Bank 750m cells
# uses DMSP ntl values as outcome, 5 years prior to treatment date (so 2008-2011)

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
library(mondate)
library(geojsonio)


# ---------
# Import wb_cells file from WB_Dataset_Building.R
# ---------

wb_cells <- read.csv ("Data/wb_cells.csv")

# ---------
# Prepare wide-form dataset for panel construction and construct panel
# ---------

# Panel dataset will cover years 2007 to 2011
# that gives us one baseline year, then treatment for road segments between 2008 and 2011
# subtracting 5 years from year of treatment (ignoring month for dmsp)

#Drop out unneeded variables
#For vars that change over time, only using yearly vars 2007-2011
#Drop out yearly viirs, temp, precip, yearly ndvi; keep monthly viirs for robustness checks
wb_reshape <- wb_cells
wb_reshape1 <- wb_reshape[,-c(23:54,112:126,132:403)]

wb_reshape <- wb_reshape1

#Order variables by name/time to allow reshape to work properly
wb_reshape<-wb_reshape[,order(names(wb_reshape))]

#Identify variables where values will change monthly in panel dataset

DMSP<-grep("dmsp_",names(wb_reshape))

all_reshape <- c(DMSP)
wb_panel <- reshape(wb_reshape, varying=all_reshape, direction="long",idvar="cell_id",sep="_",timevar="Year")


# -------
# Create treatment and other variables
# -------

#create new date_trt1_y_5yp that reflects 5 years earlier than actual treatment year
wb_panel$date_trt1_y_5yp <- NA
wb_panel$date_trt1_y_5yp <- wb_panel$date_trt1_y - 5

#create new treatment variable trt1_5yp from date_trt1_y_5yp
wb_panel$trt1_5yp<-NA
wb_panel$trt1_5yp[which(wb_panel$Year<wb_panel$date_trt1_y_5yp)]<-0
wb_panel$trt1_5yp[which(wb_panel$Year>=wb_panel$date_trt1_y_5yp)]<-1
View(wb_panel[120:213])


# ------
# Write to file
# ------

# Write full version of panel data to file
write.csv(wb_panel,"/Users/rbtrichler/Box Sync/usaidwestbank_roadsgie/Data/wb_panel_750m_5yp.csv")



