#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("C:/Users/jflak/OneDrive/Github/usaidwestbank_roadsgie/Data")

#needed packages
library(sf)
library(readxl)
library(stringdist)
library(plyr)

##Loads old data (just for viewing if necessary), the "x" and "workable" data
#loads "workable" and "x" dataframes
#load("Data_Old/WB_Question.RData")
load("Data_Old/WB_wide.RData")
#creates x_old from the old x file
old_x <- x

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

#merge the covariates into x_merged (note: all cell_ids should match, leaving the same 4449 observations - if it doesn't look for errors)
merge_wb_cells <- read.csv("merge_westbank_cells.csv")
x_merged[["cell_id"]] <- as.numeric(as.character(x_merged[["cell_id"]]))
x_merged <- join(x = x_merged, y = merge_wb_cells, by = "cell_id", type = "inner")
# 
# #adds the geometry back into x_merged, making it an sf object again
# x_geo_col <- st_geometry(x_geometry)
# x_merged <- st_set_geometry(x_merged, x_geo_col)
# 
# #saves the finished merged shapefile
# st_write(x_merged, "x_merged.shp")


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




###Start of code related to PSM matching with multilevelMatching package###

###Reminder/Explanation of multilevelGPSMatch:
##Y is "a continuous response vector" aka the vector with the dependent variable/variable of interest
##W is "a treatment vector with numerical values indicating treatment groups" aka a vector in factor format for which treatment group a unit was in
##X is "a covariate matrix with no intercept" aka a matrix (or vector works if there's only one) with covariates

#more complex inputs that I made that do NOT work
#Y_sample <- rep(1, length(x_merged[["buffer_ID.treat"]]))
#Y_sample <- x_merged$row_num*runif(length(x_merged$row_num), 0, 20)
#W_sample <- x_merged$buffer_ID.treat
#X_sample <- matrix(data = c(x_merged$row_num, 2*x_merged$row_num, 3*x_merged$row_num), nrow = length(x_merged$row_num), ncol = 3)
#X_sample <- c(x_merged$row_num*0.05)

# #other inputs that I made that work (matrix and vector both work for X)
# Y_sample <- c(1, 2,3,4,5,6,7,8,9)
# X_sample <- matrix(data = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27), nrow = 9, ncol = 3)
# X_sample <- c(1, 2, 3, 4 ,5 ,6 ,7 ,8, 9)
# W_sample <- c(1,1,1,2,2,2,3,3,3)

# #example inputs from the readme
# X_sample<-c(5.5,10.6,3.1,8.7,5.1,10.2,9.8,4.4,4.9)
# Y_sample<-c(102,105,120,130,100,80,94,108,96)
# W_sample<-c(1,1,1,3,2,3,2,1,2)

# #more made-up inputs by me (work with Trimming = FALSE, don't work with Trimming = TRUE) - also produce reasonable results
# Y_sample <- c(1,1,2,3,4,3,6,5,7)
# W_sample <- c(1,1,1,2,2,2,3,3,3)
# X_sample <- c(0.5,0.9,1.2,1.1,1.4,1.5,3,2,2.1)


##test multilevelGPSMatch, with sample data
#multilevelGPSMatch.results <- multilevelGPSMatch(Y = Y_sample, W = W_sample, X = X_sample, Trimming = FALSE, GPSM="multinomiallogisticReg")
#multilevelMatchX()

# #creating "real" Y, W, and X
# Y_real <- x_merged["viirs_ntl_yearly.2016.mean"]
# W_real <- x_merged["buffer_id.treat"]
# X_real <- matrix(data = c(x_merged[["dist_to_water.na.mean"]], x_merged[["ltdr_avhrr_yearly_ndvi.2016.mean"]]), nrow = 4449, ncol = 2)
# 
# 
# ##multilevelGPSMatch on actual data
# multilevelGPSMatch.results <- multilevelGPSMatch(Y = Y_real, W = W_real, X = X_real, Trimming = FALSE, GPSM = "multinomiallogisticReg")





###Code of the multilevelGPSMatch function:
# 
# Y <- Y_real
# W <- W_real
# X <- X_real
# 
# Trimming <- FALSE
# GPSM = "multinomiallogisticReg"
# 
# if(Trimming==1){
#   #PF modeling
#   W.ref <- relevel(as.factor(W),ref=1)
#   temp<-capture.output(PF.out <- multinom(W.ref~X))
#   PF.fit <- fitted(PF.out)
#   ## identify sufficient overlap
#   overlap.idx<-overlap(PF.fit)$idx
# 
#   W <- W[overlap.idx]
#   X <- as.matrix(X)
#   X <- X[overlap.idx,]
#   Y <- Y[overlap.idx]
#   analysisidx<-overlap.idx
# }
# if(Trimming==0){
#   analysisidx<-1:length(Y)
# }
# ## order the treatment increasingly
# if(1-is.unsorted(W)){
#   temp<-sort(W,index.return=TRUE)
#   temp<-list(x=temp)
#   temp$ix<-1:length(W)
# }
# if(is.unsorted(W)){
#   temp<-sort(W,index.return=TRUE)
# }
# W<-W[temp$ix]
# N=length(Y) # number of observations
# X<-as.matrix(X)
# X<-X[temp$ix,]
# Y<-Y[temp$ix]
# 
# 
# trtnumber<-length(unique(W)) # number of treatment levels
# trtlevels<-unique(W) # all treatment levels
# pertrtlevelnumber<-table(W) # number of observations by treatment level
# taunumber<-trtnumber*(trtnumber+1)/2-trtnumber  # number of pairwise treatment effects
# 
# 
# #PF modeling
# if(GPSM=="multinomiallogisticReg"){
#   W.ref <- relevel(as.factor(W),ref=1)
#   temp<-capture.output(PF.out <- multinom(W.ref~X))
#   PF.fit <- fitted(PF.out)
#   vcov_coeff <- vcov(PF.out)
# }
# if(GPSM=="ordinallogisticReg"){
#   PF.out <- polr(as.factor(W)~X)
#   PF.fit <- fitted(PF.out)
# }
# if(GPSM=="existing"){
#   #need to check the row sum of X is 1 - debug1
#   PF.fit <- X
# }
# 
# 
# tauestimate<-varestimate<-varestimateAI2012<-rep(NA,taunumber)
# meanw<-rep(NA,trtnumber)
# 
# Yiw<-matrix(NA,N,trtnumber) #Yiw is the full imputed data set
# Kiw<-sigsqiw<-matrix(NA,N,1)  #Kiw is vector of number of times unit i used as a match
# 
# Matchmat<-matrix(NA,N,trtnumber*2)
# cname<-c()
# for(kk in 1:trtnumber){
#   thistrt<-trtlevels[kk]
#   cname<-c(cname,c(paste(paste(paste("m",thistrt,sep=""),".",sep=""),1,sep=""),
#                    paste(paste(paste("m",thistrt,sep=""),".",sep=""),2,sep="")))
# }
# colnames(Matchmat)<-cname
# 
# 
# for(kk in 1:trtnumber){
#   thistrt<-trtlevels[kk]
#   if(kk==1){fromto<-1:pertrtlevelnumber[1]}
#   if(kk>1){fromto<-(1:pertrtlevelnumber[kk])+sum(pertrtlevelnumber[1:(kk-1)])}
#   W1<-W!=thistrt
#   out1 <- Match(Y=Y,Tr=W1,X=PF.fit[,kk],distance.tolerance=0,ties=FALSE,Weight=2)
#   mdata1<-out1$mdata
#   meanw[kk]<-weighted.mean(c(Y[which(W==thistrt)],mdata1$Y[which(mdata1$Tr==0)]),c(rep(1,length(which(W==thistrt))),out1$weights))
#   Kiw[fromto,1]<-table(factor(out1$index.control,levels=fromto))
#   Yiw[which(W==thistrt),kk]<- Y[which(W==thistrt)]
#   Yiw[which(W!=thistrt),kk]<-mdata1$Y[which(mdata1$Tr==0)]
# 
#   WW1<-W==thistrt
#   out11<-Match(Y=rep(Y[which(WW1)],times=2),Tr=rep(c(1,0),each=sum(WW1)),
#                X=c(PF.fit[which(WW1),kk],PF.fit[which(WW1),kk]),M=1,distance.tolerance=0,ties=FALSE,Weight=2,
#                restrict=matrix(c(1:sum(WW1),(1:sum(WW1))+sum(WW1),rep(-1,sum(WW1))),nrow=sum(WW1),ncol=3,byrow=FALSE))
# 
#   mdata11<-out11$mdata
#   temp11<-(mdata11$Y[which(mdata11$Tr==1)]-mdata11$Y[which(mdata11$Tr==0)])^2/2
#   sigsqiw[which(W==thistrt),1]<-temp11
# 
#   thiscnames<-c(paste(paste(paste("m",thistrt,sep=""),".",sep=""),1,sep=""),
#                 paste(paste(paste("m",thistrt,sep=""),".",sep=""),2,sep=""))
# 
#   # find two outsiders closest
#   findmatch1<-Match(Y=Y,Tr=W1,X=PF.fit[,kk],distance.tolerance=0,ties=FALSE,Weight=2,M=2)
#   Matchmat[unique(findmatch1$index.treated),thiscnames]<-matrix(findmatch1$index.control,ncol=2,byrow=TRUE)
#   # find one insider closest
#   out111<-Match(Y=rep(Y[which(WW1)],times=2),Tr=rep(c(0,1),each=sum(WW1)),
#                 X=c(PF.fit[which(WW1),kk],PF.fit[which(WW1),kk]),M=1,distance.tolerance=0,ties=FALSE,Weight=2,
#                 restrict=matrix(c(1:sum(WW1),(1:sum(WW1))+sum(WW1),rep(-1,sum(WW1))),nrow=sum(WW1),ncol=3,byrow=FALSE))
#   Matchmat[which(WW1),thiscnames]<-matrix(c(which(WW1),which(WW1)[out111$index.control]),ncol=2,byrow=FALSE)
# 
# }
# 
# cnt<-0
# cname1<-c()
# for(jj in 1:(trtnumber-1)){
#   for(kk in (jj+1):trtnumber){
#     cnt<-cnt+1
#     thistrt<-trtlevels[jj]
#     thattrt<-trtlevels[kk]
#     cname1<-c(cname1,paste(paste(paste(paste(paste("EY(",thattrt,sep=""),")",sep=""),"-EY(",sep=""),thistrt,sep=""),")",sep=""))
#     tauestimate[cnt]<-meanw[kk]-meanw[jj]
#     varestimate[cnt]<-mean((Yiw[,kk]-Yiw[,jj]-(meanw[kk]-meanw[jj]))^2)+mean((Kiw^2+Kiw)*sigsqiw*(W==thistrt | W==thattrt))
#   }
# }
# varestimate<-varestimate/N
# names(tauestimate)<-cname1
# names(varestimate)<-cname1
# names(varestimateAI2012)<-cname1
# 
# 
# if(GPSM=="multinomiallogisticReg"){
#   I.inv<-vcov_coeff
#   ## Adjustment term c'(I^-1)c
#   X<-as.matrix(X)
#   Cmat<-matrix(0,N,(dim(X)[2]+1)*(trtnumber-1))
#   Cvec<-matrix(0,trtnumber,(dim(X)[2]+1)*(trtnumber-1))
#   for(kkk in 1:trtnumber){
#     thistrt<-trtlevels[kkk]
#     thiscnames<-c(paste(paste(paste("m",thistrt,sep=""),".",sep=""),1,sep=""),
#                   paste(paste(paste("m",thistrt,sep=""),".",sep=""),2,sep=""))
#     Y11<-matrix(Y[Matchmat[,c(thiscnames)]],ncol=2,byrow=FALSE)
#     mY11<-apply(Y11,1,mean)
#     for(kk in 1:(trtnumber-1)){
#       for(jj in 1:(dim(X)[2]+1)){
#         if(jj==1){}
#         if(jj>1){
#           X11<-matrix(X[Matchmat[,c(thiscnames)],(jj-1)],ncol=2,byrow=FALSE)
#           mX11<-apply(X11,1,mean)
#           C1.X1Y<-apply((X11-mX11)*(Y11-mY11),1,sum)
#           if(kkk==(kk+1)){C1.X1Y<-C1.X1Y*(1-PF.fit[,kk+1])}
#           else if(kkk!=(kk+1))C1.X1Y<-C1.X1Y*(-PF.fit[,kk+1])
#           Cmat[,(dim(X)[2]+1)*(kk-1)+jj]<-C1.X1Y
#         }
#       }
#     }
#     Cvec[kkk,]<-apply(Cmat,2,mean)
#   }
# 
#   for(jj in 1:(trtnumber-1)){
#     for(kk in (jj+1):trtnumber){
#       thistrt<-trtlevels[jj]
#       thattrt<-trtlevels[kk]
#       cname1<-c(paste(paste(paste(paste(paste("EY(",thattrt,sep=""),")",sep=""),"-EY(",sep=""),thistrt,sep=""),")",sep=""))
#       varestimateAI2012[cname1]<-varestimate[cname1]-
#         t(Cvec[jj,]+Cvec[kk,])%*%vcov_coeff%*%(Cvec[jj,]+Cvec[kk,])
#     }
#   }
# }
# return(list(tauestimate=tauestimate,
#             varestimate=varestimate,
#             varestimateAI2012=varestimateAI2012,
#             analysisidx=analysisidx))
