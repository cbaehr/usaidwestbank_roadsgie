
library(rgdal)
library(sf)
library(sp)
library(maptools)
library(raster)

# load in polygon grid
vector <- st_read("/Users/christianbaehr/Github/usaidwestbank_roadsgie/Data/wb_cells_shp.shp",
                  stringsAsFactors=F)
# retain only ID info
vector <- vector[, c("SP_ID", "cell_id")]

# convert grid to Spatial Polygons
vector <- as_Spatial(vector)

# load in GHSL raster data
raster <- stack("/Users/christianbaehr/Box Sync/usaidwestbank_roadsgie/Data/ghsl.tif")

# takes a long time to run
# computing zonal statistics. Finding the mean of all GHSL cells falling within
# each WB grid cell. Only raster cells whose centroid is within a grid cell are considered
ghsl <- extract(raster, vector, fun=mean, na.rm=TRUE, df=TRUE)

# merging GHSL information with cell IDs
ghsl <- merge(vector, ghsl, by.x="SP_ID", by.y="ID")

###

# load in main WB panel
panel <- read.csv("/Users/christianbaehr/Box Sync/usaidwestbank_roadsgie/Data/wb_panel_slim_750m.csv",
                 stringsAsFactors = F)

sum(panel$cell_id %in% ghsl$cell_id)

# merge GHSL data with main WB panel
panel <- merge(panel, ghsl[, c("cell_id", "ghsl")], by="cell_id")

write.csv(panel, 
          "/Users/christianbaehr/Box Sync/usaidwestbank_roadsgie/Data/wb_panel_slim_750m_GHSL.csv",
          row.names = F)


