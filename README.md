#usaidwestbank_roadsgie

##data files that are used in this project - where they came from

INPIICSV_RoadsShapefile_Reconcile _comments_clean.xlsx - from Google Drive, provided by USAID Mission
poly_raster_data_merge.shp - from Tyler (Slack in usaid_westbank_gie)

all data in Data_Old - from Tyler (Slack in usaid_westbank_gie)

buffer.shp - from spatial_join_apply
roads_merged.shp - from spatial_join_apply
spatial_join.shp - from spatial_join_apply

poly_raster_data_merge2.shp - from Tyler (Slack in usaid_westbank_gie)

merge_westbank_cells.csv - from Seth (Slack in usaid_westbank_gie)

merge_westbank_cells_monthlyndvi.csv - from Seth (Slack in usaid_westbank_gie); extract of monthly ndvi values

cell_dist_extract - from Seth (Slack in usaid_westbank_gie); gives distance between each cell and associated road segment

## Shapefiles (folder)


INPIIRoadsProjects_Line_modified.shp -- file of 59 INP II road segments (some improved; some yet to be improved) provided by Peter Lister from Black and Veatch (implementing partner); obtained from shared Google Drive folder

Localities_Classifications.shp - provides municipality admin boundaries from the Vulnerability Assessment published in June 2011; provided by the Mission; obtained from shared Google Drive folder

Localities_Classifications_EPSG4326.shp - Localities_Classifications reprojected to EPSG4326 to do spatial join with poly_raster_data_merge2

cells_localities_join.shp - joins municipality info from localities_classifications_epsg4326 with poly_raster_data_merge2, done manually in QGIS, to get municipality info for each cell

WestBankBorder.shp - provides admin border of entire West Bank area; manually clipped from countries.shp file provided by the Mission and obtained from shared Google Drive folder; used to clip grid cells that fall within 5km road buffer but outside of West Bank admin boundaries

buffercells_nonWestBank.shp - provides cell ids for cells that fall within 5km road buffer but outside of West Bank admin boundaries; these cells were part of the extracts but should be excluded from analysis and will use cell ids from this file to drop them before creation of the final dataset; created manually in QGIS from poly_raster_data_merge2.shp and WestBankBorder.shp




