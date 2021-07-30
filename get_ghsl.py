

import pandas as pd
import rasterstats as rs
# vector = "/sciclone/aiddata10/REU/geo/data/boundaries/geoboundaries/1_3_3/CAN_ADM1/CAN_ADM1.geojson"
# vector = "/Users/christianbaehr/Github/usaidwestbank_roadsgie/Data/wb_cells_shp.shp"

vector = "/Users/christianbaehr/Desktop/pre_ghsl_cells.geojson"
# raster = "/sciclone/aiddata10/REU/geo/data/rasters/udel_climate/precip_2014_v4.01/yearly/mean/precip_1904_mean.tif"
raster = "/Users/christianbaehr/Box Sync/usaidwestbank_roadsgie/Data/ghsl.tif"

# output = "/sciclone/home10/aiddatageo/ex_out.csv"
output = "/Users/christianbaehr/Desktop/ex_out.csv"

tmp_extract_type = "mean"
all_touched = True
stats = rs.zonal_stats(
    vector, 
    raster,
    geojson_out=True,
    prefix="exfield_",
    stats=tmp_extract_type,
    all_touched=all_touched)
# to csv
x = [i['properties'] for i in stats]
out = pd.DataFrame(x)
out.to_csv(output, index=False, encoding='utf-8')
