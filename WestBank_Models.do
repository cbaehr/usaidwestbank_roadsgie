
*Clear memory
clear

*Set local macros
global project "/Users/careyglenn/Box Sync/usaidwestbank_roadsgie"

*Import file

import delimited "/Users/rbtrichler/Box Sync/usaidwestbank_roadsgie/Data/wb_panel_slim_750m.csv", clear

destring dist_trt2, force replace


* Regressions

*Model 1
reghdfe viirs trt1 maxl, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, replace ctitle(Model 1) addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 2 adds linear month
reghdfe viirs trt1 maxl month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append ctitle(Model 2) keep(trt1 maxl month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 3 adds month fixed effects
reghdfe viirs trt1 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append ctitle(Model 3) keep(trt1 maxl) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 4 adds interaction with distance
reghdfe viirs c.trt1##c.dist_trt1 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append ctitle(Model 4) keep(trt1 maxl c.trt1##c.dist_trt1) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 5 adds second treatment, no distance interaction
reghdfe viirs c.trt1##c.dist_trt1 trt2 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append ctitle(Model 5) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

reghdfe viirs trt1 trt2 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
reghdfe viirs trt1 trt2 trt3 maxl i.month, cluster(pcbs_co month) absorb(cell_id)


*Model 5 adds second treatment and distance interaction
reghdfe viirs c.trt1##c.dist_trt1 c.trt2##c.dist_trt2 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append ctitle(Model 6) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 6 adds third treatment, no distance interaction
reghdfe viirs c.trt1##c.dist_trt1 trt2 trt3 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append ctitle(Model 7) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", N) 


*Model 7 adds fourth treatment, no distance interaction
reghdfe viirs c.trt1##c.dist_trt1 trt2 trt3 trt4 maxl i.month, cluster(pcbs_co month) absorb(cell_id)






bys cell_id: g viirs_at_m0 = viirs[1]
reghdfe viirs 1.trt1##c.viirs_at_m0 i.month maxl, cluster(pcbs_co month) absorb(cell_id)
