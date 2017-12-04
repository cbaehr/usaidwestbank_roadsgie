*Clear memory
clear

*Set macros
global project "/Users/careyglenn/Box Sync/usaidwestbank_roadsgie"
global project "/Users/rbtrichler/Box Sync/usaidwestbank_roadsgie"

*Import file
import delimited "$project/Data/wb_panel_slim_1000m.csv", clear
destring dist_trt2, force replace

*Generate categorical variables
*Binned treatment distance
egen dist_trt1cat = cut(dist_trt1), at (0, 1000, 2000, 3000, 4000, 5000, 6000) icodes
*Binned viirs at month 0 
bys cell_id: g viirs_at_m0 = viirs[1]
egen viirscat = cut(viirs_at_m0), group(4) icodes

* Regressions

*Model 1
reghdfe viirs trt1 maxl, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, replace ctitle(Model 1) addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 2 adds linear month
reghdfe viirs trt1 maxl month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append ctitle(Model 2) keep(trt1 maxl month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 3 adds month fixed effects
reghdfe viirs trt1 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append ctitle(Model 3) keep(trt1 maxl) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 4 adds interaction with distance
reghdfe viirs c.trt1##c.dist_trt1 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append ctitle(Model 4) keep(trt1 maxl c.trt1##c.dist_trt1) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 5 adds second treatment, no distance interaction
reghdfe viirs c.trt1##c.dist_trt1 trt2 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append ctitle(Model 5) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 6 adds second treatment and distance interaction
reghdfe viirs c.trt1##c.dist_trt1 c.trt2##c.dist_trt2 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append ctitle(Model 6) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 7 adds third treatment, no distance interaction
reghdfe viirs c.trt1##c.dist_trt1 trt2 trt3 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append ctitle(Model 7) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 7 adds fourth treatment, no distance interaction
reghdfe viirs c.trt1##c.dist_trt1 trt2 trt3 trt4 maxl i.month, cluster(pcbs_co month) absorb(cell_id)






bys cell_id: g viirs_at_m0 = viirs[1]
reghdfe viirs 1.trt1##c.viirs_at_m0 maxl, cluster(pcbs_co month) absorb(cell_id)
