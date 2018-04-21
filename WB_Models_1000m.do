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
*old code
*bys cell_id: g viirs_at_m0 = viirs[1]
*new code
bys cell_id (month): gen viirs_at_m0=viirs[1]
egen viirscat = cut(viirs_at_m0), group(4) icodes
gen viirs_sub=viirs-viirs_at_m0
*check variable creation, viirs_sub for 201204 should be equal to 0
sum viirs_sub
sum viirs_sub if month==201204

*Generate minimum distance to road out of trt1 and trt2
egen mindist = rowmin(dist_trt1-dist_trt2)

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

*Model 8 adds fourth treatment, no distance interaction
reghdfe viirs c.trt1##c.dist_trt1 trt2 trt3 trt4 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append ctitle(Model 8) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

** Drop out maxl (ndvi) from models

*Model 9, or Model 1 less maxl
reghdfe viirs trt1 month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000two.doc, replace ctitle(Model 1) addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 10, or Model 2 less maxl
reghdfe viirs trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000two.doc, append ctitle(Model 2) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 11, or Model 3 less maxl
reghdfe viirs trt1 trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000two.doc, append ctitle(Model 3) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 12, or Model 4 less maxl
reghdfe viirs trt1 trt2 trt3 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000two.doc, append ctitle(Model 4) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 13, or Model 5 less maxl
*reghdfe viirs c.trt1##c.dist_trt1 trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
reghdfe viirs trt1##viirscat i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myreg1000two.doc, append ctitle(Model 5) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 


* ----------
* Regression Models Scratch
* ----------

*Model 14, or Model 6 less maxl
reghdfe viirs c.trt1##c.dist_trt1 c.trt2##c.dist_trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000two.doc, append ctitle(Model 6) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 15, or Model 7 less maxl
reghdfe viirs c.trt1##c.dist_trt1 trt2 trt3 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000two.doc, append ctitle(Model 7) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 16, or Model 8 less maxl
reghdfe viirs c.trt1##c.dist_trt1 trt2 trt3 trt4 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000two.doc, append ctitle(Model 8) drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", N) 


* -------
* Scratch 
* -------

bys cell_id: g viirs_at_m0 = viirs[1]
reghdfe viirs 1.trt1##c.viirs_at_m0 maxl, cluster(pcbs_co month) absorb(cell_id)
