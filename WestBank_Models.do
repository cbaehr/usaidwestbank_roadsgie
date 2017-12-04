*Clear memory
clear

*Set local macros
global project "/Users/careyglenn/Box Sync/usaidwestbank_roadsgie"
global project "/Users/rbtrichler/Box Sync/usaidwestbank_roadsgie"

*Import file
import delimited "$project/Data/wb_panel_slim_750m.csv", clear
destring dist_trt2, force replace
destring viirs, force replace

*Generate categorical variables
*Binned treatment distance
egen dist_trt1cat = cut(dist_trt1), at (0, 1000, 2000, 3000, 4000, 5000, 6000) icodes
*Binned viirs at month 0 
bys cell_id: g viirs_at_m0 = viirs[1]
egen viirscat = cut(viirs_at_m0), group(4) icodes
*sum viirs_at_m0, detail
*sum viirs_at_m0 if viirscat2==0
*sum viirs_at_m0 if viirscat2==3
*Generate minimum distance to road out of trt1 and trt2
egen mindist = rowmin(dist_trt1-dist_trt2)

* Regressions

*Model 1, with linear month
reghdfe viirs trt1 maxl month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 2, adds month fixed effects
reghdfe viirs trt1 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 3, adds trt1 interaction with distance
reghdfe viirs c.trt1##c.dist_trt1 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 4, adds binned distance interaction with trt1
reghdfe viirs c.trt1##c.dist_trt1cat maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 5, adds second treatment, no distance interaction
reghdfe viirs trt1 trt2 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 6, adds trt1 interaction w mindist, trt2 interaction w mindist
reghdfe viirs maxl c.trt1##c.mindist trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 7, adds third treatment, no distance interaction
reghdfe viirs trt1 trt2 trt3 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 8, or Model 1 less maxl
reghdfe viirs trt1 month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 9, or Model 2 less maxl
reghdfe viirs trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 10, or Model 3 less maxl
reghdfe viirs c.trt1##c.dist_trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 11, or Model 4 less maxl
reghdfe viirs c.trt1##c.dist_trt1cat i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 12, or Model 5 less maxl
reghdfe viirs trt1 trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 13, or Model 6 less maxl
reghdfe viirs c.trt1##c.mindist trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 14, or Model 7 less maxl
reghdfe viirs trt1 trt2 trt3 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 


*remake models above w/o maxl, myreg2.doc
*change myreg to a box sync (put a file path in there in quotes?)
*remake all above in other do file (new variabes, new models)
*pretty summary statistics table, couple of tables, just viirs and trt1


bys cell_id: g viirs_at_m0 = viirs[1]
reghdfe viirs 1.trt1##c.viirs_at_m0 i.month maxl, cluster(pcbs_co month) absorb(cell_id)



* -------
* Regression Models Scratch
* -------

*Model 5 adds second treatment and distance interaction
reghdfe viirs c.trt1##c.dist_trt1 c.trt2##c.dist_trt2 maxl i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*basic model with no time effects
reghdfe viirs trt1 maxl, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)






