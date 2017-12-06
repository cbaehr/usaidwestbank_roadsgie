*Clear memory
clear

*Set local macros
global project "/Users/careyglenn/Box Sync/usaidwestbank_roadsgie"
global project "/Users/rbtrichler/Box Sync/usaidwestbank_roadsgie"

*Import file
import delimited "$project/Data/wb_panel_750m_5yp.csv", clear
destring dist_trt2, force replace


*Generate categorical variables
*Binned treatment distance
egen dist_trt1cat = cut(dist_trt1), at (0, 1000, 2000, 3000, 4000, 5000, 6000) icodes


* Regressions

*Model 1, with linear month
reghdfe dmsp trt1_5yp year, cluster(pcbs_co year) absorb(cell_id)
outreg2 using myreg5.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 2, adds month fixed effects
reghdfe dmsp trt1_5yp i.year, cluster(pcbs_co year) absorb(cell_id)
outreg2 using myreg5.doc, append drop(i.year) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 3, adds trt1 interaction with distance
reghdfe dmsp c.trt1_5yp##c.dist_trt1 i.year, cluster(pcbs_co year) absorb(cell_id)
outreg2 using myreg5.doc, append drop(i.year) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 4, adds binned distance interaction with trt1
reghdfe dmsp c.trt1_5yp##c.dist_trt1cat i.year, cluster(pcbs_co year) absorb(cell_id)
outreg2 using myreg5.doc, append drop(i.year) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

* trt1 with viirs at month 0 interactions
*reghdfe viirs c.trt1_5yp##c.viirscat maxl i.month, cluster (pcbs_co month) absorb(cellid)
*outreg2 using myreg5.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

* trt1 interact with governorate
*reghdfe viirs c.trt1##c.governorat maxl i.month, cluster (pcbs_co month) absorb(cellid)
*outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)
