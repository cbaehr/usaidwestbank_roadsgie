
*Clear memory
clear

*Set local macros
global project "/Users/careyglenn/Box Sync/usaidwestbank_roadsgie"
global project "/Users/rbtrichler/Box Sync/usaidwestbank_roadsgie"

*Import file
import delimited "$project/Data/wb_panel_750m_near.csv", clear
destring dist_trt2, force replace
destring dist_trt3, force replace
destring viirs, force replace

* Generate Variables

*Binned treatment 1 distance
egen dist_trt_near_cat = cut(dist_trt_nearest), at (0, 1000, 2000, 3000, 4000, 5000, 6000) icodes




** REGRESSIONS
** Only Ever Treated Cells

* earliest treatment, 
reghdfe viirs trt1 i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myregnear.doc, replace drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

* add second treatment 
reghdfe viirs trt1 trt2 i.month,cluster (pcbs_co month) absorb(cell_id) 
outreg2 using myregnear.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

* add third treatment
reghdfe viirs trt1 trt2 trt3 i.month,cluster (pcbs_co month) absorb(cell_id) 
outreg2 using myregnear.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

* nearest treatment
reghdfe viirs trt_near i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myregnear.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

* interacts nearest treatment with continuous distance
reghdfe viirs trt_near#c.dist_trt_nearest i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myregnear.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

* interacts nearest treatment with categorical distance (0-5)
reghdfe viirs trt_near#ibn.dist_trt_near_cat i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myregnear.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)
