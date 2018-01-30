**2km not 5km do file

*Clear memory
clear

*Set local macros
global project "/Users/careyglenn/Box Sync/usaidwestbank_roadsgie"
*global project "/Users/rbtrichler/Box Sync/usaidwestbank_roadsgie"

*Import file
import delimited "$project/Data/wb_panel_full_750m.csv", clear
destring dist_trt2, force replace
destring dist_trt3, force replace
destring viirs, force replace

*Binned treatment 1 distance
egen dist_trt1cat = cut(dist_trt1), at (0, 1000, 2000, 3000, 4000, 5000, 6000) icodes

*Binned viirs at month 0 
bys month cell_id: g viirs_at_m0 = viirs[1]
egen viirscat = cut(viirs_at_m0), group(4) icodes
*sum viirs_at_m0, detail
*sum viirs_at_m0 if viirscat2==0
*sum viirs_at_m0 if viirscat2==3
*Generate minimum distance to road out of trt1 and trt2
egen mindist = rowmin(dist_trt1-dist_trt2)

*Create trt1 and dist1 only for cells that fall into only 1 buffer (trt1, but no trt2 or trt3)
*identify cells that fall into only 1 buffer
gen buffer1_only=0
replace buffer1_only=1 if date_trt2=="NA"
*create new trt1 and dist_trt1 variable that is missing if buffer1_only=0 (falls into multiple buffers)
* allows us to look only at subset of 1 buffer cells for regressions
gen trt1_only=.
replace trt1_only=trt1 if buffer1_only==1
gen dist_trt1_only=.
replace dist_trt1_only=dist_trt1 if buffer1_only==1
egen dist_trt1cat_only = cut(dist_trt1_only), at (0, 1000, 2000, 3000, 4000, 5000, 6000) icodes

gen pop_2010_int=int(pop_2010)
***************************

*Final model 1
reghdfe viirs pop_2010_int##trt1 month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myregpop2010.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Final model 2
reghdfe viirs pop_2010_int##trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myregpop2010.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 3
reghdfe viirs pop_2010_int##trt1 trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myregpop2010.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 4
reghdfe viirs pop_2010_int##trt1 trt2 trt3 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myregpop2010.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 5
reghdfe viirs trt1##viirscat i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myregpop2010.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)
