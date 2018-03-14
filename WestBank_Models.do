*Clear memory
clear

*Set local macros
global project "/Users/careyglenn/Box Sync/usaidwestbank_roadsgie"
global project "/Users/rbtrichler/Box Sync/usaidwestbank_roadsgie"

*Import file
import delimited "$project/Data/wb_panel_slim_750m.csv", clear
destring dist_trt2, force replace
destring dist_trt3, force replace
destring viirs, force replace

* Generate Variables

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

reghdfe viirs trt1_only#dist_trt1cat_only i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myreg1a.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)
reghdfe viirs trt1_only#c.dist_trt1_only i.month, cluster (pcbs_co month) absorb(cell_id)

* Regressions
**MAIN REGRESSION MODELS USED FOR MISSION PRESENTATION**

*Final model 1
reghdfe viirs trt1 month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Final model 2
reghdfe viirs trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
est sto model2
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 3
reghdfe viirs trt1 trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
est sto model3
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 4
reghdfe viirs trt1 trt2 trt3 i.month, cluster(pcbs_co month) absorb(cell_id)
est sto model4
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 5
reghdfe viirs trt1##viirscat i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

* Coefficient plots for main regression models for WB Final Report
coefplot (model2, label (Model 2)) (model3, label(Model 3)) (model4, label (Model 4)), ///
keep(trt1 trt2 trt3) xline(0) ///
coeflabels(trt1="Treatment 1" trt2="Treatment 2" trt3="Treatment 3")

** Recreate regression models with 6 months prior placebo treatment variable

*Model 1, with linear month
reghdfe viirs trt1_6mp month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg3.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 2, adds month fixed effects
reghdfe viirs trt1_6mp i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg3.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 3, adds trt1 interaction with distance
reghdfe viirs c.trt1_6mp##c.dist_trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg3.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 4, adds binned distance interaction with trt1
reghdfe viirs c.trt1_6mp##dist_trt1cat i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg3.doc, append drop(i.month dist_tr1cat) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

* trt1 with viirs at month 0 interactions
reghdfe viirs c.trt1_6mp##viirscat i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myreg3.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

* trt1 interact with governorate
*reghdfe viirs c.trt1_6mp##c.governorat maxl i.month, cluster (pcbs_co month) absorb(cellid)
*outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

** Recreate regression models without maxl
*Model 10, or Model 3 less maxl
reghdfe viirs c.trt1##c.dist_trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 11, or Model 4 less maxl
reghdfe viirs c.trt1##dist_trt1cat i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 13, or Model 6 less maxl
reghdfe viirs c.trt1##c.mindist trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

* adds interaction with viirscat
reghdfe viirs trt1##viirscat i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myreg4.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

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


*Model 8, or Model 1 less maxl
reghdfe viirs trt1 month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg4.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 9, or Model 2 less maxl
reghdfe viirs trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg4.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 12, or Model 5 less maxl
reghdfe viirs trt1 trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg4.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 14, or Model 7 less maxl
reghdfe viirs trt1 trt2 trt3 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg4.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 
