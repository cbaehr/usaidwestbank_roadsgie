*Carey keeping track of everything for Rachel tables

***Table 1***
*Main models for West Bank Gaza presentation
*Use WestBank_Models.do
***********************
*Final model 1
reghdfe viirs trt1 month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Final model 2
reghdfe viirs trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 3
reghdfe viirs trt1 trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 4
reghdfe viirs trt1 trt2 trt3 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 5
reghdfe viirs trt1##viirscat i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myreg.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

***Table 2***
*Use WB_Models_1000m.do
***********************
*Model 1
reghdfe viirs trt1 month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append keep(trt1 maxl month) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 2
reghdfe viirs trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append keep(trt1 maxl) addtext ("Grid cell FEs", Y, "Month FEs", N) 

*Model 3
reghdfe viirs trt1 trt2 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append keep(trt1 maxl c.trt1##c.dist_trt1) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 4
reghdfe viirs trt1 trt2 trt3 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 5
reghdfe viirs trt1##viirscat i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg1000.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

***Table 3***
*Use WestBank_Models.do
***********************
*Model 1, with linear month
reghdfe viirs trt1_6mp month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg3.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 2, adds month fixed effects
reghdfe viirs trt1_6mp i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg3.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

* trt1 with viirs at month 0 interactions
reghdfe viirs trt1_6mp##viirscat i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myreg3.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

***Table 4***
*Use WestBank_Models_DMSP.do
***********************
*Model 1, with linear month
reghdfe dmsp trt1_5yp year, cluster(pcbs_co year) absorb(cell_id)
outreg2 using myreg5.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 2, adds month fixed effects
reghdfe dmsp trt1_5yp i.year, cluster(pcbs_co year) absorb(cell_id)
outreg2 using myreg5.doc, append drop(i.year) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Model 4, adds binned distance interaction with trt1
reghdfe dmsp trt1_5yp##dist_trt1cat i.year, cluster(pcbs_co year) absorb(cell_id)
outreg2 using myreg5.doc, append drop(i.year) addtext ("Grid cell FEs", Y, "Month FEs", Y) 
