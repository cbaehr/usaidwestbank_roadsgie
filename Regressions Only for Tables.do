*Carey keeping track of everything for Rachel tables
*If anything is strange within these particular regressions, default to the 
*original do file listed next to "Use" i.e. Use WestBank_Models.do

***Table 2 in the Final Report***
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

***Table 3 in the Final Report***
*Use WestBank_Models_2km.do
***********************
*Final model 1
reghdfe viirs trt1 month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2000.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Final model 2
reghdfe viirs trt1 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2000.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 3
reghdfe viirs trt1 trt2_2000 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2000.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 4
reghdfe viirs trt1 trt2_2000 trt3_2000 i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg2000.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

*Final model 5
reghdfe viirs trt1##viirscat i.month, cluster (pcbs_co month) absorb(cell_id)
outreg2 using myreg2000.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y)

***Table 4 in the Final Report***
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

***Table 5 in Final Report***
*Use WestBank_Models.do
***********************
*Model 1, with linear month
reghdfe viirs trt1_6mp month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg3.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 2, adds month fixed effects
reghdfe viirs trt1_6mp i.month, cluster(pcbs_co month) absorb(cell_id)
outreg2 using myreg3.doc, append drop(i.month) addtext ("Grid cell FEs", Y, "Month FEs", Y) 

***Table 6 in Final Report***
*Use WestBank_Models_DMSP.do
***********************
*Model 1, with linear month
reghdfe dmsp trt1_5yp year, cluster(pcbs_co year) absorb(cell_id)
outreg2 using myreg5.doc, replace addtext ("Grid cell FEs", Y, "Month FEs", N)

*Model 2, adds month fixed effects
reghdfe dmsp trt1_5yp i.year, cluster(pcbs_co year) absorb(cell_id)
outreg2 using myreg5.doc, append drop(i.year) addtext ("Grid cell FEs", Y, "Month FEs", Y) 
