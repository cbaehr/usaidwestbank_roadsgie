*Clear memory
clear

*Set local macros
*global project "/Users/careyglenn/Box Sync/usaidwestbank_roadsgie"
*global project "/Users/rbtrichler/Box Sync/usaidwestbank_roadsgie"
global project "/Users/christianbaehr/Box Sync/usaidwestbank_roadsgie"

*Import file
import delimited "$project/Data/wb_panel_slim_750m.csv", clear

replace viirs = "." if viirs=="NA"
destring viirs, replace
* destring viirs, force replace

* time to treatment

tostring month, replace

gen year_actual = substr(month, 1, 4)
gen month_actual = substr(month, 5, 6)

destring year_actual, replace
destring month_actual, replace

drop if date_trt1_y==9999

destring month, replace


gen time_to_trt = ((year_actual - date_trt1_y) * 12) + (month_actual - date_trt1_m)

***

* eststo: su date_trt1_6mp_ym, by(date_trt1_6mp_ym)

* drop if date_trt1_6mp_ym == 999807

* hist date_trt1_y

* hist date_trt1

* gen m_onset = mofd(date_trt1)
* histogram m_onset, width(1) frequency  ///
* xlabel(, format(%tmm_y))


***

replace time_to_trt = 24 if time_to_trt > 24 & !missing(time_to_trt)
replace time_to_trt = -24 if time_to_trt < -24 & !missing(time_to_trt)

replace time_to_trt = time_to_trt + 24

eststo clear

* eststo: reghdfe viirs ib24.time_to_trt, cluster(pcbs_co month) absorb(cell_id month)
eststo: reghdfe viirs ib24.time_to_trt, cluster(pcbs_co month) absorb(cell_id month)

esttab using "/Users/christianbaehr/Desktop/example.csv", replace wide plain ci

***

eststo clear

* eststo: reghdfe viirs ib24.time_to_trt, cluster(pcbs_co month) absorb(cell_id month)
cap encode governorat, gen(gov)
eststo: reghdfe viirs ib24.time_to_trt c.month#i.gov, cluster(pcbs_co month) absorb(cell_id month)
* c.year#i.province_number
esttab using "/Users/christianbaehr/Desktop/example.csv", replace wide plain ci

***

*

tostring month, gen(month_str)
encode month_str, gen(month_count)

gen temp1 = (trt1>0)
gen temp2 = temp1*month_count
replace temp2 = . if temp2==0
egen temp3 = min(temp2), by(cell_id)

gen time_to_trt2 = month_count - temp3

gen test = time_to_trt==time_to_trt2







***************************************

* 1km
eststo clear

eststo: reghdfe viirs ib24.time_to_trt if dist_trt1<=1000, cluster(pcbs_co month) absorb(cell_id month)

esttab using "/Users/christianbaehr/Desktop/eventstudy_1km.csv", replace wide plain ci


* 2km
eststo clear

eststo: reghdfe viirs ib24.time_to_trt if dist_trt1<=2000 & dist_trt1>1000, cluster(pcbs_co month) absorb(cell_id month)

esttab using "/Users/christianbaehr/Desktop/eventstudy_2km.csv", replace wide plain ci


* 3km
eststo clear

eststo: reghdfe viirs ib24.time_to_trt if dist_trt1<=3000 & dist_trt1>2000, cluster(pcbs_co month) absorb(cell_id month)

esttab using "/Users/christianbaehr/Desktop/eventstudy_3km.csv", replace wide plain ci


sample 10, count

***




eststo clear

* eststo: reghdfe viirs ib24.time_to_trt, cluster(pcbs_co month) absorb(cell_id month)

eststo: reghdfe viirs i.time_to_trt, cluster(pcbs_co month) absorb(cell_id month)


esttab using "/Users/christianbaehr/Desktop/example.csv", replace wide plain ci

***

eststo clear

* eststo: reghdfe viirs ib24.time_to_trt, cluster(pcbs_co month) absorb(cell_id month)

eststo: reghdfe viirs i.time_to_trt  if dist_trt1<=1000, cluster(pcbs_co month) absorb(cell_id month)


esttab using "/Users/christianbaehr/Desktop/eventstudy_1km.csv", replace wide plain ci


***




eststo clear

* eststo: reghdfe viirs ib24.time_to_trt, cluster(pcbs_co month) absorb(cell_id month)

eststo: reghdfe viirs i.time_to_trt  if dist_trt1<=2000 & dist_trt1>1000, cluster(pcbs_co month) absorb(cell_id month)


esttab using "/Users/christianbaehr/Desktop/eventstudy_2km.csv", replace wide plain ci


***


eststo clear

* eststo: reghdfe viirs ib24.time_to_trt, cluster(pcbs_co month) absorb(cell_id month)

eststo: reghdfe viirs i.time_to_trt  if dist_trt1<=3000 & dist_trt1>2000, cluster(pcbs_co month) absorb(cell_id month)


esttab using "/Users/christianbaehr/Desktop/eventstudy_3km.csv", replace wide plain ci












