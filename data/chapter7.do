********************
*** Programmer: Lucia Petito
*** Date: November 2, 2016
*** Purpose: Clean teensex.dta and reproduce slides from Chapter 7
********************

*** Set working directory
cd "insert/working/directory/here"
*** Load data
use "teensex.dta"

*** Make variable ct for each observation
sort eid day
by eid: gen ct = _n

*** Make variable maxct 
by eid: egen maxct = max(ct)

*** Generate observation count
gen cattot = .
replace cattot = 1 if maxct <= 10
replace cattot = 2 if maxct <= 20 & maxct >= 11
replace cattot = 3 if maxct <= 30 & maxct >=21
replace cattot = 4 if maxct >= 31

***  labels
label define yesno 0 "no" 1 "yes"
label define obsct 1 "1-10" 2 "11-20" 3 "21-30" 4 ">30"

*** Label variables
label values sx24hrs yesno
label values drgalcoh yesno
label values cattot obsct

*** Slide 6
tab cattot if ct==1
tab cattot sx24hrs, r

*** Slide 7
tab cattot drgalcoh, r

*** Slide 8: odds ratios
cs sx24hrs drgalcoh, or

*** Slide 9: random intercept
xtmelogit sx24hrs drgalcoh || eid:, stddev
lincom drgalcoh, or

*** Other code that provides the same results:
meqrlogit sx24hrs drgalcoh || eid: 
melogit sx24hrs drgalcoh || eid: 
*** The melogit command is almost the same, but doesn't work so well for 
*** variance estimates close to the boundary, so safer to use meqrlogit
*** Note both the melogit and meqrlogit commands return variance of the random 
*** effect, not SD
*** The lincom postestimation command works with melogit and meqrlogit

*** Slide 10
estat ic
estat icc

*** Slide 11

*** Slide 12
*** Remember to set seed so when you do it again you'll get the same numbers
set seed 123456
bootstrap, cluster(eid) idcluster(newid) group(eid) reps(50): meqrlogit sx24hrs drgalcoh || newid:,  stddeviations

*** Slide 13-14: random slope
meqrlogit sx24hrs drgalcoh || eid: drgalcoh,  stddeviations cov(unstruct)

*** Slide 15
estat ic
estat icc

*** Slide 17: IQR for OR from random slope model

disp "(" exp(.3287796-invnormal(0.75)*.7279197) " , " exp(.3287796+invnormal(0.75)*.7279197) " )"

*** Slide 18: generate cumulative average and the lag

sort eid day
capture drop cumsum cumprop cumlag
gen cumsum = 0
replace cumsum = sx24hrs if ct==1
by eid: replace cumsum = cumsum[_n-1]+sx24hrs if ct > 1
gen cumprop = cumsum/ct
by eid: gen cumlag = cumprop[_n-1] if ct > 1

*** Slide 19: random slope model adjusting for outcome at previous time
meqrlogit sx24hrs drgalcoh cumlag || eid: drgalcoh, stddeviations cov(unstruct)
lincom drgalcoh, or

*** Slide 20: model comparison 
meqrlogit sx24hrs drgalcoh cumlag || eid: drgalcoh, stddeviations cov(unstruct)
est store A
meqrlogit sx24hrs drgalcoh || eid: drgalcoh, stddeviations cov(unstruct)
est store B
lrtest A B

*** Slide 22: 3 level random intercept model
* Load new data
use "village.dta", clear
*This next line needs to be edited
meqrlogit diarrhea || vilid: || hhid:,  intpoints(5) 

*** Slide 31: get MH OR estimate
cs sx24hrs drgalcoh, by(eid) or

*** Slide 32: get OR estimate from conditional logistic regression
clogit sx24hrs drgalcoh, or group(eid) 






