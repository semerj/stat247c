clear
**n=100
set obs 100
gen X1=5*runiform()
gen X2=0.5*X1+rnormal(0,2)
scalar b0 = -2
scalar b1 = 2.0
scalar b2 = -2.25
gen logitPY = b0+b1*X1+b2*X2
gen PY=1/(1+exp(-logitPY))
gen Y = rbinomial(1, PY)
display 1/(1+exp(-(b0+b1*0+b2*1)))
****** Lowess plots (smooth) on logit scalee
lowess Y X1, gen(logitPX1) nograph logit
lowess Y X2, gen(logitPX2) nograph logit
*** transform to probability
gen PX1 = 1/(1+exp(-logitPX1))
gen PX2 = 1/(1+exp(-logitPX2))
sort X1
scatter PX1 X1, ms(i) c(l) || scatter Y X1, ms(o) c(i) ytitle("P(Y=1|X1=x1)") xtitle("x1") legend(off) ti("Estimate P(Y=1|X1=x1)") saving(graph1, replace) 
sort X2
scatter PX2 X2, ms(i) c(l) || scatter Y X2, ms(o) c(i) ytitle("P(Y=1|X2=x2)") xtitle("x2") legend(off) ti("Estimate P(Y=1|X2=x2)") saving(graph2, replace) 
graph combine graph1.gph graph2.gph, ti("Lowess Smooths") ycommon 
** Run Logistic Regression
logit Y X1 X2
** Get odds ratios from model
matrix b = get(_b)
matrix list b
display 1/(1+exp(-(b[1,3]+b[1,1]*0+b[1,2]*1)))
lincom 0.5*X1, or
logit, or
**n=500
clear
set obs 500
gen X1=5*runiform()
gen X2=0.5*X1+rnormal(0,2)
scalar b0 = -2
scalar b1 = 2.0
scalar b2 = -2.25
gen logitPY = b0+b1*X1+b2*X2
gen PY=1/(1+exp(-logitPY))
gen Y = rbinomial(1, PY)
****** Lowess plots (smooth) on logit scalee
lowess Y X1, gen(logitPX1) nograph logit
lowess Y X2, gen(logitPX2) nograph logit
*** transform to probability
gen PX1 = 1/(1+exp(-logitPX1))
gen PX2 = 1/(1+exp(-logitPX2))
sort X1
scatter PX1 X1, ms(i) c(l) || scatter Y X1, ms(o) c(i) ytitle("P(Y=1|X1=x1)") xtitle("x1") legend(off) ti("Estimate P(Y=1|X1=x1)") saving(graph1, replace) 
sort X2
scatter PX2 X2, ms(i) c(l) || scatter Y X2, ms(o) c(i) ytitle("P(Y=1|X2=x2)") xtitle("x2") legend(off) ti("Estimate P(Y=1|X2=x2)") saving(graph2, replace) 
graph combine graph1.gph graph2.gph, ti("Lowess Smooths") ycommon 
** Run Logistic Regression
logit Y X1 X2
** Get odds ratios from model
logit, or
