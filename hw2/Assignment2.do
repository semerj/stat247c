**NEW
**** Simulation of Simple Random Effects Model     
 clear
 set obs 5000 
*m=1000 

**** Generate Random N(0,sigma_alpha) variable 
**** This section needs to be completed
 scalar sigmaalpha= //value for sigmaalpha
 scalar rho = //value for rho
 scalar sigmae = //formula for sigmae (using sigmaalpha and rho)
 gen alpha = rnormal(0,sigmaalpha)
 
**** Generate id
 gen id = _n
**** Give two observations for every person
 expand 2
**** Make "time" variable
 sort id
 by id: gen time = _n
 
**** Grand mean = 10
 gen mu = 10
**** independent random errors
  ** Generate Random independent eij~N(0,sigmae) variables (see solutions for sigma_e = 2)
 gen epsilon = rnormal(0,sigmae)
**** Make outcome variables, Yij
 gen Y = mu+alpha+epsilon 
 ** Check the distribution of outcome -- does this look okay?
 twoway hist Y   
 
*** True Correlation (should equal rho)
scalar cor = sigmaalpha^2/(sigmae^2+sigmaalpha^2)
display "cor = " cor 

**** Generate estimates of the grand mean and correlation
  ** You can do this one at a time or using the loop below, setting the values of m you want.

**** Break up samples chunks so that we can get estimate for m=10,50,100,1000 by using ID
*************** (A little trick)

gen muest = -1000
gen corest = -1000
foreach k of numlist 10 50 100 1000 5000 {
display " m = " `k'   
capture drop Ytemp
gen Ytemp = Y
**** Make all values > m of interest = . to restrict sample size
replace Ytemp = . if id > `k'
capture drop mutemp
**** Estimate mean, and store value at id = m of interest
egen mutemp = mean(Ytemp)
replace muest = mutemp if id == `k'
**** Estimate cor, and store value at id = m of interest
reshape wide Y Ytemp epsilon, i(id) j(time)
cor Ytemp1 Ytemp2
scalar cortemp = r(rho)
replace corest=cortemp if id == `k'
reshape long Y Ytemp epsilon, i(id) j(time)
}

reshape wide Y Ytemp epsilon, i(id) j(time)
*** Get rid of all observations except those with estimates to plot
keep if muest > -999
label variable id "Number Subjects"
label variable corest  "Correlation Estimate"
label variable muest "Mean Estimate"
scatter muest id, t1("Mean Estimate by Sample Size") saving(graph1, replace) xlabel(10 1000 ) yline(10, lcolor(red)) ylabel(9.5(0.1)10.5) xscale(log)
scatter corest id, t1("Cor Estimate by Sample Size") saving(graph2, replace) xlabel(10 1000 ) yline(0.3,lcolor(red)) ylabel(0.0(0.05)0.4) xscale(log)
gr combine graph1.gph graph2.gph

