* Question 1
clear all
set more off, perm
set scrollbufsize 2000000

* turn the data set into panel data
insheet using "C:\Users\home\Desktop\class\ECON 613\HW4\Koop-Tobias.csv", clear
xtset personid timetrnd
bysort personid: gen t = _n
xtdes

* choose the individuals whose ID are 1, 2, 4, 6, 8, plot the figures
gen ind1 = personid==1
gen logwage_1 = ind1*logwage
gen time_1 = ind1*timetrnd
scatter logwage_1 time_1
gen ind2 = personid==2
gen logwage_2 = ind2*logwage
gen time_2 = ind2*timetrnd
scatter logwage_2 time_2
gen ind3 = personid==4
gen logwage_3 = ind3*logwage
gen time_3 = ind3*timetrnd
scatter logwage_3 time_3
gen ind4 = personid==6
gen logwage_4 = ind4*logwage
gen time_4 = ind4*timetrnd
scatter logwage_4 time_4
gen ind5 = personid==8
gen logwage_5 = ind5*logwage
gen time_5 = ind5*timetrnd
scatter logwage_5 time_5

* Question 2
* estimate the random effect
 xtreg logwage educ potexper, re
 
* Question 3
* estimate the within estimator
xtreg logwage educ potexper, fe
* estimate the between estimator
xtreg logwage educ potexper, be

* estimate the first difference estimator
xtset personid t
xtdes
gen logwage_D = D.logwage
gen educ_D = D.educ
gen potexper_D = D.potexper
xtreg logwage_D educ_D potexper_D, fe
