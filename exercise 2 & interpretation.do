* Exercise 2
clear all
set more off, perm
set scrollbufsize 2000000
cd "C:\Users\home\Desktop\class\ECON 613\HW5"
pwd

* Question 1
set seed 100
set obs 10000
generate u1 = runiform(1,3)
generate u2 = rgamma(3,2)
generate u3 = rbinomial(10000,0.3)
generate eps = rnormal(2,1)
generate Y = 0.5 + 1.2*u1 -0.9*u2 + 0.1*u3 + eps
su Y


egen mean_Y = mean(Y)
* method 1
generate ydum = 0
replace ydum = 1 if Y > mean_Y
* method 2
gen ydum_2 = (Y > mean_Y)
tab ydum

* Question 2
corr Y u1
* using OLS to get standard errors
reg Y u1 u2 u3
ssc install outreg2
outreg2 using "result.xls"
* using bootstrap with 49 and 499 replications to get standard errors
bootstrap, reps(49): regress Y u1 u2 u3
bootstrap, reps(499): regress Y u1 u2 u3

* Question 4 & 5
* write the probit model
probit ydum u1 u2 u3
* interpretation 
* 1.132912 is positive, which means that y will be more likely to happen when x1 = 1
* -.9120145 is negative, which means that y will be less likely to happen when x2 = 1
* .1018701 is positive, which means that y will be more likely to happen when x3 = 1

* Compute the marginal effect of X on Y according to the probit models and use the delta method to compute the standard deviations
margins, dydx(*) atmeans vce(delta)

* write the logit model
logit ydum u1 u2 u3
* interpretation 
* 2.057558 is positive, which means that y will be more likely to happen when x1 = 1
* -1.647092 is negative, which means that y will be less likely to happen when x2 = 1
* .1839136 is positive, which means that y will be more likely to happen when x3 = 1
* Logit and probit models yield almost the same result.

* Compute the marginal effect of X on Y according to the logit models and use the delta method to compute the standard deviations
margins, dydx(*) atmeans vce(delta)

* write the linear probability model
regress ydum u1 u2 u3, vce(robust)
* Since X1 is continuous, .0818024 is the change in the probability of success given one unit increase in x1.
* Since X2 is continuous, -.0601577 is the change in the probability of success given one unit increase in x2.
* Since X3 is discrete, .0071021 is the difference in the probability of success when x3 = 1 and x3 = 0, holding other xj fixed. It seems that X3 almost doesn't impact the probability.

* I fail to find the solution using bootstrap to calculate the se for marginal effects.
* only able to calculate the se for betas.
probit ydum u1 u2 u3, vce(bootstrap, reps(499))
logit ydum u1 u2 u3, vce(bootstrap, reps(499))
