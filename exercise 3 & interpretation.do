* Question 1
clear all
set more off, perm
set scrollbufsize 2000000

* input two datasets and merge the data
insheet using "C:\Users\home\Desktop\class\ECON 613\HW5\choiceprice.csv", clear
sort hhid
save data1,replace 
clear
insheet using "C:\Users\home\Desktop\class\ECON 613\HW5\demos.csv", clear
save data2,replace
sort hhid
merge 1:m hhid using data1.dta
gen n = 4470
gen num = _n
* generate the market shares for 10 choices
summarize ppk_stk pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub ppk_tub pfl_tub phse_tub

* Question 2&4
* turn the data of each individual into long form
rename (ppk_stk pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub ppk_tub pfl_tub phse_tub)(c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)
reshape long c,i(num) j(price)

* generate the dummy matrix for choice
gen dum = cond(price == choice,1,0)

* use the conditional logit model
asclogit dum c,case(num) alternatives(price)
* Since beta < 0, an increase in the price of one of the products 2 to 10 decreases the probability of choosing that product and increases the probability of choosing other alternatives.

* compute the marginal effect
est sto c_logit
estat mfx
* Interpretation:
* Take the first marginal effect for choice 1 as example, -1.62007 means that by other prices are unchanged, one unit increase in the price of product 1 will decrease 1.62007 in the probability to buy the product 1 on average.
* Take the second marginal effect for choice 1 as example, .38092 means that by other prices are unchanged, one unit increase in the price of product 2 will increase .38092 in the probability to buy the product 1 on average.


* Question 3&4
* use the multinomial logit model
asclogit dum, case(num) alternatives(price) casevar(income)
* Interpretation: 
* The coefficients for product 2, 5 and 7 are negative, which means that compared to product 1, a higher income leads to reduced likelihood of choosing product 4 and 6.
* The coefficients for product 1, 3, 4, 6, 8, 9 and 10 are positive, which means that compared to product 1, a higher income leads to increased likelihood of choosing product 2, 3, 5, 7, 8, 9 and 10.

* compute the marginal effect
est sto m_logit
estat mfx
* Interpretagtion:
* Take the first element in the marginal effect matrix as example, -.001062 means that one unit increase in income will decrease -0.0010504137 in probability of choosing product 1 on average.


* Question 5
* use the mixed logit model
asmixlogit dum, random(c) casevars(income) alternatives(price) case(num)
estimates store haust
* drop the data for choice 10 and use the mixed logit model again
drop if choice==10
drop if price==10
asmixlogit dum, random(c) casevars(income) alternatives(price) case(num)
estimates store haust2

hausman haust haust2, alleqs constant
* Under the degree of freedom of 9, we can check the chart of chi-square at 5% siginificance level and get the critical value of 16.919. Then we fail to reject the null hypothesis that the two results in (1) and (2) are the same. 
* We get the result because that the market share of product 10 is only 7%, which means that it may effect very little on the whole market, so we can see very small difference after excluding product 10.

