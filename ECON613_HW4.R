# Question 1
panel_data <- read.csv("C:/Users/home/Desktop/class/ECON 613/HW4/Koop-Tobias.csv", header=TRUE)
# extract data of wage and time trend from origin data set
indwage <- panel_data$LOGWAGE
indtime <-panel_data$TIMETRND
# We choose the individuals whose personal IDs are 1, 2, 4, 6, 8, then represent the panel dimension of wages for these 5 selected individuals.
ind1_w <- indwage[1:4]
ind1_t <-indtime[1:4]
plot(ind1_w ~ ind1_t)
ind2_w <- indwage[5:13]
ind2_t <-indtime[5:13]
plot(ind2_w ~ ind2_t)
ind3_w <- indwage[15:26]
ind3_t <-indtime[15:26]
plot(ind3_w ~ ind3_t)
ind4_w <- indwage[30:41]
ind4_t <-indtime[30:41]
plot(ind4_w ~ ind4_t)
ind5_w <- indwage[45:58]
ind5_t <-indtime[45:58]
plot(ind5_w ~ ind5_t)

# Question 2
# We want to use gls function to estimate the random effect, thus we should install the package "nlme".
install.packages("nlme")
library("nlme")
# extract the data of education and potential experience, from original data set.
indeduc <- panel_data$EDUC
indpot <- panel_data$POTEXPER
# combine the whole data set and use gls function to estimate coefficients
data_all <- as.data.frame(cbind(indwage,indeduc,indpot))
gls(indwage ~ indeduc + indpot, data=data_all)
# Coefficients:
#   (Intercept)     indeduc      indpot 
# 0.79419112  0.09386374  0.03740530

# Use function plm to check the coefficients, the results are almost the same.
install.packages("plm")
library("plm")
ind <- panel_data$PERSONID
data_all2 <- cbind(ind,indtime,data_all)
plm_d <- plm.data(data_all2, index = c("ind","indtime"))
plm(indwage ~ indeduc + indpot, data=data_all2, model = "random")
# Coefficients:
#   (Intercept)     indeduc      indpot 
# 0.573095    0.107268    0.038745 


# Question 3
install.packages("dplyr")
library("dplyr")
# We want to group the data set by individual ID and summarize the sum of logwage, education and potential experience,
dat_prog  =  data_all2 %>% 
  group_by(ind) %>% 
  summarise(avwage  = sum(indwage,na.rm=T),
            aveduc = sum(indeduc,na.rm=T),
            avpot = sum(indpot,na.rm=T))
# table individual ID and let sum divide the frequency to get the average of data
fre <- as.matrix(table(data_all2[,1]))
aveg <- as.data.frame(dat_prog[,2:4]/fre)
between_all <- as.data.frame(cbind(dat_prog[,1],aveg))
avwage <- aveg[,1]
aveduc <- aveg[,2]
avpot <- aveg[,3]
# use ols function to get the between estimators 
lm(avwage ~ aveduc + avpot, data=aveg)
# Coefficients:
#   (Intercept)       aveduc        avpot  
# 0.8456       0.0931       0.0260  


# combine the individual ID and the average of data
ind3 <- as.matrix(1:2178, nrow = 2178)
aveg_all <- as.matrix(cbind(ind3,aveg))
colnames(aveg_all) = c("ind","avwage","aveduc","avpot")
aveg_all <- as.data.frame(aveg_all)
# merge the new data set to original one
within_m <- data_all2 %>% left_join(aveg_all, by = "ind")
# let original data minus the average one
within_wage <- as.vector(within_m[,3]-within_m[,6])
within_educ <- as.vector(within_m[,4]-within_m[,7])
within_pot <- as.vector(within_m[,5]-within_m[,8])
within_all <- as.data.frame(cbind(within_wage,within_educ,within_pot))
# use ols function to get within estimators 
pepper <- lm(within_wage ~ within_educ + within_pot-1, data=within_all)
summary(pepper)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# within_educ 0.1236620  0.0054003   22.90   <2e-16 ***
#   within_pot  0.0385611  0.0007109   54.24   <2e-16 ***
plm(indwage ~ indeduc + indpot, data=data_all2, model = "within")


# build the vector based on t+1 and vector based on t
all_data_3 <- as.data.frame(data_all2[2:17919,])
all_data_4 <- as.data.frame(data_all2[1:17918,])
all_data <- as.data.frame(all_data_3-all_data_4)
# We want to delete the information which is created by the data from individual i+1 minusing the data from individual i.
# We only need to delete the rows whose first element equals to 1.
all_data <- all_data[!all_data[,1]==1,]
# use the first-difference model
indwage2 <- all_data$indwage
indeduc2 <- all_data$indeduc
indpot2 <- all_data$indpot
diff <- as.data.frame(cbind(indwage2,indeduc2,indpot2))
# use ols function to get first time difference estimators 
lm(indwage2 ~ indeduc2 + indpot2, data=diff)
# Coefficients:
#   (Intercept)     indeduc2      indpot2  
# 0.049464     0.038352     0.003989
plm(indwage ~ indeduc + indpot, data=data_all2, model = "fd")


# Question 4
# randomly choose the data of 100 individuals
random_data <- aveg_all[sample(1:nrow(aveg_all),100,replace=FALSE),]
random_data <- as.matrix(random_data[,1])
d <- c(0)

# Method 1
d <- panel_data[panel_data[,1] %in% random_data,]

# Method 2
#for(i in 1:100){
#  b <- random_data[i]
#  for (a in 1:17919){
#  y <- data_all2[,1]
#  y <- y[a]
#  if(y == b){
#    c <- panel_data[a,]
#    d <- rbind(d,c)
#  }
#  }
#}
#d <- d[2:nrow(d),]

# write the function of ols likelihood function and optimize the estimators
x_3 <- as.matrix(d[,2])
x_4 <- as.matrix(d[,4])
x_2 <- as.matrix(cbind(x_3,x_4))
y_2 <- as.matrix(d[,3])
like <- function(beta){  
  y <- sum(y_2*log(pnorm(x_2%*%beta))) + sum((1-y_2)*log(1-pnorm(x_2%*%beta)))
  return(-y)
}
xmin_log <- optim(c(0,0), like)$par
xmin_log
# [1] 0.3398509 0.2039105

# generate the information of alpha i, using the coefficients got from previous question
alpha <- as.matrix(d[,3]-d[,2]*0.3398509-d[,3]*0.2039105)
x_3 <- d[,6:10]
indability <- x_3[,1]
indmother <- x_3[,2]
indfather <- x_3[,3]
indbrkn <- x_3[,4]
indsibling <- x_3[,5]
data_all3 <- cbind(alpha, x_3)
# use ols function to get the estimators
lm(alpha ~ indability + indmother + indfather + indbrkn + indsibling, data=data_all3)
# Coefficients:
#   (Intercept)   indability    indmother    indfather      indbrkn   indsibling  
# -2.5651503   -0.3357836   -0.0028097    0.0004628    0.2192221    0.0229109


# Problem 1
# There could be some measurement errors when we estimate the fixed effects, so we should use bootstrap method.
# Take the between effect for example
d <- panel_data[panel_data[,1] %in% sample(1:2178,100),]
boot_between <- matrix(0,nrow = 1, ncol = 6)
for(i in 1:100){
  i <- as.matrix(sample(unique(d[,1]),100,replace = TRUE),ncol = 1)
  indi <- matrix(c(0),nrow=1,ncol = 10)
  colnames(indi) = c("PERSONID","EDUC","LOGWAGE","POTEXPER","TIMETRND","ABILITY","MOTHERED","FATHERED","BRKNHOME","SIBLINGS") 
  for(j in 1:100){
    c <- as.data.frame(d[d[,1] %in% i[j,],])
    indi <- rbind(indi, c)
  }
  indi <- indi[2:nrow(indi),]
  boot_mean <- aggregate(cbind(LOGWAGE,EDUC,POTEXPER)~PERSONID,data = indi,mean)
  alpha2 <- matrix(boot_mean[,2]-(boot_mean[,3]*0.3398509+boot_mean[,4]*0.2039105),ncol = 1)
  indi <- indi[!duplicated(indi$PERSONID),]
  boot_x <- as.matrix(indi[,6:10])
  data2 <- as.data.frame(cbind(alpha2,boot_x))
  a <- lm(alpha2 ~ boot_x)$coef
  boot_between <- rbind(boot_between,a)
}
boot_between <- boot_between[2:nrow(boot_between),]
sd_1 <- sd(boot_between[,1])
sd_2 <- sd(boot_between[,2])
sd_3 <- sd(boot_between[,3])
sd_4 <- sd(boot_between[,4])
sd_5 <- sd(boot_between[,5])
sd_6 <- sd(boot_between[,6])
sd_all <- cbind(sd_1,sd_2,sd_3,sd_4,sd_5,sd_6)
# 0.4976356 0.1164713 0.04946196 0.03570656 0.2256428 0.04747988

# Just a point of considering...
# Problem 2
# The previous standard errors are biased because they are correlated over t for given i and have the problem of heteroscedasticity.
# The inference should be based on panel-robust standard errors that permit errors to be correlated over time for a given individual and to have variances and covariances that differ across individuals.
# We use the alternative method to get robust standard errors (sandwich estimate of the vatiance matrix).
install.packages("sandwich")
library("sandwich")
vcovHC(pepper, type = "HC3")
sandwich_se <- diag(vcovHC(pepper, type = "HC3"))^0.5
print(sandwich_se)
