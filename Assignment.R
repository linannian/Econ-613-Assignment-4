getwd()
setwd('Documents/R programming/Econ 613/assignment4/')

# Question 1
dat <- read.csv('Koop-Tobias.csv')
par(mfrow=c(3,2))
set.seed(2018)
sam <- sort(sample(1:length(unique(dat$PERSONID)),replace=F, size=5))
for (i in 1:5){
  holder <- dat[dat$PERSONID==sam[i],]
  plot(holder$TIMETRND,holder$LOGWAGE,xlab = 'Time Trend',ylab = 'Log Wage', main = paste('Personal ID',sam[i],sep = ' '))
}

# Question 2
library('nlme')
RandomEffect <- gls(LOGWAGE ~ EDUC+POTEXPER, data = dat,correlation = corCompSymm(form = ~1 | PERSONID)) 
summary(RandomEffect)

## Question 3
# Between estimator
library(dplyr)
library(plm)
dat_fe1 <- dat %>%
  group_by(PERSONID) %>%
  summarise(SumWage = sum(LOGWAGE, na.rm = T),
            SumEdu = sum(EDUC, na.rm = T),
            SumPot = sum(POTEXPER, na.rm = T))

freq <- as.matrix(table(dat[,1]))
between_est <- as.data.frame(dat_fe1[,2:4]/freq)
between_est <- cbind.data.frame(dat_fe1[,1],between_est)
colnames(between_est)[2:4] <- c('AvgWage','AvgEdu','AvgPot')
fit <- lm(AvgWage ~ AvgEdu + AvgPot, data = between_est)
summary(fit)
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.845569   0.077018  10.979  < 2e-16 ***
#  AvgEdu      0.093100   0.004668  19.942  < 2e-16 ***
#  AvgPot      0.025999   0.003605   7.212 7.57e-13 ***
# For verification, plm(formula = LOGWAGE ~ EDUC + POTEXPER,data = dat,model = "between",index = 'PERSONID')

# Within estimator
dat_fe2 <- merge(dat,between_est,by = 'PERSONID')
org_data <- cbind(dat_fe2$LOGWAGE,dat_fe2$EDUC,dat_fe2$POTEXPER)
within_est <- as.data.frame(org_data - as.matrix(dat_fe2[,11:13]))
colnames(within_est) <- c('WiWage','WiEdu','WiPot')
fit2 <- lm(WiWage~WiEdu+WiPot-1, data = within_est)
summary(fit2)

#Coefficients:
#       Estimate   Std. Error t value Pr(>|t|)   
# WiEdu 0.1236620  0.0054003   22.90   <2e-16 ***
# WiPot 0.0385611  0.0007109   54.24   <2e-16 ***
# For verification, plm(formula = LOGWAGE ~ EDUC + POTEXPER,data = dat,model = "within",index = 'PERSONID')

# First time difference estimator
dat_fe3 <- as.data.frame(apply(dat, 2, diff))
dat_fe3 <- dat_fe3[dat_fe3[,1]!=1,]
fd_estimator <- cbind.data.frame(dat_fe3$LOGWAGE,dat_fe3$EDUC,dat_fe3$POTEXPER)
colnames(fd_estimator) <- c('FdWage','FdEdu','FdPot')
fit3 <- lm(FdWage~FdEdu+FdPot, data = fd_estimator)
summary(fit3)
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 0.049464   0.005536   8.935  < 2e-16 ***
#  FdEdu       0.038352   0.008141   4.711 2.49e-06 ***
#  FdPot       0.003989   0.003887   1.026    0.305   
# For verification, plm(formula = LOGWAGE ~ EDUC + POTEXPER,data = dat,model = "fd",index = 'PERSONID')

## The beta in three kinds of estimator are very different and so does their
## significance level. The first two model produce beta estimates significant 
## at 5% level. But the POTEXPER in the first time differencing model is not 
## significant at 5%.


# Question 4
samp <- sample(1:2178,replace = F,size = 100)
samp <- as.data.frame(sort(samp))
colnames(samp) <- 'PERSONID'
Q4subset <- merge(samp,dat,by='PERSONID')

loglik_probit <- function(X,Y,beta){
  norm_cdf <- pnorm(X %*% beta)
  f <- sum(Y*log(norm_cdf)) + sum((1-Y)*log(1-norm_cdf))
  f <- -f
  return(f)
}

x <- cbind(Q4subset$EDUC,Q4subset$POTEXPER)
y <- Q4subset$LOGWAGE

start <- c(0,0)
op <- optim(par=start,loglik_probit,X=x, Y=y)
beta <- op$par #0.48305016 0.04025418

mean_data <- matrix(0,ncol = 4, nrow = 100)
colnames(mean_data) <- c('ID','Edu','LWage','Potexp')
ID_num <- unique(Q4subset$PERSONID)
for (i in 1:100){
  holder <- as.matrix(apply(Q4subset[Q4subset$PERSONID == ID_num[i],],2,mean))
  mean_data[i,] <- holder[1:4]
}

alpha <- mean_data[,3] - (mean_data[,c(2,4)] %*% beta)
invariant <- cbind(Q4subset[,c(1,6,7,8,9,10)])
invariant <- invariant[!duplicated(invariant),]
invariant <- cbind(invariant, alpha)

fit4 <- lm(alpha ~ ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS,data = invariant)
summary(fit4)

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) -4.11383    0.37862 -10.865  < 2e-16 ***
#  ABILITY     -0.38114    0.07762  -4.910 3.83e-06 ***
#  MOTHERED    -0.04651    0.03779  -1.231   0.2216    
#  FATHERED     0.01890    0.02667   0.709   0.4803    
#  BRKNHOME     0.18185    0.15914   1.143   0.2561    
#  SIBLINGS     0.06871    0.03946   1.741   0.0849 .  

## Error here may correlate overtime
## It might be wise to calculate SE with heteroskedascity assumption
## We might also use bootstrap to resampling these 100 samples from 
## our 2178 samples and find average of SE, or calculate the standard
## error of these bootstrapped beta coefficients

# bootstrap way to calculate std
coefficient_holder <- matrix(0,ncol = 6,nrow = 100)
coefficient_holder <- as.data.frame(coefficient_holder)
colnames(coefficient_holder) <- c('Intercept','Ability','Mothered',
                                  'Fathered','Brknhome','Siblings')

## this loop takes around 5 secs, bootstrap for 100 times, 100person drawed for each time
for (i in 1:100){
  samples <- sample(1:2178,replace = F,size = 100)
  samples <- as.data.frame(sort(samples))
  colnames(samples) <- 'PERSONID'
  BS_subset <- merge(samples,dat,by='PERSONID')
  
  x <- cbind(BS_subset$EDUC,BS_subset$POTEXPER)
  y <- BS_subset$LOGWAGE
  
  op <- optim(par=start,loglik_probit,X=x, Y=y)
  beta <- op$par
  
  mean_datas <- matrix(0,ncol = 4, nrow = 100)
  colnames(mean_datas) <- c('ID','Edu','LWage','Potexp')
  ID_nums <- unique(BS_subset$PERSONID)
  for (j in 1:100){
    holder <- as.matrix(apply(BS_subset[BS_subset$PERSONID == ID_num[j],],2,mean))
    mean_datas[j,] <- holder[1:4]
  }
  
  alphas <- mean_datas[,3] - (mean_datas[,c(2,4)] %*% beta)
  invars <- cbind(BS_subset[,c(1,6,7,8,9,10)])
  invars <- invariant[!duplicated(invars),]
  invars <- cbind(invars, alpha)
  
  fit_bs <- lm(alpha ~ ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS,data = invars)
  coefficient_holder[i,] <- as.numeric(coefficients(fit_bs))
}

SE_boot <- matrix(0,ncol = 6,nrow = 1)
colnames(SE_boot) <- colnames(coefficient_holder)
for (i in 1:6){
  SE_boot[i] <- sd(coefficient_holder[,i],na.rm = T)
}
SE_boot # 1.594669 0.3091645 0.1677312 0.1112179 0.5978643 0.2155053 quite different from last time

