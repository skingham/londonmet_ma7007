#=======================================================================
# The Hospital Stay Data Analysis
#=======================================================================
# R data file: aep in package gamlss of dimensions 1383 x 8
# los : total number of days
# noinap : number of inappropriate days patient stay in hospital
# loglos : the log of los/10
# sex : the gender of patient
# ward : type of ward in the hospital (medical, surgical etc)
# year : 1988 or 1990
# age : age of the patient subtracted from 55
# y : the response variable, a matrix with columns (noinap, los-noinap)
#=======================================================================

library(gamlss)
data(aep)

#Fitting four different models for the data

mI <- gamlss(y ~ ward + year + loglos, sigma.fo = ~year,
              family = BB,data = aep)
mII <- gamlss(y ~ ward + year + loglos, sigma.fo = ~year +
                   ward, family = BB, data = aep)
mIII <- gamlss(y ~ ward + year + cs(loglos, 1),
                  sigma.fo = ~year+ward, family = BB, data = aep)
mIV <- gamlss(y ~ ward + year + cs(loglos, 1) + cs(age, 1),
                 sigma.fo = ~year + ward, family = BB, data = aep)

#Selecting the best model

GAIC(mI, mII, mIII, mIV, k = 0)

#Plots: fitted model fop mu, sigma and normalised quantile residuals

op <- par(mfrow = c(2, 2))
term.plot(mIV, se = T)
par(op)
op <- par(mfrow = c(2, 1))
term.plot(mIV, "sigma", se = T)
par(op)
rqres.plot(mIV)
