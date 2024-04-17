##====================================================================
# A Stylometric Data Analysis: Application
##====================================================================
# R data file: stylo in package gamlss.data of dimensions 64 x 2
# word : is the number of times a word appears in a single text
# freq : the frequency of the number of times a word appears in a text
# purpose: to demonstrate the tting of a truncated discrete dist.
# conclusion the truncated SICHEL distributions ts best
#=====================================================================

library(gamlss)
library(gamlss.tr)

data(stylo)

plot(freq ~ word, data = stylo, type = "h",
     xlim = c(0, 22), xlab = "no of times",
     ylab = "frequencies", col = "blue")

library(gamlss.tr)

gen.trun(par = 0, family = PO, type = "left")

gen.trun(par = 0, family = NBII, type = "left")

gen.trun(par = 0, family = DEL, type = "left")

gen.trun(par = 0, family = SICHEL, type = "left",
            delta = 0.001)

# Fitting the different models

mPO <- gamlss(word ~ 1, weights = freq, data = stylo,
               family = POtr, trace = FALSE)

mNBII <- gamlss(word ~ 1, weights = freq, data = stylo,
                   family = NBIItr, n.cyc = 50, trace = FALSE)

mDEL <- gamlss(word ~ 1, weights = freq, data = stylo,
                 family = DELtr, n.cyc = 50, trace = FALSE)

mSI <- gamlss(word ~ 1, weights = freq, data = stylo,
                 family = SICHELtr, n.cyc = 50, trace = FALSE)

# Choose the best model by GAIC

GAIC(mPO, mNBII, mDEL, mSI)

