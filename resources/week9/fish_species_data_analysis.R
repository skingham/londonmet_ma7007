#===============================================================================
# The Fish Species Data Analysis
#===============================================================================
# R data file: species in package gamlss.data of dimension 70 × 2
# fish : the number of different species in 70 lakes in the world
# lake : the lake area
# purpose: to demonstrate results of fitting using RS, CG and ‘mixed’ algorithms
#===============================================================================

library(gamlss)
data(species)

# creating the log(lake)
species <- transform(species, x=log(lake))
plot(fish~x,data=species)

#=================== Questions =========================
# How does the mean of the response variable depend on x?
# Is the response variable overdispersed Poisson?
# How does the variance of the response variable depend on its mean?
# What is the conditional distribution of the response variable given x?
# Do the scale and shape parameters of the response variable distribution depend on x?
#=========================================================

# the count distributions
fam<-c("PO","DPO", "NBI", "NBII", "PIG", "DEL", "SICHEL")
#creating lists to keep the results
m.l<-m.q<-list()
# fitting the linear in x models
for (i in 1:7) {
  m.l[[fam[i]]]<-GAIC(gamlss(fish~x,data=species, family=fam[i],
                             n.cyc=60, trace=FALSE),k=2)}
# fitting the quadratic in x models
for (i in 1:7) {
  m.q[[fam[i]]]<-GAIC(gamlss(fish~poly(x,2),data=species,
                             family=fam[i], n.cyc=60, trace=FALSE), k=2)}
# print the AICs
unlist(m.l)
unlist(m.q)

GAIC(m.pb<-gamlss(fish~pb(x), data=species, family=PIG, trace=FALSE))

# redefine the list of distributions
fam<-c("DPO","NBI", "NBII", "PIG", "DEL", "SICHEL")
m.ql<-list()
for (i in 1:6) {
  m.ql[[fam[i]]]<-GAIC(gamlss(fish~poly(x,2),data=species,
                              sigma.fo=~x, family=fam[i], n.cyc=60, trace=FALSE))}
unlist(m.ql)

fam<-c("DEL", "SICHEL")
m.qll<-list()
for (i in 1:2) {
  m.qll[[fam[i]]]<-GAIC(gamlss(fish~poly(x,2),data=species,
                               sigma.fo=~x, nu.fo=~x, family=fam[i], n.cyc=60,
                               trace=FALSE))}
unlist(m.qll)

mSI<-gamlss(fish~poly(x,2),data=species, sigma.fo=~1, nu.fo=~x,
            family=SICHEL, n.cyc=60, trace=FALSE)
GAIC(mSI)

plot(fish~log(lake), data=species)
lines(species$x[order(species$lake)], 
      fitted(mSI)[order(species$lake)], col="red")

pdf.plot(mSI,c(7,68), min=0, max=120, step=1)

# install.packages("gamlss.mx")
library(gamlss.mx)

m1 <- gamlss(fish~poly(x,2), data=species, family=PO, trace=FALSE)
m2 <- gamlss(fish~x, data=species, family=NBI, trace=FALSE)
m3 <- gamlss(fish~poly(x,2), data=species, family=NBI, trace=FALSE)
m4 <- gamlss(fish~cs(x,3), data=species, family=NBI, trace=FALSE)
m5 <- gamlss(fish~poly(x,2), sigma.fo=~x, data=species, family=NBI, trace=FALSE)
m6 <- gamlss(fish~poly(x,2), sigma.fo=~1, data=species, family=NBF, n.cyc=200, trace=FALSE)
m7 <- gamlss(fish~poly(x,2), sigma.fo=~x, data=species, family=NBF, n.cyc=100, trace=FALSE)
m8 <- gamlss(fish~poly(x,2), data=species, family=PIG, trace=FALSE)
m9 <- gamlss(fish~poly(x,2), nu.fo=~x, data=species, family=SICHEL, trace=FALSE)
m10 <- gamlss(fish~poly(x,2), nu.fo=~x, data=species, family=DEL, n.cyc=50, trace=FALSE)
m11 <- gamlss(fish~poly(x,2), nu.fo=~x, data=species, family=DEL, sigma.fix=TRUE, sigma.start=1, n.cyc=50, trace=FALSE)
m12 <- gamlssNP(fish~poly(x,2), data=species, mixture = "gq", K=20,
                family=PO, control=NP.control(trace=FALSE))
m13 <- gamlssNP(fish~poly(x,2), sigma.fo=~x, data=species,
                mixture = "gq", K=20, family=NBI,
                control=NP.control(trace=FALSE))
m14 <- gamlssNP(fish~poly(x,2), data=species, mixture = "np", K=6,
                tol=0.1,family=PO, control=NP.control(trace=FALSE))
m15 <- gamlssNP(fish~poly(x,2), data=species, mixture = "np", K=2,
                family=NBI, control=NP.control(trace=FALSE))
m16 <- gamlss(fish~poly(x,2), nu.fo=~x, data=species, family=DPO, trace=FALSE)

# install.packages("gamlss.cens")
library(gamlss.cens)

m17 <- gamlss(Surv(fish,fish+1,type= "interval2")~x+I(x^2),
              sigma.fo=~1, data=species,
              family=cens(IG, type="interval"), trace=FALSE)
GAIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14,
     m15, m16, m17)

GAIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14,
     m15, m16, m17, k=log(70))

wp(m9) ; title("(a)")
wp(m11); title("(b)")

mSI<- gamlss(fish~x+I(x^2), sigma.fo=~1, nu.fo=~x, data=species,
             family=SICHEL, trace=FALSE)
summary(mSI)
