########################################################################
########################################################################
rm(list=ls())
da <- read.csv("LondonHousePriceDataset.csv")
names(da)
dim(da)
# may be too many for demonstration 
########################################################################
########################################################################
# I create one with less variable
da1 <- da[, 1:10]
dim(da1)
names(da1)
head(da1)
da2  <- da1[, -c(1,2)]
head(da2)
names(da2)
dim(da2)
da3 <- da[, -c(1,2,22)]
head(da3)
names(da3)
dim(da3)
########################################################################
########################################################################
# exploring you data 
plot(da2)
# HPrice line is of interest 
# also look for high correlation in the x's
library(gamlss.foreach)
which.Data.Corr(da2, r=0.7)
library(corrplot)
cc <- cor(da2)
corrplot(cc)
######################################################################## 
# is any missing
dim(da2)
#dim(is.na(da2)) 
dim(na.omit(da2))
# no missing 
####################################################################### 
# look of outliers
plot(HPrice~medIncome, data=da2)
plot(HPrice~Tquantity, data=da2)
#possible transforming?
plot(HPrice~sqrt(Tquantity), data=da2)
plot(HPrice~log(Tquantity), data=da2)
# I am not sure
plot(HPrice~CrimRate, data=da2)
plot(HPrice~log(CrimRate), data=da2)
# a possible transformation 
plot(HPrice~factor(FareZone), data=da2)
# if it is not make it a factor
plot(HPrice~Perof65years, data=da2)

plot(HPrice~drivingTime, data=da2)

plot(HPrice~PTransportTime, data=da)
##################################################################
#plot(HPrice~TPopulation, data=da)
#plot(HPrice~log(TPopulation), data=da)
# think a possible transformation 

#plot(HPrice~AreaKm2, data=da)
#plot(HPrice~log(AreaKm2), data=da)
# think a possible transformation 
#plot(HPrice~PeoplePerkm2, data=da)
#plot(HPrice~Av.GCSE.2013, data=da)
#plot(HPrice~GrnSpacem2T, data=da)
#plot(HPrice~log(GrnSpacem2T), data=da)
# think a possible transformation 
# plot(HPrice~Cemissions, data=da)
# plot(HPrice~TTprimaryS, data=da)
####################################################################### 
# check for funny values
# check why the discrete values 
plot(HPrice~TTsecodS, data=da)
plot(HPrice~TTGP, data=da)
######################################################################
######################################################################
# checking simple regression with normal errors 
library( gamlss)
names(da2)
mno <- gamlss(HPrice~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
               Perof65years+drivingTime+PTransportTime, 
              family=NO, data=da)
library(gamlss.ggplots)
resid_plots(mno)

plot(mno)
# too many observations in the top 
# we may need a positive real line distribution
# The gamma distribution
mga <- gamlss(HPrice~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
               Perof65years+drivingTime+PTransportTime, 
             family=GA, data=da)
GAIC(mno, mga)
resid_plots(mga)
wp(mga)
# still too many extreme values use chooseDist

T1 <- chooseDist(mga, type="realplus", parallel="snow", ncpuss=8)
T1

mbct1 <- gamlss(HPrice~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
                Perof65years+drivingTime+PTransportTime, 
              family=BCTo, data=da)
resid_plots(mbct1)
resid_plots(mbct1)
resid_qqplot(mbct1)
resid_wp(mbct1)
model_wp(mbct1, mga)

#######################################################################
#######################################################################
# first parametric model 
mbct0 <- gamlss(HPrice~1,family=BCTo, data=da)
mbct2 <- stepGAICAll.B(mbct0, scope=list(lower=~1, 
            upper=~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
                    Perof65years+drivingTime+PTransportTime),
            parallel="snow", ncpuss=8)
# checking the model
mbct2
resid_plots(mbct2)
resid_plots(mbct2)
resid_wp(mbct2)
# interpretation of the model 
fitted_terms(mbct2) # mu
fitted_terms(mbct2,"sigma") # sigma
fitted_terms(mbct2,"nu")# nu
fitted_terms(mbct2,"tau")
summary(mbct2)

fitted_pdf(mbct2, obs=c(1000,2000,3000, 4000), from=0, to=1000000 )



#######################################################################
#######################################################################
mbct3 <- stepGAICAll.A(mbct0, scope=list(lower=~1, 
        upper=~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
                        Perof65years+drivingTime+PTransportTime),
                       parallel="snow", ncpuss=8)
mbct3

summary(mbct3)
GAIC(mbct1, mbct2, mbct3)
# checking the model
#resid_plot(mbct3)
resid_plots(mbct3)
resid_wp(mbct3)
# interpretation of the model 
fitted_terms(mbct3)
fitted_terms(mbct3,"sigma")
fitted_terms(mbct3,"nu")
fitted_terms(mbct3,"tau")

fitted_terms(mbct3, partial=T)
fitted_terms(mbct3,"sigma",  partial=T)
fitted_terms(mbct3,"nu", partial=T)
fitted_terms(mbct3,"tau", partial=T)



mbct3
summary(mbct3)
fitted_pdf(mbct3, obs=c(1000,2000,3000, 4000), from=0, to=1000000 )
fitted_pdf(mbct3, obs=1:100, from=0, to=1000000 )

GAIC(mbct1, mbct2, mbct3)

#######################################################################
#######################################################################
# smoothing terms
mbct4 <- stepGAICAll.A(mbct0, scope=list(lower=~1, 
        upper=~pb(medIncome)+pb(sqrt(Tquantity))+pb(log(CrimRate))+pb(FareZone)+
              pb(Perof65years)+pb(drivingTime)+pb(PTransportTime)),
                       parallel="snow", ncpuss=8, k=log(4835))
mbct4

fitted_terms(mbct3, partial=T)
fitted_terms(mbct4, partial=T)
fitted_terms(mbct4,"sigma",  partial=T)
fitted_terms(mbct4,"nu", partial=T)
fitted_terms(mbct4,"tau", partial=T)

resid_plot(mbct3)
resid_plots(mbct3)
resid_wp(mbct3)

GAIC(mbct1, mbct2, mbct3, mbct4)

#save(mbct2, mbct3, mbct4, mbctnn, mbcttr, file="/Users/...")
#load("/Users/...")
#######################################################################
#######################################################################
library(gamlss.add)
# neural network 
mbctnn <- gamlss(HPrice~nn(~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
                      Perof65years+drivingTime+PTransportTime,  size=10, decay=0.01),
                sigma.fo=~nn(~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
                              Perof65years+drivingTime+PTransportTime,  size=5, decay=0.1),
              #  nu.fo=~nn(~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
              #                Perof65years+drivingTime+PTransportTime),
              #  tau.fo=~nn(~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
              #              Perof65years+drivingTime+PTransportTime),
                family=BCTo, data=da2,  c.crit=0.01)
              
              plot(getSmo(mbctnn))  
              plot(getSmo(mbctnn, parameter="sigma"))       
resid_plots(mbctnn )
resid_wp(mbctnn )

GAIC(mbct1, mbct2, mbct3, mbct4, mbctnn)
GAIC(mbct1, mbct2, mbct3, mbct4, mbctnn, k=log(4835))
#######################################################################
#######################################################################                
# regression trees
                      
mbcttr <- gamlss(HPrice~tr(~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
                             Perof65years+drivingTime+PTransportTime),
                  sigma.fo=~tr(~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
                                Perof65years+drivingTime+PTransportTime),
                 # nu.fo=~tr(~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
                 #            Perof65years+drivingTime+PTransportTime),
                 # tau.fo=~nn(~medIncome+sqrt(Tquantity)+log(CrimRate)+FareZone+
                 #             Perof65years+drivingTime+PTransportTime),
                 family=BCTo, data=da2, c.crit=0.01,  bf.cyc=1)
plot(getSmo(mbcttr))
text(getSmo(mbcttr))

plot(getSmo(mbcttr, parameter="sigma"))
text(getSmo(mbcttr, parameter="sigma"))

#plot(getSmo(mbcttr, parameter="nu"))
#text(getSmo(mbcttr, parameter="nu"))
getSmo(mbcttr, parameter="nu")
resid_plots(mbcttr)

GAIC(mbct1, mbct2, mbct3, mbct4, mbctnn, mbcttr) 
GAIC(mbct1, mbct2, mbct3, mbct4, mbctnn, mbcttr,  k=log(4835))
#####################################################################
#####################################################################
#####################################################################
#####################################################################
library(gamlss)
library(gamlss.ggplots)

str(da3)
dim(da3)
l1 <- lm(HPrice~., data=da3)
summary(l1)
m1 <- gamlss(HPrice~., data=da3)

resid_plots(m1)
wp(m1)

m2 <- gamlss(HPrice~., data=da3, family=BCTo)
resid_plots(m2)
resid_wp(m2)
summary(m2)
######################################################################
######################################################################
######################################################################
library(gamlss.foreach)

X <- as.matrix(da3[,2:19])
class(X)
dim(X)
mod1 <- gamlss(HPrice~pc(x=X), data=da3, family=BCT)

resid_wp(mod1)

mod2 <- gamlss(HPrice~pc(x=X), sigma.fo=~pc(x=X), data=da3, family=BCT)             
resid_wp(mod2)

mod3 <- gamlss(HPrice~pc(x=X), sigma.fo=~pc(x=X), nu.fo=~pc(x=X), data=da3, family=BCT)             

GAIC(m2, mod1, mod2,mod3)

GAIC(m2, mod1, mod2,mod3,k=log(4884))

resid_wp(mod2)

model_wp(m2, mod1, mod2)

#--------------------------------------------------------
#interaction formula
#
form2 <- as.formula(paste("HPrice ~ ",paste0(paste0("(",paste(colnames(X), collapse='+')), ")^2"))) 
form2

m22 <- gamlss(form2, data=da3)

m22
#summary(m22)

resid_plots(m22)
wp(m22)


m33 <- gamlss(form2, data=da3, family=BCT)
resid_plots(m33)
resid_wp(m33)

GAIC(m2, mod1, mod2,mod3, m33, k=log(4884))
#########################################################



