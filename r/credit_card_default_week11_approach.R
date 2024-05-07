########################################################################
########################################################################
setwd('C:\\Users\\stuar\\Projects\\Londonmet\\londonmet_ma7007\\data')
rm(list=ls())
cc <- read.csv("UCI_Credit_Card.csv")
########################################################################
# Schema of UCI_Credit_Card.csv
# https://archive.ics.uci.edu/dataset/350/default+of+credit+card+clients
# "ID","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","AGE","PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6","default.payment.next.month"
# Variable Name Role	Type	  Demographic	      Description	Units	Missing Values
# ID            ID	  Integer				                              no
# X1	      Feature	  Integer		                LIMIT_BAL		      no
# X2	      Feature	  Integer	  Sex	            SEX		            no
# X3	      Feature	  Integer	  Education Level	EDUCATION		      no
# X4	      Feature	  Integer	  Marital Status	MARRIAGE		      no
# X5	      Feature	  Integer	  Age	            AGE		            no
# X6	      Feature	  Integer		                PAY_0		          no
# X7	      Feature	  Integer		                PAY_2		          no
# X8	      Feature	  Integer		                PAY_3		          no
# X9	      Feature	  Integer		                PAY_4		          no
# X10	      Feature	  Integer		                PAY_5		          no
# X11	      Feature	  Integer		                PAY_6		          no
# X12	      Feature	  Integer		                BILL_AMT1		      no
# X13	      Feature	  Integer		                BILL_AMT2		      no
# X14	      Feature	  Integer		                BILL_AMT3		      no
# X15	      Feature	  Integer		                BILL_AMT4		      no
# X16	      Feature	  Integer		                BILL_AMT5		      no
# X17	      Feature	  Integer		                BILL_AMT6		      no
# X18	      Feature	  Integer		                PAY_AMT1		      no
# X19	      Feature	  Integer		                PAY_AMT2		      no
# X20	      Feature	  Integer		                PAY_AMT3		      no
# X21	      Feature	  Integer		                PAY_AMT4		      no
# X22	      Feature	  Integer		                PAY_AMT5		      no
# X23	      Feature	  Integer		                PAY_AMT6		      no
# Y	        Target	  Binary		                default.payment.next.month		no
names(cc)
dim(cc)

cc$SEX <- factor(cc$SEX)
cc$EDUCATION <- factor(cc$EDUCATION)
cc$MARRIAGE <- factor(cc$MARRIAGE)

# may be too many for demonstration 
########################################################################
########################################################################
# I create one with less variable
cc1 <- cc[, -c(1, 9, 10, 11, 12, 15, 16, 17, 18, 21, 22, 23, 24)]
dim(cc1)
names(cc1)
head(cc1)

cc2  <- cc1[, -c(7, 9, 11)]
head(cc2)
names(cc2)
dim(cc2)

cc3 <- cc[, -c(1,11,12,17,18,23,24)]
head(cc3)
names(cc3)
dim(cc3)
########################################################################
########################################################################
# exploring you data 
plot(cc2)
# default.payment.next.month line is of interest 
# also look for high correlation in the x's
library(gamlss.foreach)
which.Data.Corr(cc2, r=0.7)
library(corrplot)
cc_cor <- cor(cc)
corrplot(cc_cor)
######################################################################## 
# is any missing
dim(cc2)
#dim(is.na(cc2)) 
dim(na.omit(cc2))
# no missing 
####################################################################### 
# look of outliers
plot(default.payment.next.month~LIMIT_BAL, data=cc2)
plot(default.payment.next.month~BILL_AMT1, data=cc2)
#possible transforming?
plot(default.payment.next.month~sqrt(BILL_AMT1), data=cc2)
plot(default.payment.next.month~log(BILL_AMT1), data=cc2)
# I am not sure
plot(factor(default.payment.next.month)~PAY_0, data=cc2)
plot(default.payment.next.month~log(PAY_0), data=cc2)

# a possible transformation 
plot(factor(default.payment.next.month)~MARRIAGE, data=cc2)

# if it is not make it a factor
plot(default.payment.next.month~EDUCATION, data=cc2)

plot(default.payment.next.month~PAY_AMT1, data=cc2)

plot(default.payment.next.month~AGE, data=cc)
##################################################################
#plot(default.payment.next.month~TPopulation, data=cc)
#plot(default.payment.next.month~log(TPopulation), data=cc)
# think a possible transformation 

#plot(default.payment.next.month~AreaKm2, data=cc)
#plot(default.payment.next.month~log(AreaKm2), data=cc)
# think a possible transformation 
#plot(default.payment.next.month~PeoplePerkm2, data=cc)
#plot(default.payment.next.month~Av.GCSE.2013, data=cc)
#plot(default.payment.next.month~GrnSpacem2T, data=cc)
#plot(default.payment.next.month~log(GrnSpacem2T), data=cc)
# think a possible transformation 
# plot(default.payment.next.month~Cemissions, data=cc)
# plot(default.payment.next.month~TTprimaryS, data=cc)
####################################################################### 
# check for funny values
# check why the discrete values 
plot(default.payment.next.month~TTsecodS, data=cc)
plot(default.payment.next.month~TTGP, data=cc)
######################################################################
######################################################################
# checking simple regression with normal errors 
library( gamlss)
names(cc2)

mno <- gamlss(default.payment.next.month ~ LIMIT_BAL, 
              family = NO, # NO is for Normal distribution
              data = cc)

mno <- gamlss(default.payment.next.month~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
              PAY_0+PAY_AMT1+AGE, 
              family=NO, data=cc)
library(gamlss.ggplots)
resid_plots(mno)

plot(mno)
# too many observations in the top 
# we may need a positive real line distribution
# The gamma distribution

mga <- gamlss(default.payment.next.month~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                PAY_0+PAY_AMT1+AGE, 
              family=GA, data=cc)
GAIC(mno, mga)
resid_plots(mga)
wp(mga)
# still too many extreme values use chooseDist

T1 <- chooseDist(mga, type="realplus", parallel="snow", ncpuss=8)
T1

mbct1 <- gamlss(default.payment.next.month~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                  PAY_0+PAY_AMT1+AGE, 
                family=BCTo, data=cc)
resid_plots(mbct1)
resid_plots(mbct1)
resid_qqplot(mbct1)
resid_wp(mbct1)
model_wp(mbct1, mga)

#######################################################################
#######################################################################
# first parametric model 
mbct0 <- gamlss(default.payment.next.month~1,family=BCTo, data=cc)
mbct2 <- stepGAICAll.B(mbct0, scope=list(lower=~1, 
                                         upper=~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                                           PAY_0+PAY_AMT1+AGE),
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
                                         upper=~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                                         PAY_0+PAY_AMT1+AGE),
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
                                         upper=~pb(LIMIT_BAL)+pb(sqrt(BILL_AMT1))+pb(log(EDUCATION))+pb(MARRIAGE)+
                                           pb(PAY_0)+pb(PAY_AMT1)+pb(AGE)),
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
mbctnn <- gamlss(default.payment.next.month~nn(~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                             PAY_0+PAY_AMT1+AGE,  size=10, decay=0.01),
                 sigma.fo=~nn(~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                                PAY_0+PAY_AMT1+AGE,  size=5, decay=0.1),
                 #  nu.fo=~nn(~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                 #                PAY_0+PAY_AMT1+AGE),
                 #  tau.fo=~nn(~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                 #              PAY_0+PAY_AMT1+AGE),
                 family=BCTo, data=cc2,  c.crit=0.01)

plot(getSmo(mbctnn))  
plot(getSmo(mbctnn, parameter="sigma"))       
resid_plots(mbctnn )
resid_wp(mbctnn )

GAIC(mbct1, mbct2, mbct3, mbct4, mbctnn)
GAIC(mbct1, mbct2, mbct3, mbct4, mbctnn, k=log(4835))
#######################################################################
#######################################################################                
# regression trees

mbcttr <- gamlss(default.payment.next.month~tr(~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                             PAY_0+PAY_AMT1+AGE),
                 sigma.fo=~tr(~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                                PAY_0+PAY_AMT1+AGE),
                 # nu.fo=~tr(~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                 #            PAY_0+PAY_AMT1+AGE),
                 # tau.fo=~nn(~LIMIT_BAL+sqrt(BILL_AMT1)+log(EDUCATION)+MARRIAGE+
                 #             PAY_0+PAY_AMT1+AGE),
                 family=BCTo, data=cc2, c.crit=0.01,  bf.cyc=1)
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

str(cc3)
dim(cc3)
l1 <- lm(default.payment.next.month~., data=cc3)
summary(l1)
m1 <- gamlss(default.payment.next.month~., data=cc3)

resid_plots(m1)
wp(m1)

m2 <- gamlss(default.payment.next.month~., data=cc3, family=BCTo)
resid_plots(m2)
resid_wp(m2)
summary(m2)
######################################################################
######################################################################
######################################################################
library(gamlss.foreach)

X <- as.matrix(cc3[,2:19])
class(X)
dim(X)
mod1 <- gamlss(default.payment.next.month~pc(x=X), data=cc3, family=BCT)

resid_wp(mod1)

mod2 <- gamlss(default.payment.next.month~pc(x=X), sigma.fo=~pc(x=X), data=cc3, family=BCT)             
resid_wp(mod2)

mod3 <- gamlss(default.payment.next.month~pc(x=X), sigma.fo=~pc(x=X), nu.fo=~pc(x=X), data=cc3, family=BCT)             

GAIC(m2, mod1, mod2,mod3)

GAIC(m2, mod1, mod2,mod3,k=log(4884))

resid_wp(mod2)

model_wp(m2, mod1, mod2)

#--------------------------------------------------------
#interaction formula
#
form2 <- as.formula(paste("default.payment.next.month ~ ",paste0(paste0("(",paste(colnames(X), collapse='+')), ")^2"))) 
form2

m22 <- gamlss(form2, data=cc3)

m22
#summary(m22)

resid_plots(m22)
wp(m22)


m33 <- gamlss(form2, data=cc3, family=BCT)
resid_plots(m33)
resid_wp(m33)

GAIC(m2, mod1, mod2,mod3, m33, k=log(4884))
#########################################################
