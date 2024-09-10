library(gamlss)
library(ggplot2)
library(gamlss.ggplots)

data(aids)
# fitting a negative binomial type I distribution
aids.1<-gamlss(y~poly(x,3)+qrt, family=NBI, data=aids) #

da <- data.frame(aids, fv=fitted(aids.1))
ggplot(data=da, 
  aes(x=x, y=y))+
  geom_point()+ 
  geom_line(aes(x=x, y=fv), col="red")
  

#plot(aids$x, aids$y)
#lines(aids$x, aids.1$mu.fv,col="red")
# just the  predictor eta
predict(aids.1)
aids.1$mu.lp
# the parameters
predict(aids.1, type="response")
fitted(aids.1)

# plot actual values against predicted values
plot(predict(aids.1, type="response"), aids$y, 
     xlab = "Predicted Values", 
     ylab = "Actual Values") 
abline(a = 0, b = 1, lwd=2, 
       col = "green")

# Correlation between actual and predicted values
Actual<-aids$y
Predicted<-predict(aids.1, type="response")

cor(Predicted, Actual)
# Compare actual and predicted values
dtfr<-data.frame(Actual, Predicted)
dtfr

#### To obtain Actual versus Predicted graph
plot(Actual, type = "l", col = "black", lwd = 1, ylab = "Actual Values",
     
     main = "Actual versus Predicted Graph")

#add a legend
legend(x = "topleft", legend = c("Actual","Predicted"), lty = 1,
       lwd = 1, col = c("green", "blue"))
#add lines
lines(Actual, col = "green", lwd = 1)
lines(Predicted, col = "blue", lwd = 1)

# with se

paids.1 <- predict(aids.1, what="mu", se.fit=TRUE ,type="response")
names(paids.1)
paids.1$se.fit

#paids.2 <- predict(aids.1, what="mu", se.fit=TRUE ,type="link")
#head(paids.2$se.fit)

da <- data.frame(aids, fv=fitted(aids.1), pl=fitted(aids.1)-2*paids.1$se.fit, 
                 pu=fitted(aids.1)+2*paids.1$se.fit)
head(da)
ggplot(data=da, aes(x=x, y=y))+geom_point()+ 
  geom_ribbon(aes(ymin = pl, ymax = pu), fill = "grey")+
  geom_line(aes(x=x, y=fv), col="red")


# terms 
paids.2 <- predict(aids.1, what="mu", type="terms")
colnames(paids.2)
# now with se
paids.2 <- predict(aids.1, what="mu", type="terms", se.fit=TRUE)
names(paids.2)
colnames(paids.2$fit)
colnames(paids.2$se.fit)


paids.2 <- predict(aids.1, what="mu", type="terms", se.fit=TRUE, terms="qrt")
colnames(paids.2$fit)


data(aids)
# use with poly
mod1 <- gamlss(y~poly(x,3)+qrt, family=NBI, data=aids, trace=FALSE) #
# use bs
mod2 <- gamlss(y~bs(x,5)+qrt, family=NBI, data=aids, trace=FALSE) #
# use with pb
mod3 <- gamlss(y~pb(x)+qrt, family=NBI, data=aids) 

newaids<-data.frame(x=c(46,47,48), qrt=c(2,3,4))
# predict "mu" at new values
(ap1 <- predict(mod1, what="mu", newdata=newaids, type = "response"))
(ap2 <- predict(mod2, what="mu", newdata=newaids, type = "response"))
(ap3 <- predict(mod3, what="mu", newdata=newaids, type = "response"))

# get the term contributions to the predictor of mu
(ap4 <- predict(mod3, what="mu", newdata=newaids, type = "terms"))
# predict sigma at new values
(ap5 <- predict(mod3, what="sigma", newdata=newaids, type="response"))
# usinf tranformed x variable
ap1
ap2
ap3
ap4
ap5

# compare predicted mu at new data for mod1, mod2 & mod3
dtfr2<-data.frame(ap1, ap2, ap3)
dtfr2

#### To obtain the graph of Predicted Values
plot(ap1, type = "l", col = "black", lwd = 1, ylab = "Predicted Values",
     
     main = "Graph of Predicted Values")

#add a legend
legend(x = "topleft", legend = c("ap1","ap2","ap3"), lty = 1,
       lwd = 1, col = c("green", "blue", "red"))
#add lines
lines(ap1, col = "green", lwd = 1)
lines(ap2, col = "blue", lwd = 1)
lines(ap3, col = "red", lwd = 1)

# compare and select the best model based on AIC
AIC(mod1, mod2, mod3)

## plot() and wp()
plot(mod3)
wp(mod3)

# transorming x
# data(abdom)
# assume that a transformation x^5 is required
aa<-gamlss(y~pb(x^.5),data=abdom, trace=FALSE)
# predict at old values, e.g. case 610
predict(aa, what="mu")[610]
# predict at new data
predict(aa,newdata=data.frame(x=abdom$x[610]))
# now transform x first
nx<-abdom$x^.5
aaa<-gamlss(y~pb(nx),data=abdom, trace=FALSE)
# create a new data frame
newd<-data.frame(abdom, nx=abdom$x^0.5)
# predict at old values
predict(aaa)[610]
# predict at new values
predict(aaa,newdata=data.frame(nx=abdom$x[610]^.5), data=newd)



h<-gamlss(y~pb(x), sigma.fo=~pb(x), family=TF, data=abdom)
hall<-predictAll(h)
hall

hall1<-predictAll(h, output="matrix")
hall1



newabdom<-data.frame(x=c(20,25,30))
hall2<-predictAll(h,newdata=newabdom)
hall2
hall21<-predictAll(h,newdata=newabdom, output="matrix")
hall21

library(gamlss.ggplots)
#predict_pdf(h, newdata=newabdom, from=30, to=100)
predict_pdf(h, newdata=abdom[10:20, ], from=30, to=100)

pdf.plot(mu=hall2$mu, sigma=hall2$sigma, nu=hall2$nu, family=TF, min=100, 
         max= 350, step=1)


h<-gamlss(y~pb(x),sigma.fo=~pb(x),family=TF,data=abdom,trace=FALSE)
hall<-predictAll(h)


newabdom<-data.frame(x=c(20,25,30))
hall2<-predictAll(h,newdata=newabdom)

par(cex.axis=1.5,cex.lab=1.5,cex.main=1.2)
pdf.plot(mu=hall2$mu, sigma=hall2$sigma, nu=hall2$nu, family=TF, 
         min=100, max= 350, step=1)




