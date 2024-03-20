rm(list=ls())
##################################################################
##################################################################
# Function creating the B basis
Bbase <- function(x,  ndx=20, deg=3){
tpower <- function(x, t, p) {(x - t) ^ p * (x > t)}
    xl <- min(x, na.rm = TRUE)
    xr <- max(x, na.rm = TRUE)
  xmin <- xl - 0.01 * (xr - xl) 
  xmax <- xr + 0.01 * (xr - xl)
    dx <- (xmax - xmin) / ndx # DS increment 
 knots <- seq(xmin - deg * dx, xmax + deg * dx, by = dx)
     P <- outer(x, knots, tpower, deg)# calculate the power in the knots
     n <- dim(P)[2]
     D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg) # 
     B <- (-1) ^ (deg + 1) * P %*% t(D) 
  attr(B, "knots") <- knots[-c(1:(deg-1), (n-(deg-2)):n)]
  B 
}
#################################################################
#################################################################
#################################################################
# Showing how to generate the penalty matrices
#################################################################
# order=1
D1 <- diff(diag(10), diff=1)
D1
# order=2
D2 <- diff(diag(10), diff=2)
D2
G1 <- t(D1)%*%D1
G1
G2 <- t(D2)%*%D2
G2
#################################################################
#################################################################
#################################################################
# generating the data
################################################################
################################################################
n <- 500
x <- seq(0, 1, length = n)*1.4
set.seed(123)
y <- 1.2 + .3*sin(5  * x) + rnorm(n) * 0.2
plot(y~x)
###############################################################
###############################################################
# fitting the model
# fit the penalise least squares
library(gamlss)
m1 <- gamlss(y~pb(x))
# the knots 
pbKnots <- getSmo(m1)$knots
length(pbKnots)
# get the x's and cut than in equal space
plot(y~x, pch=20)
abline(v=getSmo(m1)$knots, col="gray")
length(getSmo(m1)$knots)
###############################################################
###############################################################
# create the basis for x
B <- Bbase(x)
dim(B)
#cbind(attr(B, "knots"),getSmo(m1)$knots)
# B basis
matplot(x,B, type="l", col="black")
abline(v=getSmo(m1)$knots, col="gray")
# The fitted values is the B multiplied by gamma's
# The fitted coefficients   
plot(getSmo(m1)$coef)
###############################################################
###############################################################
plot(y~x, ylim=c(0,max(y)), pch=20) 
abline(v=getSmo(m1)$knots, col=gray(.9))
matplot(x,B, type="l", col="gray",lty=1,  add=T)
lines(fitted(m1)~x, lwd=2, col="blue")
##############################################################
##############################################################
#
# smoothing matrix
plot(y~x)
B <- Bbase(x)
dim(B)
attributes(B)
D2 <- diff(diag(23), diff=2)
dim(D2)
G <- t(D2)%*%D2
dim(G)
lambda <-100
beta <- solve(t(B)%*%B+lambda*G)%*%t(B)%*%y
S <- B%*%solve(t(B)%*%B+lambda*G)%*%t(B) 
sum(diag(S))# gegrees of freedom 

plot(beta)

###############################################################
###############################################################
# P-SPLINES pb() selection of smoothing parameters
###############################################################
###############################################################
library(gamlss)


p1 <- gamlss(bmi~pb(age, method="ML"), data=dbbmi)
p2 <- gamlss(bmi~pb(age, method="GCV") , data=dbbmi)
p3 <- gamlss(bmi~pb(age, method="GAIC", k=2) , data=dbbmi)
p4 <- gamlss(bmi~pb(age, method="GAIC", 
                    k=log(length(dbbmi$bmi))), 
             data=dbbmi)

plot(bmi~age , data=dbbmi, pch=20, col="gray")
lines(fitted(p1)~dbbmi$age, col=2, lwd=2) 
lines(fitted(p2)~dbbmi$age, col=3, lwd=2) 
lines(fitted(p2)~dbbmi$age, col=3, lwd=2) 
lines(fitted(p2)~dbbmi$age, col=5, lwd=2) 
###############################################################
###############################################################
# P-SPLINES WHICH CAN SHRINK TO A CONSTANT: zpb()
###############################################################
###############################################################
data(abdom)
# add a nuisance variable
abdom$x1 <- rNO(610, mu=5, sigma=5)
# fitting the original x
m0 <- gamlss(y~pb(x), data=abdom, trace=FALSE)
# fitting extra x1 with pb()
m1 <- gamlss(y~pb(x)+pb(x1), data=abdom, trace=FALSE)
# fitting extra x1 with pbz()
m2 <- gamlss(y~pbz(x)+pbz(x1), data=abdom, trace=FALSE)
# smaller deviance but liitle reduction in deviance
AIC(m0,m1,m2, k=2)
# the second term x1 is not needed but 
# m1 adds an extra degree of freedom
###############################################################
###############################################################
# MONOTONIC P-SPLINES: mpb()
###############################################################
###############################################################
library(gamlss)
#   Creating the data 
set.seed(1334)
x = seq(0, 1, length = 1000)
p = 0.4
y = sin(2 * pi * p * x) + rnorm(1000) * 0.1
plot(y~x, pch=20)
# fitting a monotonic curve going up
m1 <- gamlss(y~pbm(x), trace=FALSE)
plot(y~x, pch=20)
lines(fitted(m1)~x, col="red", lwd=2.5)
# fitting a monotonic curve going down
yy <- -y
plot(yy~x)
m2 <- gamlss(yy~pbm(x, mono="down"), trace=FALSE)
plot(yy~x, pch=20)
lines(fitted(m2)~x, col="red", lwd=2.5)
###############################################################
###############################################################
# CYCLING C P-SPLINES
###############################################################
###############################################################
###############################################################
# plot(y~x, ylim=c(0,max(y)), pch=20) 
# #abline(v=getSmo(m1)$knots, col=gray(.9))
# matplot(x,bs(x, 6), type="l", col="gray",lty=1,  add=T)
# 
# m1 <- gamlss(y~bs(x,6))
# lines(fitted(m1)~x, lwd=2, col="blue")
# matplot(x,bs(x))

# this is for an example how bs() is working

plotBS <- function(y, x, knots=6)
{
  plot(y~x, ylim=c(0,max(y)), pch=20, col=gray(.5)) 
  m1 <- gamlss(y~bs(x,knots), trace=F)
  matplot(x,bs(x, knots), type="l", col="gray",lty=1,  add=T)
  lines(fitted(m1)~x, lwd=2, col="blue")
}
op <- par(mfrow=c(2,2))
plotBS(y,x,3); title("(a) knots=3")
plotBS(y,x,7); title("(b) knots=7")
plotBS(y,x,10); title("(b) knots=10")
plotBS(y,x,20); title("(b) knots=20")
par(op)

