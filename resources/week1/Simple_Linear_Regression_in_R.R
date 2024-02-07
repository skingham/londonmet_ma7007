
##=================================================
# Simple Linear Regression
##=================================================

## The data regarding the production of wheat in tons (x) and the price of the
## kilo of flour in pounds (y ) in a given country.

## To read x and y in R

x<-c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40)
x
y<-c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)
y

# To compute the correlation coefficient in R

r_xy<-cor(x,y)
r_xy

# To fit the regression model in R

model<-lm(y~x)

# To obtain the regression coefficients (the intercept and slope), and other measures

summary(model)

# To obtain the fitted values in R

residuals(lm(y~x))

# To perform stepwise variable selection in R

step(model)
 

layout(matrix(c(1,1,2,3),2,2,byrow=T))
# Wheat Production x Residuals Plot

plot(resid(model),
     main="Wheat Production x Residuals\nfor Simple Regression",
     xlab="Wheat Production", ylab="Residuals")
abline(h=0,lty=2)

# Histogram of Residuals

hist(model$resid, main="Histogram of Residuals",
     ylab="Residuals")

## Q-Q Plot

qqnorm(model$resid)
qqline(model$resid)

## To read x and y in R

x<-c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40)
x
y<-c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)
y

# To normalize x and y in R

x1<-rnorm(x)
x1
y1<rnorm(y)
y1

# To obtain a scattered plot in R

plot(x1,y1)

# To fit the regression line (line of best fit) in R

abline(lm(y1~x1))

# Plot the histogram for y1

hist(y1, main = "Normal DIstribution")

# To perform normality test in R: Shapiro-Wilk

shapiro.test(x1)

shapiro.test(y1)
