# suppress the warnings by setting warn=-1 
options(warn=-1) 
# Install if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gamlss", quietly = TRUE)) install.packages("gamlss")
if (!requireNamespace("gamlss.data", quietly = TRUE)) install.packages("gamlss.data")

# Load the packages
library(ggplot2)
library(gamlss)
library(gamlss.ggplots)
library(gamlss.add)
library(gamlss.data)

### (a) Read the data file by typing data(grip)into R. Note that the gamlss packages have to be downloaded first i.e. library(gamlss).
### (b) In order to select your individual sample a unique seed number will be given to you. (In the example below we use the seed number 243 for demonstration.)

# Load the data
data("grip", package = "gamlss.data")

set.seed(243) 
index <- sample(3766, 1000) 
mydata <- grip[index, ] 
dim(mydata)

# Sample 1000 observations
index <- sample(nrow(grip), 1000)
grip_sample <- grip[index, ]
dim(grip_sample)

### (c) Plot grip against age.  Note that there is no need to power transform the age in this data set. Explain why.

# Plot grip against age
plot(grip$age, grip$grip,
     xlab = "Age",
     ylab = "Grip Strength",
     main = "Grip Strength vs Age",
     col = "blue",
     pch = 19)

# Optional: Add a smooth line to highlight the trend
lines(smooth.spline(grip$age, grip$grip), col = "red")

# Plot grip against age
plot(grip_sample$age, grip_sample$grip,
     xlab = "Age",
     ylab = "Grip Strength",
     main = "Grip Strength vs Age",
     col = "blue",
     pch = 19)

# Optional: Add a smooth line to highlight the trend
lines(smooth.spline(grip_sample$age, grip_sample$grip), col = "red")

### (d) Use the LMS method to fit the data (You can simplify some of the steps below by using the gamlss package function lms() but you still have to justify the choice of the final model.)  That is, fit the BCCG distribution for grip.

# Fit the model
gbccg <- gamlss(grip ~ pb(age),
                sigma.fo = ~ pb(age),
                nu.fo = ~ pb(age),
                data = grip_sample,
                family = BCCG)

# Extract Effective Degrees of Freedom
edf_details <- edfAll(gbccg)

# Print the EDF for each parameter
print(edf_details)

### (e) Use the fitted values from the LMS model in (d) as starting values for fitting the BCT and the BCPE distributions to the data

# Fit the BCT model using gbccg as starting values
gbct <- gamlss(grip ~ pb(age),
               sigma.fo = ~ pb(age),
               nu.fo = ~ pb(age),
               tau.fo = ~ pb(age),
               data = grip_sample,
               family = BCT,
               start.from = gbccg)

# Fit the BCPE model using gbccg as starting values
gbcpe <- gamlss(grip ~ pb(age),
                sigma.fo = ~ pb(age),
                nu.fo = ~ pb(age),
                tau.fo = ~ pb(age),
                data = grip_sample,
                family = BCPE,
                start.from = gbccg)

# The EDF indicates the complexity of the model related to each parameter.

# Extract EDF for BCT model
edf_bct <- edfAll(gbct)

# Extract EDF for BCPE model
edf_bcpe <- edfAll(gbcpe)

# Print the EDF for each model
print(edf_bct)
print(edf_bcpe)

### (f) Use the generalised Akaike information criterion, GAIC, to compare the three models.

# Calculate GAIC for each model
gaic_bccg <- GAIC(gbccg)
gaic_gbct <- GAIC(gbct)
gaic_gbcpe <- GAIC(gbcpe)

# Print the GAIC values
print(paste("GAIC for BCCG:", gaic_bccg))
print(paste("GAIC for BCT:", gaic_gbct))
print(paste("GAIC for BCPE:", gaic_gbcpe))

### (g) Plot the fitted parameters for the fitted models in (d) and (e) using for example

fitted.plot(gbccg, gbct, x=da$age)

# Assuming 'grip_sample' is your dataset and 'gbccg' and 'gbct' are fitted models
fittedPlot(gbccg, gbct, x=grip_sample$age)

fittedPlot(gbccg, gbct, x=grip_sample$age)

# Calculate fitted values or centiles for each model across a range of ages
age_seq <- seq(min(grip_sample$age), max(grip_sample$age), length.out = 100)

# For BCCG Model
fitted_bccg <- predict(gbccg, newdata=data.frame(age=age_seq), type="response")

# For BCT Model
fitted_gbct <- predict(gbct, newdata=data.frame(age=age_seq), type="response")

# Plotting
plot(age_seq, fitted_bccg, type='l', col='blue', ylim=range(c(fitted_bccg, fitted_gbct)),
     xlab='Age', ylab='Fitted Grip Strength', main='Fitted Models Comparison')
lines(age_seq, fitted_gbct, col='red')

# Adding a legend
legend("topright", legend=c("BCCG", "BCT"), col=c("blue", "red"), lty=1, cex=0.8)

### (h) Obtain a centile plot for the fitted models in (d) and (e) using centiles() orcentiles.split() and compare them.

centilesTwo(gbccg, grid.x1 = age, grid.x2 = grip)
centiles(gbccg, xvar=grip_sample$age, cent=c(0.1, 0.4, 2,10,25,50,75,90,98,99.6, 99.9), ylab="grip", xlab="age", legend=FALSE)

# Plot centiles for BCCG model
plot(grip_sample$age, grip_sample$grip, col="gray90", main="Centile Comparison", xlab="Age", ylab="Grip Strength")

centiles(gbccg, xvar = grip_sample$age, col = "blue", lty = 1, cex=0.6, legend = TRUE, xlab="Age", ylab="Grip Strength",  main="Grip strength data \n @")

# Add centiles for BCT model to the existing plot
centilesTwo(gbct, xvar = grip_sample$age, col = "red", lty = 2, cex=0.2, legend = TRUE, xlab="Age", ylab="Grip Strength",  main="Grip strength data \n @")
# Update the legend to include BCT
#legend("topright", legend=c("BCCG", "BCT"), col=c("blue", "red"), lty=1:2, cex=0.8)

### (i) Investigate the residuals from the fitted models in (d) and (e) 

# For BCCG Model
plot(residuals(gbccg), main="Residuals for BCCG Model")

# For BCT Model
plot(residuals(gbct), main="Residuals for BCT Model")

# Assuming the 'gamlss' package is loaded

# For BCCG Model
wp(gbccg, main="Worm Plot for BCCG Model")

# For BCT Model
wp(gbct, main="Worm Plot for BCT Model")

# For BCCG Model
qstats_bccg <- Q.stats(gbccg)
print(qstats_bccg)

# For BCT Model
qstats_bct <- Q.stats(gbct)
