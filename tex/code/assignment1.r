library(gamlss.data)
library(MASS)
library(gamlss)
library(ggplot2)
library(gamlss.ggplots)

################################################################################
##
## (a) The original data, which contains all ages from zero to twenty two, 
##     exists in the gamlss.data package under the name of dbbmi. Each student 
##     should analyse a different age. Here we give an example how to analyse 
##     age 10. We first bring the data set in R and then create a subset 
#      data.frame containing only a specific age (here from 10-11). The 
##     following commands can be used:


# Load the dataset
data(dbbmi)
summary(dbbmi)

# Subset for ages 10 to 11
old <- 10
dbbmi_subset <- with(dbbmi, subset(dbbmi, age > old & age < old + 1))
bmi10 <- dbbmi_subset$bmi

# Plot the histogram
truehist(bmi10, nbins=30) # Adjust nbins as needed to make the histogram look good

density(bmi10, cut = 0)
gamlss.ggplots:::y_hist(bmi10)


################################################################################
### (b) Fit different parametric distributions to the data and choose an appropriate distribution to the data. Justify the choice of the distribution by explaining what you have done and why you select this specific distribution.

# m1 is the model with the lowest AIC: list the best 6 fits:
m1 <- fitDist(bmi, type=c('realplus'), data=dbbmi_subset, k=2)
m1$fit[1:6]
m1 <- fitDist(bmi, type=c('realline'), data=dbbmi_subset, k=2)
m1$fit[1:6]
m1 <- fitDist(bmi, type=c('realAll'), data=dbbmi_subset, k=2)
m1$fit[1:6]

c1 <- chooseDist(m1, type=c('realAll'), parallel="snow", ncpus=4)
c1


# Best fit is BCCG
m1 <- histDist(dbbmi_subset$bmi, "BCCG", density=TRUE, line.col=c(1,1), line.ty=c(1,2))
plot(m1)

m2 <- histDist(dbbmi_subset$bmi, "TF", density=TRUE, line.col=c(1,1), line.ty=c(1,2))
plot(m2)

m3 <- histDist(dbbmi_subset$bmi, "LOGNO", density=TRUE, line.col=c(1,1), line.ty=c(1,2))
plot(m3)

plot(function(x) dJSU(x, mu=16.8475, sigma=1.7560, nu=2.439, tau=2.6890), 10, 25, main = "The JSU  density mu=16.8475, sigma=0.7560, nu=2.439, tau=0.6890")


# Fit different distributions
fit_bccg <- gamlss(bmi10 ~ 1, family=BCCG)
fit_jsu <- gamlss(bmi10 ~ 1, family=JSU)
fit_tf <- gamlss(bmi10 ~ 1, family=TF)
fit_lognorm <- gamlss(bmi10 ~ 1, family=LOGNO)
fit_lo <- gamlss(bmi10 ~ 1, family=LO)
fit_pe <- gamlss(bmi10 ~ 1, family=PE)
fit_gamma <- gamlss(bmi10 ~ 1, family=GA)
fit_norm <- gamlss(bmi10 ~ 1, family=NO)
fit_wei <- gamlss(bmi10 ~ 1, family=WEI)
fit_gu <- gamlss(bmi10 ~ 1, family=GU)
fit_exp <- gamlss(bmi10 ~ 1, family=EXP)
fit_lg <- gamlss(bmi10 ~ 1, family=LG)


models <- list(fit_jsu, fit_tf, fit_lognorm, fit_lo, fit_pe, fit_gamma, fit_norm, fit_wei, fit_gu, fit_exp, fit_lg)

# Compare models
aic_values <- sapply(models, AIC)
print(aic_values)

# Select the model with the lowest AIC
selected_model <- models[[which.min(aic_values)]]
selected_model <- histDist(dbbmi_subset$bmi, "JSU", density=TRUE, line.col=c(1,1), line.ty=c(1,2))
plot(selected_model)

################################################################################
### (c) Output the parameter estimates for your chosen model 

# Output parameter estimates for the chosen model
summary(selected_model)
print(fit_jsu$mu.coefficients)

# Function to calculate the fitted density
density_fitted <- function(x) {
  dJSU(x, mu=fit_jsu$mu.coefficients, sigma=fit_jsu$sigma.coefficients, nu=fit_jsu$nu.coefficients, tau=fit_jsu$tau.coefficients)
}

# Histogram with ggplot
p <- ggplot(dbbmi_subset, aes(x=bmi10)) +
  geom_histogram(aes(y=..density..), binwidth = 0.5, colour="black", fill="white") 

# Add the fitted model curve
p <- p + stat_function(fun=density_fitted, colour="red", linewidth=1)
p


################################################################################
### Sample Code:

# Fit models with different distributions to find the best fit
# Replace GA, NO, and BI with the distributions you're interested in
# Example: GA (Gamma), NO (Normal), BI (Binomial)
models <- list(
  GA = gamlss(bmi10 ~ 1, family = GA, data = dbbmi_subset),
  NO = gamlss(bmi10 ~ 1, family = NO, data = dbbmi_subset),
  BCCG = gamlss(bmi10 ~ 1, family = BCCG, data = dbbmi_subset)
)

# Compare models using AIC
sapply(models, AIC)

# Proceed with the model having the lowest AIC value
# For demonstration, let's assume the Gamma distribution is chosen

# Model fitting with the selected distribution
final_model <- gamlss(bmi10 ~ 1, family = BCCG, data = dbbmi_subset)

# Variable selection using stepGAIC
final_model_step <- stepGAIC(final_model, method="backward")

# Model diagnostics
# Residual plots
plot(residuals(final_model_step), type="p") # Basic residual plot

# QQ plot for residuals
qqnorm(residuals(final_model_step))
qqline(residuals(final_model_step))

# Checking for influential observations
#plot(influence.measures(stepGAIC(final_model, method="simple"))$infmat)

# Adjust the model as needed based on diagnostics

