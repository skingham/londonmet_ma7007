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
library(MASS)

# Load the dataset
data(dbbmi)
summary(dbbmi)

# Subset for ages 15 to 16
old <- 15
dbbmi_15 <- with(dbbmi, subset(dbbmi, age > old & age < old + 1))
bmi15 <- dbbmi_15$bmi

# Plot the histogram; adjust nbins as needed to make the histogram look good
binwith = 0.5
nbins = trunc(max(bmi15) - min(bmi15)) / binwith

truehist(bmi15, nbins=nbins) # A

density(bmi15, cut = 0)

gamlss.ggplots:::y_hist(dbbmi_15$bmi,
                        from=floor(min(bmi15)), 
                        to=ceiling(max(bmi15)), 
                        binwidth=binwith,
                        title="Histogram of BMI for 15 year olds")

# (b) Fit different parametric distributions to the data and choose an appropriate distribution to the data. Justify the choice of the distribution by explaining what you have done and why you select this specific distribution.

# m1 is the model with the lowest AIC: list the best 6 fits:
#m1 <- fitDist(bmi, type=c('realplus'), data=dbbmi_15, k=2)
#m1$fit[1:6]
#m1 <- fitDist(bmi, type=c('realline'), data=dbbmi_15, k=2)
#m1$fit[1:6]
m1 <- fitDist(bmi, type='realAll', data=dbbmi_15, k=2)
m1$fit[1:6]

m1 <- histDist(bmi, "exGAUS", density=TRUE, line.col=c(1,1), line.ty=c(1,2), nbins=nbins, data=dbbmi_15)
plot(m1)

m2 <- histDist(bmi, "BCPEo", density=TRUE, line.col=c(1,1), line.ty=c(1,2), nbins=nbins, data=dbbmi_15)
plot(m2)

m3 <- histDist(bmi, "BCPE", density=TRUE, line.col=c(1,1), line.ty=c(1,2), nbins=nbins, data=dbbmi_15)
plot(m3)

m1 <- gamlss(bmi~1, family=NO, data=dbbmi_15)
c1 <- chooseDist(m1, type='realAll', data=dbbmi_15, parallel="snow", ncpus=4)
c1

# Best fit is exGAUS
m1 <- histDist(bmi, "exGAUS", density=TRUE, line.col=c(1,1), line.ty=c(1,2), nbins=nbins, data=dbbmi_15)
plot(m1)

m2 <- histDist(bmi, "BCPEo", density=TRUE, line.col=c(1,1), line.ty=c(1,2), nbins=nbins, data=dbbmi_15)
plot(m2)

m3 <- histDist(bmi, "TF", density=TRUE, line.col=c(1,1), line.ty=c(1,2), nbins=nbins, data=dbbmi_15)
plot(m3)

plot(function(x) dJSU(x, mu=16.8475, sigma=1.7560, nu=2.439, tau=2.6890), 10, 25, main = "The JSU  density mu=16.8475, sigma=0.7560, nu=2.439, tau=0.6890")

#print(dJSU(x, mu=16.8475, sigma=1.7560, nu=2.439, tau=2.742))

# Fit different distributions
fit_bccg <- gamlss(bmi15 ~ 1, family=BCCG)
fit_jsu <- gamlss(bmi15 ~ 1, family=JSU)
fit_tf <- gamlss(bmi15 ~ 1, family=TF)
fit_lognorm <- gamlss(bmi15 ~ 1, family=LOGNO)
fit_lo <- gamlss(bmi15 ~ 1, family=LO)
fit_pe <- gamlss(bmi15 ~ 1, family=PE)
fit_gamma <- gamlss(bmi15 ~ 1, family=GA)
fit_norm <- gamlss(bmi15 ~ 1, family=NO)
fit_wei <- gamlss(bmi15 ~ 1, family=WEI)
fit_gu <- gamlss(bmi15 ~ 1, family=GU)
fit_exp <- gamlss(bmi15 ~ 1, family=EXP)
fit_lg <- gamlss(bmi15 ~ 1, family=LG)


models <- list(fit_jsu, fit_tf, fit_lognorm, fit_lo, fit_pe, fit_gamma, fit_norm, fit_wei, fit_gu, fit_exp, fit_lg)

# Compare models
aic_values <- sapply(models, AIC)
print(aic_values)

# Select the model with the lowest AIC
selected_model <- models[[which.min(aic_values)]]
selected_model <- histDist(dbbmi_15$bmi, "JSU", density=TRUE, line.col=c(1,1), line.ty=c(1,2))
plot(selected_model)

# (c) Output the parameter estimates for your chosen model 
summary(selected_model)
print(fit_jsu$mu.coefficients)

 # Function to calculate the fitted density
 density_fitted <- function(x) {
   dJSU(x, mu=fit_jsu$mu.coefficients, sigma=fit_jsu$sigma.coefficients, nu=fit_jsu$nu.coefficients, tau=fit_jsu$tau.coefficients)
 }
 
 # Histogram with ggplot
 p <- ggplot(dbbmi_15, aes(x=bmi15)) +
   geom_histogram(aes(y=..density..), binwidth = 0.5, colour="black", fill="white") 
 
 # Add the fitted model curve
 p <- p + stat_function(fun=density_fitted, colour="red", linewidth=1)
 p

 
################################################################################
# Model Selection Week 8
 
# Broad outline of approach for Q3
 
### Determining an Appropriate Distribution
 
# 1. Exploratory Data Analysis (EDA): Start with EDA to understand the distribution of your response variable and the relationships between explanatory variables and the response. This can include plotting histograms, box plots, and scatter plots.
# 2. Distribution Selection: Based on the EDA, you can hypothesize which distributions might fit your response variable. GAMLSS supports a wide range of distributions, so consider whether your data suggests normality, skewness, kurtosis, or zero-inflation which could guide your choice towards distributions like Gaussian, Binomial, Poisson, Negative Binomial, etc.
# 3. Fit Multiple Models: Initially, fit models with different distributions to your data. For instance, if you're unsure whether your data are Poisson or Negative Binomial distributed, fit both models.
# 4. Distribution Comparison: Use criteria like the Akaike Information Criterion (AIC), Bayesian Information Criterion (BIC), or Generalized Akaike Information Criterion (GAIC) to compare models with different distributions. Lower values typically indicate a better fit to the data.

### Selecting Relevant Explanatory Variables
# 1. Univariate Analysis: Start by fitting simple models that include the response variable and one explanatory variable at a time. This can help identify variables that have no significant relationship with the response.
# 2. Multivariate Analysis: Include all significant variables in a multivariate GAMLSS model. Be mindful of multicollinearity among explanatory variables, as it can affect model estimates.
# 3. Variable Selection Techniques: Utilize backward elimination, forward selection, or both in a stepwise fashion to select relevant variables. Functions like stepGAIC can automate this process based on information criteria.

### Model Diagnostics
# 1. Residual Analysis: Check the residuals of your fitted model for any patterns or systematic deviations from assumptions. Plots like residual vs. fitted values or Q-Q plots are useful.
# 2. Check for Overdispersion: Particularly for count data, ensure that the model accounts for overdispersion if present. This might influence your choice of distribution.
# 3. Influence Measures: Assess the influence of individual data points on your model using diagnostic measures like Cook’s distance.
# 4. Model Fit Statistics: Beyond AIC or BIC, consider using goodness-of-fit tests or R-squared analogs for GAMLSS models to evaluate how well your model explains the data.

### Putting It All Together
# 1. Iteration: Model building is an iterative process. Based on diagnostics and fit statistics, you may need to revisit your choice of distribution, the set of explanatory variables, or even transform variables to meet model assumptions.
# 2. Validation: Use cross-validation or split your dataset into training and test sets to validate your model's predictive performance on unseen data.

### Sample Code:
# Fit models with different distributions to find the best fit
# Replace GA, NO, and BI with the distributions you're interested in
# Example: GA (Gamma), NO (Normal), BI (Binomial)
models <- list(
   GA = gamlss(bmi15 ~ 1, family = GA, data = dbbmi_15),
   NO = gamlss(bmi15 ~ 1, family = NO, data = dbbmi_15),
   BCCG = gamlss(bmi15 ~ 1, family = BCCG, data = dbbmi_15)
)
 
# Compare models using AIC
sapply(models, AIC)

# Proceed with the model having the lowest AIC value
# For demonstration, let's assume the Gamma distribution is chosen
 
# Model fitting with the selected distribution
final_model <- gamlss(bmi15 ~ 1, family = BCCG, data = dbbmi_15)
 
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
