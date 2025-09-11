# Load the grunfeld data from the plm package
data("Grunfeld", package = "plm")

# Load the plm package for panel data analysis
library(plm)

# Compare pooled, fixed, and random effects models using the Grunfeld data
pooled_model <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
fixed_model <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
random_model <- plm(inv ~ value + capital, data = Grunfeld, model = "random")

# Summary of the pooled model
summary(pooled_model)

# Summary of the fixed effects model
summary(fixed_model)

# Summary of the random effects model
summary(random_model)

# To decide between the pooled, fixed effects, and random effects models, you
# can use specific tests designed for panel data analysis. Here are some
# commonly used tests:
#
# 1. Hausman Test: This test helps in choosing between the fixed effects and
# random effects models. If the p-value of the Hausman test is less than a
# chosen significance level (e.g., 0.05), you would reject the null hypothesis
# that the random effects model is consistent and efficient, and choose the
# fixed effects model.
#
# 2. Breusch-Pagan Test: This test is used to choose between the pooled and
# random effects models. If the p-value of the Breusch-Pagan test is less than a
# chosen significance level, you would reject the null hypothesis of no
# individual-specific effects and choose the random effects model.
#
# 3. F-Test for Poolability: This test is used to choose between the pooled and
# fixed effects models. If the p-value of the F-test for poolability is less
# than a chosen significance level, you would reject the null hypothesis of no
# individual-specific effects and choose the fixed effects model.
#
# Here's an example of how to perform these tests using the Grunfeld data:

# Perform the Hausman test to choose between fixed and random effects models
hausman_test <- phtest(fixed_model, random_model)
print(hausman_test)

# Perform the Breusch-Pagan test to choose between pooled and random effects models
bp_test <- phtest(pooled_model, random_model)
print(bp_test)

# Perform the F-test for poolability to choose between pooled and fixed effects models
poolability_test <- pFtest(pooled_model, fixed_model)
print(poolability_test)

# In addition to the specific tests mentioned, you can also consider the
# following details for each model:
#
# 1. Pooled Model:
#   - Assumes that all individual-specific effects are constant across individuals.
# - Efficient if individual-specific effects are absent or uncorrelated with the independent variables.
# - May lead to biased and inconsistent estimates if individual-specific effects are present and correlated with the independent variables.
#
# 2. Fixed Effects Model:
#   - Allows for individual-specific effects that are correlated with the independent variables.
# - Controls for time-invariant unobserved heterogeneity.
# - Inefficient if individual-specific effects are uncorrelated with the independent variables.
#
# 3. Random Effects Model:
#   - Assumes that individual-specific effects are uncorrelated with the independent variables.
# - More efficient than the fixed effects model if the assumption of uncorrelated individual-specific effects holds.
# - Inefficient and biased if individual-specific effects are correlated with the independent variables.

# When choosing between these models, it's important to consider the nature of
# the data, the assumptions of each model, and the results of the specific tests
# mentioned earlier. Additionally, you may want to assess the robustness of your
# results by comparing the models using different specifications and sensitivity
# analyses. 


# Calculate AIC and BIC for each model
AIC(pooled_model, fixed_model, random_model)
BIC(pooled_model, fixed_model, random_model)

# Diagnostic plots for the fixed effects model
plot(fixed_model)

# Breusch-Pagan test for heteroscedasticity
bptest(fixed_model)

# Normality test for residuals
shapiro.test(residuals(fixed_model))

# Calculate robust standard errors for the fixed effects model
library(lmtest)
coeftest(fixed_model, vcov = vcovHC)




# Cross-section dependence test -------------------------------------------

library(plm)
data("Grunfeld", package = "plm")
## test on heterogeneous model (separate time series regressions)
pcdtest(inv ~ value + capital, data = Grunfeld,
        index = c("firm", "year"))

## test on two-way fixed effects homogeneous model
pcdtest(inv ~ value + capital, data = Grunfeld, model = "within",
        effect = "twoways", index = c("firm", "year"))


## test on panel model object
g <- plm(inv ~ value + capital, data = Grunfeld, index = c("firm", "year"))
pcdtest(g)

## scaled LM test
pcdtest(g, test = "sclm")

## test on pseries
pGrunfeld <- pdata.frame(Grunfeld) 

pcdtest(pGrunfeld$value)

## local test
## define neighbours for individual 2: 1, 3, 4, 5 in lower triangular matrix
w <- matrix(0, ncol= 10, nrow=10)
w[2,1] <- w[3,2] <- w[4,2] <- w[5,2] <- 1
pcdtest(g, w = w)
w
??Grunfeld
