# You can install the released version of ARDL from CRAN:
# install.packages("ARDL")
# 
# # Or the latest development version from GitHub:
# install.packages("devtools")
# devtools::install_github("Natsiopoulos/ARDL")

# Load the package
library(tidyverse)
library(ARDL)
data(denmark)
models <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark, max_order = 5)

# The top 20 models according to the AIC

models$top_orders

ardl_3132 <- models$best_model 

ardl_3132$order

summary(ardl_3132)


# BIC ---------------------------------------------------------------------

# Using BIC as selection criterion instead of AIC
model1_b <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                      max_order = c(5,4,4,4), selection = "BIC")
model1_b$top_orders



## Add dummies or other variables that should stay fixed ---------------
# Using other criteria like adjusted R squared (the bigger the better)
adjr2 <- function(x) { summary(x)$adj.r.squared }
model1_adjr2 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                          max_order = c(5,4,4,4), selection = "adjr2",
                          selection_minmax = "max")
model1_adjr2$top_orders
# Using functions from other packages as selection criteria
if (requireNamespace("qpcR", quietly = TRUE)) {
  library(qpcR)
  model1_aicc <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                           max_order = c(5,4,4,4), selection = "AICc")
}
  model1_aicc$top_orders
adjr2 <- function(x){ Rsq.ad(x) }
  
model1_adjr2 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                            max_order = c(5,4,4,4), selection = "adjr2",
                            selection_minmax = "max")
  model1_adjr2$top_orders
model1_so <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                         max_order = c(5,4,4,4), starting_order = c(1,1,3,2))

  
  model1_so_3 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                           max_order = c(5,4,4,4), starting_order = c(3,1,3,2))
 
## Add constraints -----------------------------------------------------
# Restrict only the order of IBO to be 2
model1_ibo2 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                         max_order = c(5,4,4,4), fixed_order = c(-1,-1,2,-1))
model1_ibo2$top_orders
# Restrict the order of LRM to be 3 and the order of IBO to be 2
model1_lrm3_ibo2 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                              max_order = c(5,4,4,4), fixed_order = c(3,-1,2,-1))
model1_lrm3_ibo2$top_orders
## Set the starting date for the regression (data starts at "1974 Q1") -
# Set regression starting date to "1976 Q1"
model1_76q1 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                         max_order = c(5,4,4,4), start = "1976 Q1")
start(model1_76q1$best_model)



d_74Q1_75Q3 <- case_when(time(denmark) >= 1974 & time(denmark) <= 1975.5 ~ 1,
                         TRUE ~ 0)

## bind_cols() 
den <- bind_cols(denmark, d_74Q1_75Q3)

names(den)[names(den) == "...6"] <- "d_74Q1_75Q3"

# den <- cbind(denmark, d_74Q1_75Q3)
ardl_3132_d <- ardl(LRM ~ LRY + IBO + IDE | d_74Q1_75Q3,
                    data = den, order = c(3,1,3,2))

summary(ardl_3132_d)

compare <- data.frame(AIC = c(AIC(ardl_3132), AIC(ardl_3132_d)),
                      BIC = c(BIC(ardl_3132), BIC(ardl_3132_d)))

rownames(compare) <- c("no dummy", "with dummy")

compare

# Estimate an ARDL(3,1,3,2) model with a linear trend -----------------
ardl_3132_tr <- ardl(LRM ~ LRY + IBO + IDE + trend(LRM),
                     data = denmark, order = c(3,1,3,2))

summary(ardl_3132_tr)

## Subsample ARDL regression (start after 1975 Q4) ---------------------
ardl_3132_sub <- ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                      order = c(3,1,3,2), start = "1975 Q4")
# the date can also be setted as below
ardl_3132_sub2 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                       order = c(3,1,3,2), start = c(1975,4))
identical(ardl_3132_sub, ardl_3132_sub2)
summary(ardl_3132_sub)


## Add constraints -----------------------------------------------------
# Restrict only the order of IBO to be 2
model1_ibo2 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                         max_order = c(5,4,4,4), fixed_order = c(-1,-1,2,-1))
model1_ibo2$top_orders


#Then we can estimate the UECM (Unrestricted Error Correction Model) of the
#underlying ARDL(3,1,3,2).

uecm_3132 <- uecm(ardl_3132) 
summary(uecm_3132)







# And also the RECM (Restricted Error Correction Model) of the underlying
# ARDL(3,1,3,2), allowing the constant to join the long-run relationship (case
# 2), instead of the short-run (case 3).

recm_3132 <- recm(uecm_3132, case = 2)
summary(recm_3132)


#Let’s test if there is a long-run levels relationship (cointegration) using the
#bounds test from Pesaran et al. (2001).

# The bounds F-test (under the case 2) rejects the NULL hypothesis (let's say,
# assuming alpha = 0.01) with p-value = 0.004418. 

bounds_f_test(ardl_3132, case = 2)

# The bounds t-test (under the case 2) rejects the NULL hypothesis (let's say,
# assuming alpha = 0.01) with p-value = 0.005.

# We also provide the critical value bounds for alpha = 0.01.
tbounds <- bounds_t_test(uecm_3132, case = 3, alpha = 0.01)
tbounds

# Here is a more clear view of the main results.
tbounds$tab


#Here we have the short-run and the long-run multipliers (with standard errors,
#t-statistics and p-values).

multipliers(ardl_3132, type = "sr")


multipliers(ardl_3132)


#We can also estimate and visualize the delay multipliers along with their
#standard errors.

mult15 <- multipliers(ardl_3132, type = 15, se = TRUE) 

plot_delay(mult15, interval = 0.95)


#Now let’s graphically check the estimated long-run relationship (cointegrating
#equation) against the dependent variable LRM.

ce <- coint_eq(ardl_3132, case = 2)

plot_lr(ardl_3132, coint_eq = ce, show.legend = TRUE)


#Forecasting and using an ardl, uecm, or recm model in other functions are easy
#as they can be converted in regular lm models.

ardl_3132_lm <- to_lm(ardl_3132)

# Forecast using the in-sample data 

insample_data <- ardl_3132$model
predicted_values <- predict(ardl_3132_lm, newdata = insample_data)

# Convert to ts class for the plot
predicted_values <- ts(predicted_values, start = c(1974,4), frequency=4)
plot(denmark$LRM, lwd=2) #The input dependent variable
lines(predicted_values, col="red", lwd=2) #The predicted values 

ardl_model <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
# Without the ARDL package: (Using the dynlm package, because striving with the
# lm function would require extra data transformation to behave like
# time-series)

library(dynlm)
dynlm_ardl_model <- dynlm(LRM ~ LRY + IBO + IDE + L(LRM,1) + L(LRM,2) + L(LRM,3) +
                    L(LRY,1) + L(LRY,2) + L(LRY,3) + L(IBO,1) + L(IBO,2) +
                    L(IBO,3) + L(IDE,1) + L(IDE,2) + L(IDE,3), data = denmark)



identical(ardl_3132$coefficients, dynlm_ardl_model$coefficients)


#Using the ARDL package (literally one line of code):
  
 uecm_model <- uecm(ardl_3132)
# Without the ARDL package:
  
  dynlm_uecm_model <- dynlm(d(LRM) ~ L(LRM, 1) + L(LRY, 1) + L(IBO, 1) +
                              L(IDE, 1) + d(L(LRM, 1)) + d(L(LRM, 2)) +
                              d(LRY) + d(IBO) + d(L(IBO, 1)) + d(L(IBO, 2)) +
                              d(IDE) + d(L(IDE, 1)), data = denmark)
  
## Both models are identical
identical(uecm_model$coefficients, dynlm_uecm_model$coefficients)
  