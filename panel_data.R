#https://cran.r-project.org/web/packages/panelvar/panelvar.pdf

# Load the required libraries
library(plm)
library(vars)
library(panelvar)
# # Load the panel data
# data("Grunfeld", package = "plm")
# 
# # Convert the data to a panel data format
# pdata <- pdata.frame(Grunfeld, index = c("firm", "year"))
# 
# # Estimate the Panel VAR model
# panel_var <- pvar(pdata, lag = 2, type = "const", season = NULL)
# 
# # Summary of the Panel VAR model
# summary(panel_var)
# panel_var
# 

data("ex3_abdata")
Andrews_Lu_MMSC(ex3_abdata)

data("ex1_dahlberg_data")
ex1_dahlberg_data_bs <- bootstrap_irf(ex1_dahlberg_data, typeof_irf = c("GIRF"),
                                      n.ahead = 8,
                                      nof_Nstar_draws = 500,
                                      confidence.band = 0.95,
                                      mc.cores = 100)

## End(Not run)
data("ex1_dahlberg_data")
ex1_dahlberg_data_girf <- girf(ex1_dahlberg_data, n.ahead = 8, ma_approx_steps= 8)
data("ex1_dahlberg_data_bs")
plot(ex1_dahlberg_data_girf, ex1_dahlberg_data_bs)
## End(Not run)
data("ex1_dahlberg_data")
coef(ex1_dahlberg_data)
data("ex1_dahlberg_data")
ex1_dahlberg_data_girf <- girf(ex1_dahlberg_data, n.ahead = 8, ma_approx_steps= 8)
data("ex1_dahlberg_data_bs")
plot(ex1_dahlberg_data_girf, ex1_dahlberg_data_bs)



data("ex1_dahlberg_data")

extract(ex1_dahlberg_data)

data("ex1_dahlberg_data")

fevd_orthogonal(ex1_dahlberg_data, n.ahead = 8)

fixedeffects(ex1_dahlberg_data)
girf(ex1_dahlberg_data, n.ahead = 8, ma_approx_steps= 8)

data("ex1_dahlberg_data")

oirf(ex1_dahlberg_data, n.ahead = 8)

pvalue(ex1_dahlberg_data)


data(Cigar)
ex1_feols <-
  pvarfeols(dependent_vars = c("log_sales", "log_price"),
            lags = 1,
            exog_vars = c("cpi"),
            transformation = "demean",
            data = Cigar,
            panel_identifier= c("state", "year"))

summary(ex1_feols)

data(abdata)
ex3_abdata <-pvargmm(
  dependent_vars = c("emp"),
  lags = 4,
  predet_vars = c("wage"),
  exog_vars = c("cap"),
  transformation = "fd",
  data = abdata,
  panel_identifier = c("id", "year"),
  steps = c("twostep"),
  system_instruments = TRUE,
  max_instr_dependent_vars = 99,
  max_instr_predet_vars = 99,
  min_instr_dependent_vars = 2L,
  min_instr_predet_vars = 1L,
  collapse = FALSE
)
## End(Not run)
data("ex3_abdata")
summary(ex3_abdata)
data("Dahlberg")
## Not run:
## Not run:
ex1_dahlberg_data <- pvargmm(dependent_vars = c("expenditures", "revenues", "grants"),
                             lags = 1,
                             transformation = "fod",
                             data = Dahlberg,
                             panel_identifier=c("id", "year"),
                             steps = c("twostep"),system_instruments = FALSE,
                             max_instr_dependent_vars = 99,
                             max_instr_predet_vars = 99,
                             min_instr_dependent_vars = 2L,
                             min_instr_predet_vars = 1L,
                             collapse = FALSE
)

## End(Not run)
data("ex1_dahlberg_data")

summary(ex1_dahlberg_data)

data(Dahlberg)

ex1_hk <-
  pvarhk(dependent_vars = c("expenditures", "revenues", "grants"),
         transformation = "demean",
         data = Dahlberg,
         panel_identifier= c("id", "year"))

summary(ex1_hk)

data("ex1_dahlberg_data")

residuals_level(ex1_dahlberg_data)

data("ex1_dahlberg_data")
se(ex1_dahlberg_data)

data("ex1_dahlberg_data")
stability_info <- stability(ex1_dahlberg_data)
print(stability_info)
plot(stability_info)



data("ex1_dahlberg_data") 
ex1_dahlberg_data <- pvargmm(dependent_vars = c("expenditures", "revenues", "grants"),
                             lags = 1,
                             transformation = "fod",
                             data = Dahlberg,
                             panel_identifier=c("id", "year"),
                             steps = c("twostep"),system_instruments = FALSE,
                             max_instr_dependent_vars = 99,
                             max_instr_predet_vars = 99,
                             min_instr_dependent_vars = 2L,
                             min_instr_predet_vars = 1L,
                             collapse = FALSE
) 

stability_info <- stability(ex1_dahlberg_data)

print(stability_info)

plot(stability_info)



