library(haven)
library(tidyverse)
library(xts)
library(forecast)
library(urca)

samon <- read_dta("data/samon.dta")

samon |> dim()
samon |> glimpse()

samon_sm <- samon |> select(1:33)  ## Lets say
samon_sm |> glimpse()  ## but two additional are selected

samon_sm <- samon_sm |> select(1:31)

# Load the required libraries
library(plm)
library(vars)


# Convert the data to a panel data format
pdata <- pdata.frame(samon_sm, index = c("years", "countrycode"))

pdata |> dim()

pdata <- pdata |>  na.omit() 
result <- purtest( gdpconst2015~ 1, data = pdata, test = "levinlin")

result <- purtest(gdpconst2015 ~ 1, data = pdata, test = "adf")


adf_test <- plm(gdpconst2015 ~ 1, data = pdata, model = "pooling", effect = "individual", test = "adf")
summary(adf_test)




model <- pgmm(gdpconst2015 ~ lag(gdpconst2015, 1) + inflation + ppp, 
              data = pdata, model = "twosteps")



model <- pgmm(formula = gdpconst2015 ~ lag(gdpconst2015, 1) + inflation + ppp,
              data = pdata, model = "twosteps")



# Specify the lag structure and instruments if needed
model <- pgmm(y ~ lag(y, 1) + x1 + x2, data = pdata, model = "twosteps", lags = 1, endog = "lag(y, 1)")

# View model summary
summary(model)











panel_data <- data.frame(time = c(1, 2, 3, 1, 2, 3),
                         country = c("A", "A", "A", "B", "B", "B"),
                         variable = c(1.2, 1.5, 1.8, 2.1, 2.4, 2.7))

# Create a pdata.frame object from your panel data
pdata <- pdata.frame(panel_data, index = c("country", "time"))

# Perform panel unit root testing using the purtest function with a reduced lag value
result <- purtest(variable ~ 1, data = pdata, test = "levinlin", lag = 2)

# Extract and interpret the test results
print(result)
