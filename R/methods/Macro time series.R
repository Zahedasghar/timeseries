---
title: "Introduction to Macroeconomic Time Series"
subtitle: "EC614: Macroeconomic Forecasting"
author: "Prof. Dr. Zahid Asghar"
institute: "School of Economics, Quaid-i-Azam University"
date: "Week 1"
format:
  revealjs:
    theme: [default, custom.css]
    logo: images/qau_logo.png
    footer: "EC614 - Macroeconomic Forecasting"
    slide-number: true
    chalkboard: true
    multiplex: true
    code-fold: true
    code-tools: true
execute:
  echo: true
  warning: false
  message: false
---

```{r setup, include=FALSE}
library(tidyverse)
library(forecast)
library(fable)
library(tsibble)
library(lubridate)
library(plotly)
library(kableExtra)
library(scales)

# Set theme for plots
theme_set(theme_minimal(base_size = 12))
```

## Course Overview {background-color="#1f4e79"}

::: {.columns}
::: {.column width="30%"}
![](images/zahid.png){width="300"}
:::
::: {.column width="70%"}
**EC614: Macroeconomic Forecasting**

Research in economics requires sophisticated quantitative skills. This course emphasizes the **multivariate nature of economic systems** and modern forecasting methods.

**Core Focus**: VAR, SVAR, Cointegration, VECM, Panel Data
:::
:::

---

## Learning Objectives

By the end of this course, you will:

- Master univariate time series as foundation for multivariate modeling
- **Build and estimate VAR models** for policy analysis
- **Apply structural identification in SVAR models**
- **Test for and model long-run relationships** using cointegration
- **Estimate Vector Error Correction Models (VECM)**
- **Apply panel data methods** for cross-country forecasting
- Integrate machine learning with traditional econometrics
- Conduct central bank-style nowcasting and policy analysis

---

## What is Macroeconomic Forecasting? {background-color="#2d5aa0"}

> Macroeconomic forecasting is the art and science of predicting future values of economic aggregates using historical data, economic theory, and statistical methods.

**Key Applications:**
- Central bank monetary policy decisions
- Government fiscal planning
- Business investment decisions
- International organizations' economic outlook

---

## Why Does Forecasting Matter?

::: {.incremental}
- **Policy Making**: Central banks need inflation and GDP forecasts
- **Business Planning**: Investment and hiring decisions
- **Risk Management**: Financial institutions assess economic risks
- **Academic Research**: Testing economic theories
- **Public Debate**: Economic forecasts shape political discourse
:::

::: {.notes}
Forecasting is fundamental to economic decision-making at all levels.
:::

---

## Time Series vs Cross-Sectional Data

::: {.columns}
::: {.column width="50%"}
**Time Series Data**
- Same variables observed over time
- Order matters (temporal dependence)
- Examples: GDP 1980-2023, Monthly inflation
- **Focus of this course**
:::
::: {.column width="50%"}
**Cross-Sectional Data**
- Different units at same time point
- Order doesn't matter
- Examples: Income across households in 2023
- Static relationships
:::
:::

---

## The Four Components of Time Series {background-color="#2d5aa0"}

Every economic time series can be decomposed into:

1. **Trend** - Long-run movement
2. **Seasonal** - Regular patterns within a year
3. **Cyclical** - Irregular fluctuations (business cycles)
4. **Irregular (Random)** - Unpredictable movements

$$Y_t = \text{Trend}_t + \text{Seasonal}_t + \text{Cyclical}_t + \text{Irregular}_t$$

---

## Pakistan's GDP Growth: A Time Series Perspective

```{r gdp-example}
#| echo: false
#| fig-width: 10
#| fig-height: 6

# Create sample Pakistan GDP data
years <- 1980:2023
set.seed(123)
trend <- 0.05 - 0.001 * (years - 1980) # Declining trend
cycle <- 0.02 * sin(2 * pi * (years - 1980) / 8) # 8-year cycle
irregular <- rnorm(length(years), 0, 0.015)
gdp_growth <- trend + cycle + irregular

pakistan_gdp <- tibble(
  year = years,
  gdp_growth = gdp_growth * 100
)

ggplot(pakistan_gdp, aes(x = year, y = gdp_growth)) +
  geom_line(color = "#1f4e79", size = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#d62728", alpha = 0.3) +
  labs(
    title = "Pakistan GDP Growth Rate (1980-2023)",
    subtitle = "Showing trend, cycles, and irregular movements",
    x = "Year",
    y = "GDP Growth Rate (%)",
    caption = "Source: Stylized data for illustration"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
```

---

## Key Macroeconomic Variables We'll Study

::: {.columns}
::: {.column width="50%"}
**Domestic Variables**
- GDP growth
- Inflation (CPI, core)
- Interest rates
- Exchange rate
- Money supply (M2)
- Unemployment
:::
::: {.column width="50%"}
**External Variables**
- Oil prices
- Global growth
- Capital flows
- Terms of trade
- Foreign reserves
:::
:::

**Why these matter**: They form the core of central bank forecasting models

---

## Data Sources for Pakistani Economy {background-color="#2d5aa0"}

::: {.columns}
::: {.column width="50%"}
**Official Sources**
- State Bank of Pakistan (SBP)
- Pakistan Bureau of Statistics (PBS)
- Ministry of Finance
- Securities & Exchange Commission
:::
::: {.column width="50%"}
**International Sources**
- World Bank (WDI)
- IMF (IFS, WEO)
- OECD databases
- Federal Reserve Economic Data (FRED)
:::
:::

---

## Accessing Data with R

```{r data-demo}
#| echo: true
#| eval: false

# Using WDI package for World Bank data
library(WDI)

# Get Pakistan GDP and inflation data
pak_data <- WDI(
  country = "PK",
  indicator = c("NY.GDP.MKTP.KD.ZG", "FP.CPI.TOTL.ZG"),
  start = 2000,
  end = 2023
)

# Using quantmod for financial data
library(quantmod)
getSymbols("PKR=X", src = "yahoo") # USD/PKR exchange rate
```

---

## Types of Time Series Patterns

```{r patterns-demo}
#| echo: false
#| fig-width: 12
#| fig-height: 8

# Create different time series patterns
t <- 1:100
set.seed(456)

# Trend
trend_series <- 100 + 0.5 * t + rnorm(100, 0, 2)

# Seasonal
seasonal_series <- 50 + 10 * sin(2 * pi * t / 12) + rnorm(100, 0, 1)

# Cyclical
cyclical_series <- 30 + 5 * sin(2 * pi * t / 25) + rnorm(100, 0, 1.5)

# Random walk
random_walk <- cumsum(rnorm(100, 0, 1))

patterns_data <- tibble(
  time = rep(t, 4),
  value = c(trend_series, seasonal_series, cyclical_series, random_walk),
  pattern = rep(c("Trend", "Seasonal", "Cyclical", "Random Walk"), each = 100)
)

ggplot(patterns_data, aes(x = time, y = value)) +
  geom_line(color = "#1f4e79", size = 0.8) +
  facet_wrap(~pattern, scales = "free_y", ncol = 2) +
  labs(
    title = "Common Time Series Patterns in Economics",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))
```

---

## Stationarity: A Fundamental Concept {background-color="#2d5aa0"}

**Stationary Time Series**: Statistical properties don't change over time

- Constant mean: $E[Y_t] = \mu$ for all $t$
- Constant variance: $Var[Y_t] = \sigma^2$ for all $t$
- Covariance depends only on lag: $Cov[Y_t, Y_{t-k}] = \gamma_k$

**Why it matters**: Most econometric methods assume stationarity

---

## Stationary vs Non-Stationary

```{r stationarity-demo}
#| echo: false
#| fig-width: 12
#| fig-height: 6

set.seed(789)
t <- 1:200

# Stationary series (AR(1) with |phi| < 1)
stationary <- arima.sim(list(ar = 0.7), n = 200)

# Non-stationary series (random walk)
non_stationary <- cumsum(rnorm(200, 0, 1))

stationarity_data <- tibble(
  time = rep(t, 2),
  value = c(stationary, non_stationary),
  series = rep(c("Stationary Process", "Non-Stationary (Random Walk)"), each = 200)
)

ggplot(stationarity_data, aes(x = time, y = value)) +
  geom_line(color = "#1f4e79", size = 0.8) +
  facet_wrap(~series, scales = "free_y", ncol = 1) +
  labs(
    title = "Stationary vs Non-Stationary Time Series",
    subtitle = "Notice the different behavior patterns",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))
```

---

## Course Structure and Progression

::: {.incremental}
**Weeks 1-5**: Univariate Foundation
- Time series properties, ARIMA models, structural breaks

**Weeks 6-12**: Core Multivariate Methods (Heart of the course)
- VAR, SVAR, Cointegration, VECM, Panel Data

**Weeks 13-15**: Modern Extensions
- Machine learning, nowcasting, policy applications
:::

---

## Required Textbooks {background-color="#2d5aa0"}

::: {.columns}
::: {.column width="33%"}
![FPP3](images/fpp3.png)

**Primary Text**  
Forecasting: Principles and Practice (3rd ed)
:::
::: {.column width="33%"}
![Enders](images/applied_econ.jpg)

**Technical Reference**  
Applied Econometric Time Series (4th ed)
:::
::: {.column width="33%"}
![Doing Economics](images/doing_economics.png)

**Practical Applications**  
Doing Economics (Selected Chapters)
:::
:::

---

## Software Tools We'll Use

::: {.columns}
::: {.column width="50%"}
**Primary: R**
- `fable` - Modern forecasting framework
- `forecast` - Classical time series methods
- `vars` - VAR modeling
- `urca` - Unit root and cointegration tests
- `tidyverse` - Data manipulation and visualization
:::
::: {.column width="50%"}
**Supporting Tools**
- RStudio/Posit Cloud
- Quarto for reproducible reports
- Git/GitHub for version control
- Optional: EViews, Stata for comparison
:::
:::

---

## Why R for Macroeconomic Forecasting?

::: {.incremental}
- **Free and open source** - No licensing costs
- **Cutting-edge methods** - Latest research implemented first in R
- **Reproducible research** - Code + results in same document
- **Large community** - Extensive help and packages
- **Integration** - Works with Python, databases, web APIs
- **Central bank adoption** - Many central banks use R
:::

---

## Our First Lab: Exploring Economic Data

**Today's Lab Objectives**:
1. Load and visualize GDP, inflation, employment data
2. Identify time series components
3. Basic data manipulation with `tidyverse`
4. Create publication-quality plots
5. Calculate simple summary statistics

**Data Sources**: SBP, PBS, World Bank indicators for Pakistan

---

## Lab Demo: Loading Pakistani Data

```{r lab-demo}
#| echo: true
#| eval: false

# Load required packages
library(tidyverse)
library(WDI)
library(lubridate)

# Get Pakistan macroeconomic data
pak_indicators <- WDI(
  country = "PK",
  indicator = c(
    gdp_growth = "NY.GDP.MKTP.KD.ZG",
    inflation = "FP.CPI.TOTL.ZG",
    unemployment = "SL.UEM.TOTL.ZS"
  ),
  start = 2000,
  end = 2023
)

# Basic exploration
glimpse(pak_indicators)
summary(pak_indicators)
```

---

## Assessment Structure

| Component | Weight | Description |
|-----------|---------|-------------|
| **Midterm Exam** | 25% | Univariate methods, theory |
| **Final Exam** | 30% | Comprehensive, multivariate focus |
| **Lab Assignments** | 20% | 5 assignments × 4% |
| **Research Project** | 20% | Original forecasting study |
| **Participation** | 5% | Class engagement |

---

## Research Project Options {background-color="#2d5aa0"}

Choose one macroeconomic forecasting application:

- **Monetary policy transmission analysis** using SVAR
- **International business cycle synchronization** using panel VAR
- **Exchange rate dynamics and PPP** using VECM
- **Inflation forecasting** using factor-augmented VAR
- **Central bank nowcasting** with mixed-frequency data
- **Cross-country growth convergence** using panel cointegration

---

## Today's Key Takeaways

::: {.incremental}
1. **Macroeconomic forecasting** combines theory, data, and statistical methods
2. **Time series data** has unique properties requiring specialized techniques
3. **Four components**: trend, seasonal, cyclical, irregular
4. **Stationarity** is fundamental to most econometric methods
5. **This course progresses** from univariate to multivariate methods
6. **R provides** a comprehensive toolkit for modern forecasting
:::

---

## Next Week Preview

**Week 2: Univariate Time Series Foundation Methods**

- Classical decomposition techniques
- STL (Seasonal and Trend decomposition using Loess)
- Exponential smoothing family
- Seasonal adjustment of macroeconomic variables
- Applications to CPI and industrial production data

**Reading**: FPP3 Chapters 3-8

---

## Homework for Next Week

1. **Install R and RStudio** (or set up Posit Cloud account)
2. **Read FPP3 Chapter 1** - Getting started
3. **Install required packages**:
   ```r
   install.packages(c("tidyverse", "forecast", "fable", 
                      "tsibble", "WDI", "quantmod"))
   ```
4. **Bring laptop** to next class for hands-on lab session
5. **Optional**: Browse SBP and PBS websites to familiarize yourself with Pakistani economic data

---

## Questions and Discussion {background-color="#1f4e79"}

::: {.r-fit-text}
**"The best way to learn forecasting is by forecasting"**

*- Rob J. Hyndman*
:::

**Office Hours**: Tuesday & Thursday 2-4 PM  
**Email**: zahid.asghar@qau.edu.pk  
**Course Website**: zahidasghar.com/courses

---

## References and Further Reading

**Key Papers**:
- Sims, C. A. (1980). "Macroeconomics and Reality." *Econometrica*, 48(1), 1-48.
- Stock, J. H., & Watson, M. W. (2007). "Why has US inflation become harder to forecast?" *Journal of Money, Credit and Banking*, 39, 3-33.

**Online Resources**:
- [Forecasting: Principles and Practice](https://otexts.com/fpp3/)
- [R for Data Science](https://r4ds.had.co.nz/)
- [Federal Reserve Economic Data (FRED)](https://fred.stlouisfed.org/)

**Pakistani Economic Data**:
- [State Bank of Pakistan](https://www.sbp.org.pk/)
- [Pakistan Bureau of Statistics](https://www.pbs.gov.pk/)