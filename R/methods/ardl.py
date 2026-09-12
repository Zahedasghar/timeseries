# Import libraries ----
import pandas as pd # data manipulation
import numpy as np # numerical operations
import matplotlib.pyplot as plt # plotting
import seaborn as sns # statistical data visualization
from scipy import stats # statistical functions
from statsmodels.tsa.stattools import adfuller, kpss # unit root tests
from statsmodels.tsa.ardl import ARDL, ardl_select_order # ARDL modeling
from statsmodels.stats.diagnostic import acorr_ljungbox # Ljung-Box test
import warnings # to manage warnings
warnings.filterwarnings('ignore') # ignore warnings for cleaner output

# Set display options
pd.set_option('display.max_columns', None) # show all columns
pd.set_option('display.width', None) # no limit on display width
pd.set_option('display.max_rows', 100) # show up to 100 rows

# Import and clean data ----
ardlf = pd.read_excel("data/ardl.xlsx", skiprows=3)

# Clean column names (convert to lowercase and replace spaces with underscores)
ardlf.columns = ardlf.columns.str.lower().str.replace(' ', '_').str.replace('[^a-z0-9_]', '', regex=True).str.rstrip('_')

# Format date
ardlf['date'] = pd.to_datetime(ardlf['date'])
ardlf.set_index('date', inplace=True)

# Display data structure
print(ardlf.info())
print(ardlf.head())

# Descriptive statistics ----
variables = ['tri', 'annualised_volatility_open', 'beta_monthly_leveraged', 
             'dy', 'market_cap', 'pe', 'liqudity'] 

desc_stats = ardlf[variables].agg(['count', 'mean', 'var', 'min', 'max']).T
desc_stats.columns = ['Observations', 'Mean', 'Variance', 'Min', 'Max']
desc_stats = desc_stats.round(2)

print("\nDescriptive Statistics:")
print(desc_stats)

# Create styled table (optional - requires Great Tables or similar)
# desc_stats.style.format(precision=2)

# Correlation matrix ----
correlation_matrix = ardlf[variables].corr()

print("\nCorrelation Matrix:")
print(correlation_matrix.round(2))

# Visualize correlation matrix
plt.figure(figsize=(10, 8))
sns.heatmap(correlation_matrix, annot=True, fmt='.2f', cmap='coolwarm', 
            center=0, square=True, linewidths=1)
plt.title('Correlation Matrix')
plt.tight_layout()
plt.show()

# Log transformations ----
ardlf['ln_annualised_vol_open'] = np.log(ardlf['annualised_volatility_open'])
ardlf['ln_mkt_cap'] = np.log(ardlf['market_cap'])
ardlf['lntri'] = np.log(ardlf['tri'])

# Unit root tests ----
def adf_test(series, name):
    """Perform Augmented Dickey-Fuller test"""
    result = adfuller(series.dropna(), autolag='AIC')
    return {
        'Variable': name,
        'ADF_Statistic': result[0],
        'ADF_pvalue': result[1],
        'Lags_Used': result[2],
        'Observations': result[3]
    }

def pp_test(series, name):
    """Perform Phillips-Perron test (approximation using ADF with different lag selection)"""
    result = adfuller(series.dropna(), regression='c', autolag=None, maxlag=1)
    return {
        'Variable': name,
        'PP_Statistic': result[0],
        'PP_pvalue': result[1]
    }

# Variables for unit root testing
test_vars = ['ln_annualised_vol_open', 'lntri', 'ln_mkt_cap', 'pe', 'dy', 
             'beta_monthly_leveraged', 'liqudity']

# ADF tests
adf_results = []
for var in test_vars:
    adf_results.append(adf_test(ardlf[var], var))

adf_df = pd.DataFrame(adf_results)

# PP tests (approximation)
pp_results = []
for var in test_vars:
    pp_results.append(pp_test(ardlf[var], var))

pp_df = pd.DataFrame(pp_results)

# Combine results
unit_root_tests = pd.merge(
    adf_df[['Variable', 'ADF_Statistic', 'ADF_pvalue']], 
    pp_df[['Variable', 'PP_Statistic', 'PP_pvalue']], 
    on='Variable'
)

print("\nUnit Root Tests:")
print(unit_root_tests.round(4))

# ARDL modeling ----
# Prepare data for ARDL
ardl_data = ardlf[['lntri', 'ln_annualised_vol_open', 'ln_mkt_cap', 
                   'pe', 'dy', 'beta_monthly_leveraged', 'liqudity']].dropna()

# Select optimal ARDL order
print("\nSelecting optimal ARDL model...")
ardl_selection = ardl_select_order(
    endog=ardl_data['lntri'],
    maxlag=4,
    maxorder=4,  # Added: maximum lag for exogenous variables
    exog=ardl_data[['ln_annualised_vol_open', 'ln_mkt_cap', 'pe', 
                    'dy', 'beta_monthly_leveraged', 'liqudity']],
    ic='aic',
    trend='c'
)

print("\nOptimal ARDL Order:")
print(ardl_selection.model.ardl_order)

# Fit best ARDL model
ardl_model = ardl_selection.model
ardl_fit = ardl_model.fit()

print("\nARDL Model Summary:")
print(ardl_fit.summary())

# Long-run coefficients
print("\nLong-run Coefficients:")
print(ardl_fit.params)

# Bounds test for cointegration
print("\nBounds Test for Cointegration:")
bounds_test = ardl_fit.bounds_test(case=2, trend='c')
print(bounds_test)

# Time series plots ----
fig, axes = plt.subplots(4, 2, figsize=(15, 12))
fig.suptitle('Time Series Plots', fontsize=16)

plot_vars = [
    ('tri', 'TRI'),
    ('amihud_illiqudity', 'Amihud Illiquidity'),
    ('turn_over_ratio', 'Turnover Ratio'),
    ('bid_ask_spread', 'Bid-Ask Spread'),
    ('beta_monthly_leveraged', 'Beta Monthly Leveraged'),
    ('dy', 'Dividend Yield'),
    ('market_cap', 'Market Cap'),
    ('annualised_volatility_open', 'Annualized Volatility')
]

for idx, (var, title) in enumerate(plot_vars):
    ax = axes[idx // 2, idx % 2]
    if var in ardlf.columns:
        ardlf[var].plot(ax=ax, title=title)
        ax.set_xlabel('Date')
        ax.set_ylabel(title)
        ax.grid(True, alpha=0.3)

plt.tight_layout()
plt.show()

# Additional ADF tests on individual series
print("\nIndividual ADF Tests:")
individual_vars = ['tri', 'amihud_illiqudity', 'turn_over_ratio', 'bid_ask_spread',
                   'beta_monthly_leveraged', 'dy', 'market_cap', 'annualised_volatility_open']

for var in individual_vars:
    if var in ardlf.columns:
        result = adfuller(ardlf[var].dropna(), autolag='AIC')
        print(f"\n{var}:")
        print(f"  ADF Statistic: {result[0]:.4f}")
        print(f"  p-value: {result[1]:.4f}")
        print(f"  Critical values: {result[4]}")

# Forecast and residual diagnostics ----
print("\nResidual Diagnostics:")

# Plot residuals
fig, axes = plt.subplots(2, 2, figsize=(12, 8))

# Residuals over time
ardl_fit.resid.plot(ax=axes[0, 0], title='Residuals over Time')
axes[0, 0].axhline(y=0, color='r', linestyle='--')
axes[0, 0].set_xlabel('Date')
axes[0, 0].set_ylabel('Residuals')

# Histogram of residuals
axes[0, 1].hist(ardl_fit.resid, bins=30, edgecolor='black')
axes[0, 1].set_title('Histogram of Residuals')
axes[0, 1].set_xlabel('Residuals')
axes[0, 1].set_ylabel('Frequency')

# Q-Q plot
stats.probplot(ardl_fit.resid, dist="norm", plot=axes[1, 0])
axes[1, 0].set_title('Q-Q Plot')

# ACF of residuals
from statsmodels.graphics.tsaplots import plot_acf
plot_acf(ardl_fit.resid, lags=20, ax=axes[1, 1])
axes[1, 1].set_title('ACF of Residuals')

plt.tight_layout()
plt.show()

# Ljung-Box test for autocorrelation
lb_test = acorr_ljungbox(ardl_fit.resid, lags=10, return_df=True)
print("\nLjung-Box Test:")
print(lb_test)

print("\nAnalysis complete!")