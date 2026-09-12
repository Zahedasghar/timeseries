# ==============================================================================
# Vector Autoregression (VAR) and Structural VAR (SVAR) Analysis — Python
# ==============================================================================

# If needed:
# pip install pandas numpy matplotlib statsmodels

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import grangercausalitytests
from statsmodels.stats.diagnostic import acorr_ljungbox
from statsmodels.tsa.vector_ar.svar_model import SVAR

# ------------------------------------------------------------------------------
# 1) Load & prepare data
# ------------------------------------------------------------------------------

# Read Stata data
df = pd.read_stata("data/VAR JEP data.dta")

# Preview structure
print(df.head())
print(df.info())

# If you already have a proper date/quarter column, set it as index here.
# Otherwise, create a quarterly index starting at 2000Q1 like in your R code:
# (Assumes df is already sorted chronologically)
if not isinstance(df.index, pd.PeriodIndex):
    n = len(df)
    q_index = pd.period_range(start="1960Q1", periods=n, freq="Q")
    df.index = q_index

# Select relevant variables (inflation, unemployment rate, fed funds rate)
sw_data = df[["inflation", "unrate", "ffr"]].astype(float).dropna()

# Quick sanity checks
print(sw_data.head())
print(sw_data.describe())

# ------------------------------------------------------------------------------
# 2) Estimate VAR(p=4) with constant
# ------------------------------------------------------------------------------

p = 4
var_model = VAR(sw_data)
var_res = var_model.fit(p, trend="c")
print(var_res.summary())

# ------------------------------------------------------------------------------
# 3) Granger causality tests (joint, like R's `causality`)
# ------------------------------------------------------------------------------
# In statsmodels VARResults, test_causality handles joint tests across lags.

# H0: inflation does NOT Granger-cause {others}
print("\n=== Granger Causality: inflation (joint) ===")
print(var_res.test_causality(caused="unrate", causing="inflation", kind="f").summary())
print(var_res.test_causality(caused="ffr",    causing="inflation",   kind="f").summary())

# H0: unrate does NOT Granger-cause {others}
print("\n=== Granger Causality: unrate (joint) ===")
print(var_res.test_causality(caused="inflation", causing="unrate", kind="f").summary())
print(var_res.test_causality(caused="ffr",       causing="unrate", kind="f").summary())

# H0: ffr does NOT Granger-cause {others}
print("\n=== Granger Causality: ffr (joint) ===")
print(var_res.test_causality(caused="inflation", causing="ffr", kind="f").summary())
print(var_res.test_causality(caused="unrate",    causing="ffr", kind="f").summary())

# ------------------------------------------------------------------------------
# 4) Pairwise Granger tests (like R's grangertest with order=4)
# ------------------------------------------------------------------------------
# statsmodels.grangercausalitytests expects a 2-col array [y, x] and maxlag
maxlag = 4
print("\n=== Pairwise Granger Tests (inflation <- ffr) ===")
_ = grangercausalitytests(sw_data[["inflation", "ffr"]].dropna(), maxlag=maxlag, verbose=True)

print("\n=== Pairwise Granger Tests (inflation <- unrate) ===")
_ = grangercausalitytests(sw_data[["inflation", "unrate"]].dropna(), maxlag=maxlag, verbose=True)

# ------------------------------------------------------------------------------
# 5) Forecast Error Variance Decomposition (FEVD)
# ------------------------------------------------------------------------------
print("\n=== FEVD (12 quarters ahead) ===")
fevd = var_res.fevd(12)
print(fevd.summary())

# Plot FEVD
fevd.plot()
plt.suptitle("FEVD (12 Quarters Ahead)")
plt.tight_layout()
plt.show()

# ------------------------------------------------------------------------------
# 6) VAR stability diagnostics (roots inside unit circle)
# ------------------------------------------------------------------------------
print("\n=== Stability check ===")
print("Is stable?:", var_res.is_stable(verbose=True))

# Plot companion roots
roots = var_res.roots
plt.figure()
plt.scatter(roots.real, roots.imag)
circle = plt.Circle((0, 0), 1, fill=False, linestyle="--")
plt.gca().add_artist(circle)
plt.axhline(0, linewidth=0.5)
plt.axvline(0, linewidth=0.5)
plt.gca().set_aspect('equal', adjustable='box')
plt.title("Companion Matrix Roots (unit circle)")
plt.xlabel("Real")
plt.ylabel("Imag")
plt.tight_layout()
plt.show()

# ------------------------------------------------------------------------------
# Impulse Response Functions (IRF) — next-to-last section (before SVAR)
# ------------------------------------------------------------------------------

print("\n=== Impulse Response Functions (20 Quarters) ===")

# Standard IRF (non-orthogonalized shocks, like reduced-form VAR)
irf = var_res.irf(20)

# Plot each IRF with confidence intervals
irf.plot(orth=False)
plt.suptitle("Reduced-Form Impulse Response Functions (20 Quarters)")
plt.tight_layout()
plt.show()

# Orthogonal IRFs (Cholesky shocks — comparable to R bootstrap IRF)
irf_orth = var_res.irf(20)
irf_orth.plot(orth=True)
plt.suptitle("Orthogonalized IRFs (Cholesky Structural Shocks)")
plt.tight_layout()
plt.show()

# Optionally extract IRF values into arrays / DataFrames
irf_table = irf.cov(orth=False)   # variance of IRFs
resp_vals = irf.irfs              # IRF values
lower_ci, upper_ci = irf.ci(0.95) # 95% CI bands

print("\nIRF matrices (first few values):")
print(resp_vals[:5])  # first 5 periods' responses
