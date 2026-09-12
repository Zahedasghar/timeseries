
# Answer Key (For Instructor Use Only)

## Part A - Multiple Choice Answers (20 marks)

1. **b** - May be correlated if variables are related
2. **b** - Works with variables of mixed integration orders I(0) and I(1)
3. **b** - Past values of X help predict Y beyond Y's own past
4. **c** - No long-run relationship exists
5. **b** - The contemporaneous relationships among variables
6. **d** - They are always symmetric between variables (FALSE)
7. **b** - It imposes assumptions about contemporaneous effects
8. **b** - What fraction of forecast error variance is due to each shock
9. **c** - I(0)
10. **b** - Deviation from long-run equilibrium
11. **b** - Whether rank(Π) ≤ r against rank(Π) > r
12. **a** - The variable doesn't respond to disequilibrium
13. **b** - Spurious regression
14. **c** - The result is inconclusive
15. **b** - Orthogonalizes the VAR residuals
16. **b** - Capture cross-variable dynamics
17. **b** - Removes individual-specific means
18. **c** - Fixed effects vs random effects
19. **b** - Institutional knowledge provides parameter values
20. **b** - Insufficient lag length

## Part B - Marking Guidelines (30 marks)

### Question 1 (5 marks)

- Reduced-form VAR: All variables treated as endogenous, no contemporaneous restrictions (1 mark)
- Recursive VAR: Cholesky ordering imposes recursive structure on contemporaneous effects (1.5 marks)
- Structural VAR: Economic theory-based restrictions on contemporaneous/long-run relationships (1.5 marks)
- Example and circumstances for each type (1 mark)

### Question 2 (5 marks)

**Part (a) - 2 marks:**
- First number (2) is lags of dependent variable (1 mark)
- Subsequent numbers are lags of each explanatory variable (1 mark)

**Part (b) - 2 marks:**
- F-statistic (6.45) > upper bound (3.61), so reject null (1 mark)
- Conclude cointegration exists (1 mark)

**Part (c) - 1 mark:**
- Estimate long-run coefficients and error correction model (1 mark)

### Question 3 (5 marks)

**Part (a) - 2 marks:**
- Long-run relationship: M = 0.85P + 0.35Y in equilibrium (1 mark)
- Economic interpretation: money demand equation with price and income elasticities (1 mark)

**Part (b) - 1.5 marks:**
- r = 1 means one long-run equilibrium relationship among three variables (0.75 mark)
- Two common stochastic trends drive the system (0.75 mark)

**Part (c) - 1.5 marks:**
- Test if adjustment coefficient (α) for money equation equals zero (1 mark)
- Use t-test or likelihood ratio test (0.5 mark)

### Question 4 (15 marks)

**Part (a) - 5 marks:**
- Data preparation: check for outliers, structural breaks, seasonality (1.5 marks)
- Unit root tests: ADF, PP, KPSS to determine integration order (2 marks)
- Handling mixed integration: use VECM if cointegrated, first-difference if not (1.5 marks)

**Part (b) - 5 marks:**
- Lag selection: Use AIC, BIC, HQ criteria; sequential LR tests (2 marks)
- Deterministic terms: Include constant (restricted/unrestricted) based on data characteristics; trend if non-stationary with drift (2 marks)
- Matrix form with proper notation for 4 variables and 2 lags (1 mark)

**Part (c) - 5 marks:**
- Recursive ordering with justification based on Pakistan's economy (e.g., Y → π → R → REER or Y → π → REER → R) (2 marks)
- Short-run restriction: contemporaneous effect of R on Y is zero (1 mark)
- Long-run restriction: monetary policy has no long-run effect on real GDP (1 mark)
- Identification of monetary policy shock and policy relevance (1 mark)

::: {.content-visible when-format="typst"}
```{=typst}
#v(1cm)
#align(center)[
  #line(length: 50%, stroke: 2pt)
  #v(0.3cm)
  #text(12pt, weight: "bold")[END OF EXAMINATION]
]
```
:::

