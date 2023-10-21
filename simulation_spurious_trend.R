# Set the parameters
set.seed(42)  # For reproducibility
n_values <- c(50, 100, 200)  # Different sample sizes
num_replications <- 2500  # Number of replications for each sample size

# Create a list to store density estimates for each sample size
density_estimates <- list()

# Simulate the data and run regressions
for (n in n_values) {
  beta_hat <- numeric(num_replications)
  
  for (replication in 1:num_replications) {
    # Simulate data for each replication
    Y <- rnorm(n)
    X <- rnorm(n)
    
    # Fit a linear regression model
    model <- lm(Y ~ X)
    
    # Store the estimated coefficient for X (beta_hat)
    beta_hat[replication] <- coef(model)['X']
  }
  
  # Calculate the density estimate for beta_hat
  density_estimates[[as.character(n)]] <- density(beta_hat)
}

# Create the plot
colors <- rainbow(length(n_values))
plot(density_estimates[[1]], col = colors[1], main = "Density of beta_hat for Different Sample Sizes", xlab = "beta_hat", ylab = "Density", xlim = c(-3, 3))
legend("topright", legend = n_values, col = colors, lty = 1)

for (i in 2:length(n_values)) {
  lines(density_estimates[[i]], col = colors[i])
}







# Set the parameters
set.seed(42)  # For reproducibility
n_values <- c(50, 100, 200)  # Different sample sizes
num_replications <- 2500  # Number of replications for each sample size
alpha <- 0.05  # Significance level

# Create a vector to store rejection frequencies for each sample size
rejection_freqs <- numeric(length(n_values))

# Simulate the data and run regressions
for (n in n_values) {
  for (replication in 1:num_replications) {
    # Simulate data for each replication
    Y <- rnorm(n)
    X <- rnorm(n)
    
    # Fit a linear regression model
    model <- lm(Y ~ X)
    
    # Perform the significance test for beta1
    p_value <- summary(model)$coefficients[2, 4]
    reject_null <- p_value < alpha / 2 || p_value > 1 - alpha / 2
    
    # Update the rejection frequencies
    rejection_freqs[n == n_values] <- rejection_freqs[n == n_values] + reject_null
  }
  # Calculate the estimated rejection frequency for the sample size
  rejection_freqs[n == n_values] <- rejection_freqs[n == n_values] / num_replications
}

# Report the estimated rejection frequencies
cat("Estimated Rejection Frequencies:\n")
for (i in 1:length(n_values)) {
  cat(paste("Sample Size n =", n_values[i], ": ", round(rejection_freqs[i], 3), "\n"))
}

# Show a time series plot for the last replication with n = 200
last_n200_replication <- lm(Y ~ X)  # Fit a regression model for the last replication
plot(Y ~ X, data = last_n200_replication$model, main = "Time Series Plot for n = 200", xlab = "Time (t)", col = "red", pch = 19)
points(predict(last_n200_replication), col = "blue", pch = 19)
legend("topright", legend = c("Yt", "Xt"), col = c("red", "blue"), pch = 19)

















# Set the parameters
set.seed(42)  # For reproducibility
n_values <- c(50, 100, 200)  # Different sample sizes
num_replications <- 2500  # Number of replications for each sample size

# Create storage matrices for saving regression coefficients and p-values
beta_hat_matrix <- matrix(NA, nrow = length(n_values), ncol = num_replications)
p_value_matrix <- matrix(NA, nrow = length(n_values), ncol = num_replications)

# Create storage vectors for rejection frequencies
rejection_freqs <- numeric(length(n_values))

# Simulate the data and run regressions
for (n_idx in 1:length(n_values)) {
  n <- n_values[n_idx]
  
  for (replication in 1:num_replications) {
    t <- 1:n  # Create a time variable
    
    # Simulate data with deterministic trends
    Y <- 0.05 * t + rnorm(n)
    X <- 0.03 * t + rnorm(n)
    
    # Fit a linear regression model
    model <- lm(Y ~ X)
    
    # Store the estimated coefficient for X
    beta_hat_matrix[n_idx, replication] <- coef(model)['X']
    
    # Store the p-value for the significance test of H0: beta1 = 0
    p_value_matrix[n_idx, replication] <- summary(model)$coefficients[2, 4]
  }
  
  # Calculate the estimated rejection frequency for the sample size
  rejection_freqs[n_idx] <- mean(p_value_matrix[n_idx, ] < 0.05)
}

# Report the estimated rejection frequencies
cat("Estimated Rejection Frequencies at the 5% Significance Level:\n")
for (i in 1:length(n_values)) {
  cat(paste("Sample Size n =", n_values[i], ": ", round(rejection_freqs[i], 3), "\n"))
}

# Create density plots for beta_hat
library(ggplot2)
library(gridExtra)

density_plots <- list()
for (n_idx in 1:length(n_values)) {
  p <- ggplot(data.frame(beta_hat = beta_hat_matrix[n_idx, ]), aes(x = beta_hat)) +
    geom_density(fill = "steelblue", color = "black") +
    labs(title = paste("Sample Size n =", n_values[n_idx]), x = expression(hat(beta[1])), y = "Density")
  density_plots[[n_idx]] <- p
}

grid.arrange(grobs = density_plots)







# Set the parameters
set.seed(42)  # For reproducibility
n_values <- c(50, 100, 200)  # Different sample sizes
num_replications <- 2500  # Number of replications for each sample size

# Create storage matrices for saving regression coefficients and p-values
beta_hat_matrix <- matrix(NA, nrow = length(n_values), ncol = num_replications)
p_value_matrix <- matrix(NA, nrow = length(n_values), ncol = num_replications)

# Create storage vectors for rejection frequencies
rejection_freqs <- numeric(length(n_values))

# Simulate the data and run regressions
for (n_idx in 1:length(n_values)) {
  n <- n_values[n_idx]
  
  for (replication in 1:num_replications) {
    # Simulate data with stochastic trends using cumulative sums of random errors
    y <- cumsum(rnorm(n))
    x <- cumsum(rnorm(n))
    
    # Fit a linear regression model
    model <- lm(y ~ x)
    
    # Store the estimated coefficient for x
    beta_hat_matrix[n_idx, replication] <- coef(model)['x']
    
    # Store the p-value for the significance test of H0: beta1 = 0
    p_value_matrix[n_idx, replication] <- summary(model)$coefficients[2, 4]
  }
  
  # Calculate the estimated rejection frequency for the sample size
  rejection_freqs[n_idx] <- mean(p_value_matrix[n_idx, ] < 0.05)
}

# Report the estimated rejection frequencies
cat("Estimated Rejection Frequencies at the 5% Significance Level:\n")
for (i in 1:length(n_values)) {
  cat(paste("Sample Size n =", n_values[i], ": ", round(rejection_freqs[i], 3), "\n"))
}





# Create density plots for beta_hat (same code as in Question 1)
library(ggplot2)
library(gridExtra)

density_plots <- list()
for (n_idx in 1:length(n_values)) {
  p <- ggplot(data.frame(beta_hat = beta_hat_matrix[n_idx, ]), aes(x = beta_hat)) +
    geom_density(fill = "steelblue", color = "black") +
    labs(title = paste("Sample Size n =", n_values[n_idx]), x = expression(hat(beta[1])), y = "Density")
  density_plots[[n_idx]] <- p
}

grid.arrange(grobs = density_plots)

# Calculate and report the estimated rejection frequencies (same code as in Question 1)
cat("Estimated Rejection Frequencies at the 5% Significance Level:\n")
for (i in 1:length(n_values)) {
  cat(paste("Sample Size n =", n_values[i], ": ", round(rejection_freqs[i], 3), "\n"))
}

# Create time series plots for the last replication with n = 200
last_n200_replication <- lm(y ~ x)  # Fit a regression model for the last replication
plot(y ~ x, data = last_n200_replication$model, main = "Time Series Plot for n = 200", xlab = "Time (t)", col = "red", pch = 19)
points(predict(last_n200_replication), col = "blue", pch = 19)
legend("topright", legend = c("Yt", "Xt"), col = c("red", "blue"), pch = 19)


## 3-C

# Set the parameters
set.seed(42)  # For reproducibility
n_values <- c(50, 100, 200)  # Different sample sizes
num_replications <- 2500  # Number of replications for each sample size

# Create storage matrices for saving regression coefficients and p-values
beta_hat_matrix <- matrix(NA, nrow = length(n_values), ncol = num_replications)
p_value_matrix <- matrix(NA, nrow = length(n_values), ncol = num_replications)

# Create storage vectors for rejection frequencies
rejection_freqs <- numeric(length(n_values))

# Simulate the data and run regressions in first differences
for (n_idx in 1:length(n_values)) {
  n <- n_values[n_idx]
  
  for (replication in 1:num_replications) {
    # Simulate data with stochastic trends using cumulative sums of random errors
    y <- cumsum(rnorm(n))
    x <- cumsum(rnorm(n))
    
    # Calculate first differences
    dy <- diff(y)
    dx <- diff(x)
    
    # Fit a linear regression model in first differences
    model <- lm(dy ~ dx)
    
    # Store the estimated coefficient for dx
    beta_hat_matrix[n_idx, replication] <- coef(model)['dx']
    
    # Store the p-value for the significance test of H0: beta1 = 0
    p_value_matrix[n_idx, replication] <- summary(model)$coefficients[2, 4]
  }
  
  # Calculate the estimated rejection frequency for the sample size
  rejection_freqs[n_idx] <- mean(p_value_matrix[n_idx, ] < 0.05)
}

# Report the estimated rejection frequencies
cat("Estimated Rejection Frequencies at the 5% Significance Level (in first differences):\n")
for (i in 1:length(n_values)) {
  cat(paste("Sample Size n =", n_values[i], ": ", round(rejection_freqs[i], 3), "\n"))
}
