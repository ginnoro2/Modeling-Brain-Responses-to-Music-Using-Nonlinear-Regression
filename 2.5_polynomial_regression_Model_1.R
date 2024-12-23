# Load necessary libraries
library(tidyverse)
library(GGally)
library(plotly)

# Load and filter dataset
df <- read.csv("~/Desktop/Analysis/data.csv")
df <- df %>%
  select(x1, x2, x3, x4, x5)

# Generate Model 1
generateModel1 <- function(df) {
  set.seed(100)
  ones <- rep(1, nrow(df)) # Column of ones for bias
  model <- cbind(df$x4, df$x3^2, ones) # Include x4, x3^2, and bias
  colnames(model) <- c("x4", "x3_squared", "bias") # Name columns
  return(model)
}

# Theta Hat Calculation
thetaHat <- function(model, y) {
  return(solve(t(model) %*% model) %*% t(model) %*% y)
}

# RSS Calculation
calculateRSS <- function(y, y_hat) {
  return(sum((y - y_hat)^2))
}

# Variance Calculation
calculateVariance <- function(N, rss) {
  return(rss / (N - 1))
}

# Log-Likelihood Calculation
calculateLogLikelihood <- function(N, variance, rss) {
  return(- (N / 2) * log(2 * pi) - (N / 2) * log(variance) - (1 / (2 * variance)) * rss)
}

# AIC Calculation
calculateAIC <- function(N, k, log_likelihood) {
  return(2 * k - 2 * log_likelihood)
}

# BIC Calculation
calculateBIC <- function(N, k, log_likelihood) {
  return(k * log(N) - 2 * log_likelihood)
}

# Generate Model 1 and Fit
model_1 <- generateModel1(df)
y <- df$x2 # Response variable

# Compute Theta Hat
theta_hat <- thetaHat(model_1, y)
cat("Theta Hat:\n")
print(theta_hat)

# Predictions
y_hat <- model_1 %*% theta_hat

# Compute Metrics
RSS_Model1 <- calculateRSS(y, y_hat)
cat("RSS for Model 1:", RSS_Model1, "\n")

N <- nrow(df)
Variance_Model1 <- calculateVariance(N, RSS_Model1)
cat("Variance for Model 1:", Variance_Model1, "\n")

LogLikelihood_Model1 <- calculateLogLikelihood(N, Variance_Model1, RSS_Model1)
cat("Log-Likelihood for Model 1:", LogLikelihood_Model1, "\n")

K_Model1 <- ncol(model_1) # Number of parameters
AIC_Model1 <- calculateAIC(N, K_Model1, LogLikelihood_Model1)
cat("AIC for Model 1:", AIC_Model1, "\n")

BIC_Model1 <- calculateBIC(N, K_Model1, LogLikelihood_Model1)
cat("BIC for Model 1:", BIC_Model1, "\n")

# Calculate Errors
calculateError <- function(y, y_hat) {
  return(y - y_hat)
}

errors <- calculateError(y, y_hat)

# Function to generate QQ plot
plotQQ <- function(model_error, title) {
  ggplot(data.frame(model_error = model_error), aes(sample = model_error)) +
    stat_qq(color = "blue") +
    stat_qq_line(color = "black") +
    labs(title = title, x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
}

# Display QQ plot
print(plotQQ(errors, "QQ Plot: Distribution of Prediction Errors for Model 1"))

# Polynomial Regression Visualization
plotScatterWithPoly <- function(x, y, degree, title) {
  data <- data.frame(x = x, y = y)
  ggplot(data, aes(x = x, y = y)) +
    geom_point(color = "blue") +
    stat_smooth(method = "lm", formula = y ~ poly(x, degree), se = FALSE, color = "red") +
    labs(title = title, x = "X Values", y = "Y Values") +
    theme_minimal()
}

# Visualize Polynomial Regression
x <- df$x3
y <- df$x2
degree <- 2
print(plotScatterWithPoly(x, y, degree, "Scatter Plot with Polynomial Regression Line"))
