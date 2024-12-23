# Load libraries
library(tidyverse)

# Load and filter dataset
df <- read.csv("~/Desktop/Analysis/data.csv")
df <- df %>%
  select(x1, x2, x3, x4, x5) # Ensure required columns are selected

# Generate Model 5
generateModel5 <- function(df) {
  set.seed(100)
  ones <- rep(1, nrow(df)) # Column of ones for bias
  model <- cbind(df$x4, df$x1^2, df$x3^2, ones) # Include x4, x1^2, x3^2, and bias
  colnames(model) <- c("x4", "x1_squared", "x3_squared", "bias") # Name columns
  return(model)
}

# Calculate Theta Hat
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

# Generate Model 5 and Fit
model_5 <- generateModel5(df)
y <- df$x2 # Response variable

# Compute Theta Hat
theta_hat <- thetaHat(model_5, y)
cat("Theta Hat for Model 5:\n")
print(theta_hat)

# Predictions
y_hat <- model_5 %*% theta_hat

# Compute RSS
RSS_Model5 <- calculateRSS(y, y_hat)
cat("RSS for Model 5:", RSS_Model5, "\n")

# Compute Variance
N <- nrow(df)
Variance_Model5 <- calculateVariance(N, RSS_Model5)
cat("Variance for Model 5:", Variance_Model5, "\n")

# Compute Log-Likelihood
LogLikelihood_Model5 <- calculateLogLikelihood(N, Variance_Model5, RSS_Model5)
cat("Log-Likelihood for Model 5:", LogLikelihood_Model5, "\n")

# Compute AIC
K_Model5 <- ncol(model_5) # Number of parameters
AIC_Model5 <- calculateAIC(N, K_Model5, LogLikelihood_Model5)
cat("AIC for Model 5:", AIC_Model5, "\n")

# Compute BIC
BIC_Model5 <- calculateBIC(N, K_Model5, LogLikelihood_Model5)
cat("BIC for Model 5:", BIC_Model5, "\n")


# Create Data frames for metrics
Modles <- c("Model 5")
AIC <- c(AIC_Model5) # AIC value
BIC <- c(BIC_Model5) # BIC value
RSS <- c(RSS_Model5) # RSS value
variance <- c(Variance_Model5) # Variance value
likelihood <- c(LogLikelihood_Model5) # Likelihood value

df_aic <- data.frame(Modles, AIC)
df_bic <- data.frame(Modles, BIC)
df_rss <- data.frame(Modles, RSS)
df_variance_likelihood <- data.frame(Modles, variance, likelihood)

# Calculate error
calculateError <- function(y, y_hat) {
  return(y - y_hat)
}

errors <- calculateError(y, y_hat)

# Create a data frame for the QQ plot
qq_data <- data.frame(
  sample_quantiles = qqnorm(errors, plot.it = FALSE)$x,
  model_error = qqnorm(errors, plot.it = FALSE)$y
)

# Fit a polynomial regression model
degree <- 3  # Adjust the degree as needed
poly_model <- lm(model_error ~ poly(sample_quantiles, degree), data = qq_data)

# Predict values for the polynomial line
qq_data$poly_fit <- predict(poly_model, qq_data)

# Generate the QQ plot with the polynomial line
ggplot(qq_data, aes(x = sample_quantiles, y = model_error)) +
  geom_point(color = "blue", size = 2) +  # QQ points
  geom_line(aes(y = poly_fit), color = "red", size = 1) +  # Polynomial curve
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +  # Straight QQ line
  labs(
    title = "QQ Plot: Distribution of Prediction Errors with Polynomial Fit",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()
