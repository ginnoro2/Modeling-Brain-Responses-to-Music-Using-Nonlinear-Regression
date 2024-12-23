# Load libraries
library(tidyverse)

# Load and filter dataset
df <- read.csv("~/Desktop/Analysis/data.csv")
df <- df %>%
  select(x1, x2, x3, x4, x5) # Ensure required columns are selected

# Generate Model 2
generateModel2 <- function(df) {
  set.seed(100)
  ones <- rep(1, nrow(df)) # Column of ones for bias
  model <- cbind(df$x4, df$x3^2, df$x5, ones) # Include x4, x3^2, x5, and bias
  colnames(model) <- c("x4", "x3_squared", "x5", "bias") # Name columns
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

# Generate Model 2 and Fit
model_2 <- generateModel2(df)
y <- df$x2 # Response variable

# Compute Theta Hat
theta_hat <- thetaHat(model_2, y)
cat("Theta Hat:\n")
print(theta_hat)

# Predictions
y_hat <- model_2 %*% theta_hat

# Compute RSS
RSS_Model2 <- calculateRSS(y, y_hat)
cat("RSS for Model 2:", RSS_Model2, "\n")

# Compute Variance
N <- nrow(df)
Variance_Model2 <- calculateVariance(N, RSS_Model2)
cat("Variance for Model 2:", Variance_Model2, "\n")

# Compute Log-Likelihood
LogLikelihood_Model2 <- calculateLogLikelihood(N, Variance_Model2, RSS_Model2)
cat("Log-Likelihood for Model 2:", LogLikelihood_Model2, "\n")

# Compute AIC
K_Model2 <- ncol(model_2) # Number of parameters
AIC_Model2 <- calculateAIC(N, K_Model2, LogLikelihood_Model2)
cat("AIC for Model 2:", AIC_Model2, "\n")

# Compute BIC
BIC_Model2 <- calculateBIC(N, K_Model2, LogLikelihood_Model2)
cat("BIC for Model 2:", BIC_Model2, "\n")


#regression line 
library(tidyverse)
library(plotly)

# Data frames for metrics
Modles <- c("Model 1")
AIC <- c(AIC_Model1) # AIC value
BIC <- c(BIC_Model1) #  BIC value
RSS <- c(RSS_Model1) #  RSS value
variance <- c(Variance_Model1) #  variance value
likelihood <- c(LogLikelihood_Model1) #  likelihood value

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
