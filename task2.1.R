# Load libraries
library(tidyverse)
library(GGally)

# Load and filter dataset
df = read.csv("~/Desktop/Analysis/data.csv")
df = df %>%
  select(x1, x2, x3, x4, x5)

# Generate Model 1
generateModel1 <- function(df){
  set.seed(100)
  ones = rep(1, nrow(df)) # Column of ones for bias
  model = cbind(df$x4, df$x3^2, ones) # Include x4, x3^2, and bias
  colnames(model) <- c("x4", "x3_squared", "bias") # Optional: Name columns
  return(model)
}

# Calculate Theta Hat
thetaHat <- function(model, y){
  return(solve(t(model) %*% model) %*% t(model) %*% y)
}

# Generate Model 1 and Fit
model_1 <- generateModel1(df)
y <- df$x2 # Response variable

# Compute Theta Hat
theta_hat <- thetaHat(model_1, y)
print("Theta Hat:")
print(theta_hat)

#calculate RSS
calculateRSS <- function(y, y_hat_modle){
  return(sum((y-y_hat_model)^2))
}


#calculate Log-likelihood function
calculateVariance <-function(N, rss_modle){
  return(rss_model/N-1)
}
calculateLikeliHood <- function(N, variance_model, rss_model){
  return (-(N/2)*(log)2*pi))-(N/2)*(log(variance_model))-(1/(2*variance_model))*rss_modle)
}
N = length(df$y)
Variance_Model1 = calculateVariance(N, RSS_Model1)
LikeLiHood_1 = calculateLikeliHood(N, Variance_Model1, RSS_Model1)


#calculate AIC 
calclateAIC <-function(N, modle_thetahat, likelihood_model){
  k_model = lentgh(model_thetahat)
  retunr(2*k_modle-2*likelihood_model)
}

#calculate BIC
calculateBIC <- function(N, model_theathat, likelihood_model){
  k_model = lenght(model_thetahat)
  return(k_model*log(N) - 2*likelihood_model)
}

#QQ Plot
# Load Libraries
library(tidyverse)
library(plotly)

# Create placeholder data for demonstration (replace with actual data)
Modles <- c("Model 1")          # Model names
AIC_Model1 <- 120.5            # Replace with actual AIC value
BIC_Model1 <- 130.3            # Replace with actual BIC value
RSS_Model1 <- 250.7            # Replace with actual RSS value
Variance_Model1 <- 5.2         # Replace with actual variance value
LogLikelihood_Model1 <- -75.2  # Replace with actual likelihood value

# Create Data Frames for Metrics
df_aic <- data.frame(Modles, AIC = AIC_Model1)
df_bic <- data.frame(Modles, BIC = BIC_Model1)
df_rss <- data.frame(Modles, RSS = RSS_Model1)
df_variance_likelihood <- data.frame(Modles, Variance = Variance_Model1, Likelihood = LogLikelihood_Model1)

# Print Data Frames
print("AIC Data Frame:")
print(df_aic)
print("BIC Data Frame:")
print(df_bic)
print("RSS Data Frame:")
print(df_rss)
print("Variance and Likelihood Data Frame:")
print(df_variance_likelihood)

# Placeholder for response variable (y) and predictions (y_hat)
# Replace these with actual values from your model
set.seed(42)
y <- rnorm(100, mean = 50, sd = 10)  # Simulated actual values
y_hat <- y + rnorm(100, mean = 0, sd = 5)  # Simulated predictions

# Calculate Errors
calculateError <- function(y, y_hat) {
  return(y - y_hat)
}

errors <- calculateError(y, y_hat)

# Function to Generate Non-Interactive QQ Plot
plotQQ <- function(model_error, title) {
  ggplot(data.frame(model_error = model_error), aes(sample = model_error)) +
    stat_qq(color = "green") +
    stat_qq_line(color = "black") +
    labs(title = title, x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
}

# Generate Non-Interactive QQ Plot
print(plotQQ(errors, "QQ Plot: Distribution of Prediction Errors for Model 1"))

