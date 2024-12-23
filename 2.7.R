# Load libraries
library(tidyverse)
library(caret)
library(ggplot2)

# Load the dataset
df <- read.csv("~/Desktop/Analysis/data.csv")  # Adjust the path to your dataset

# Ensure the dataset has the required columns
df <- df %>%
  select(x1, x2, x3, x4, x5)  # Ensure only the relevant columns are included

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(100)
trainIndex <- createDataPartition(df$x2, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Generate Model 2
generateModel2 <- function(df) {
  ones <- rep(1, nrow(df))  # Column of ones for bias
  model <- cbind(df$x1, df$x2^2, df$x3, df$x4, ones)  # Include x1, x2^2, x3, x4, and bias
  colnames(model) <- c("x1", "x2_squared", "x3", "x4", "bias")  # Name columns
  return(model)
}

# Train Model 2 on the training data
trainModel2 <- function(trainData) {
  model <- generateModel2(trainData)
  y <- trainData$x2  # Response variable
  theta_hat <- solve(t(model) %*% model) %*% t(model) %*% y  # Calculate theta_hat (model parameters)
  return(theta_hat)
}

# Predict using Model 2
predictModel2 <- function(model, theta_hat) {
  return(model %*% theta_hat)
}

# Calculate confidence intervals (95%)
calculateConfidenceInterval <- function(predictions, model, theta_hat, level = 0.95) {
  N <- nrow(model)
  pred_variance <- mean((predictions - model %*% theta_hat)^2)  # Compute variance of the predictions
  se <- sqrt(pred_variance / N)  # Standard error
  z_score <- qnorm(1 - (1 - level) / 2)  # Z-score for 95% confidence
  
  lower_bound <- predictions - z_score * se
  upper_bound <- predictions + z_score * se
  
  return(data.frame(lower_bound, upper_bound))
}

# Train the model and make predictions
theta_hat <- trainModel2(trainData)

# Print estimated model parameters
cat("Estimated Model Parameters (Theta_hat):\n", theta_hat, "\n\n")

train_model_matrix <- generateModel2(trainData)
train_predictions <- predictModel2(train_model_matrix, theta_hat)

# Print training predictions
cat("Training Predictions (First 10):\n", head(train_predictions, 10), "\n\n")

# Test the model and make predictions on the test data
test_model_matrix <- generateModel2(testData)
test_predictions <- predictModel2(test_model_matrix, theta_hat)

# Print test predictions
cat("Test Predictions (First 10):\n", head(test_predictions, 10), "\n\n")

# Calculate the 95% confidence intervals for the test predictions
conf_intervals <- calculateConfidenceInterval(test_predictions, test_model_matrix, theta_hat, level = 0.95)

# Print confidence intervals
cat("Confidence Intervals (First 10):\n")
print(head(conf_intervals, 10))


# Plot predictions with confidence intervals
testData$Predicted <- test_predictions
testData$LowerCI <- conf_intervals$lower_bound
testData$UpperCI <- conf_intervals$upper_bound

ggplot(testData, aes(x = x2, y = Predicted)) +
  geom_point(color = "blue", size = 3) +  # Test data points
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1, color = "red") +  # Confidence intervals
  geom_line(aes(y = Predicted), color = "green", size = 1) +  # Model prediction line
  labs(title = "Model 2: Test Predictions with 95% Confidence Interval",
       x = "x2 (Test Data)",
       y = "Predicted y") +
  theme_minimal()

#Training and testing data density plot with confidence level line 
# Load libraries
library(tidyverse)
library(ggplot2)
library(caret)

# Load and filter dataset
df <- read.csv("~/Desktop/Analysis/data.csv")
df <- df %>%
  select(x1, x2, x3, x4, x5) # Ensure required columns are selected

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(100)
trainIndex <- createDataPartition(df$x2, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Function to calculate confidence intervals
calculate_ci <- function(data, confidence = 0.95) {
  mean_value <- mean(data)
  sd_value <- sd(data)
  n <- length(data)
  error_margin <- qnorm((1 + confidence) / 2) * (sd_value / sqrt(n))
  lower_bound <- mean_value - error_margin
  upper_bound <- mean_value + error_margin
  return(data.frame(mean = mean_value, lower = lower_bound, upper = upper_bound))
}

# Calculate confidence intervals for training and testing data
ci_train <- calculate_ci(trainData$x2)
ci_test <- calculate_ci(testData$x2)

# Create the density plot with confidence interval lines
ggplot() +
  geom_density(data = trainData, aes(x = x2, fill = "Training Data"), alpha = 0.5) +
  geom_density(data = testData, aes(x = x2, fill = "Testing Data"), alpha = 0.5) +
  geom_vline(data = ci_train, aes(xintercept = lower, color = "Training CI"), linetype = "dashed", size = 1) +
  geom_vline(data = ci_train, aes(xintercept = upper, color = "Training CI"), linetype = "dashed", size = 1) +
  geom_vline(data = ci_test, aes(xintercept = lower, color = "Testing CI"), linetype = "dashed", size = 1) +
  geom_vline(data = ci_test, aes(xintercept = upper, color = "Testing CI"), linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("Training Data" = "blue", "Testing Data" = "red")) +
  scale_color_manual(values = c("Training CI" = "blue", "Testing CI" = "red")) +
  labs(
    title = "Density Plot: Training and Testing Data with Confidence Intervals",
    x = "x2 (Feature)",
    y = "Density",
    fill = "Dataset",
    color = "Confidence Interval"
  ) +
  theme_minimal()
