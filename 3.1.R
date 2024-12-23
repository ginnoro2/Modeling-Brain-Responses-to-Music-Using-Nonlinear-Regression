# Define your data (example data for testing purposes)
set.seed(100)
df <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  x3 = rnorm(100),
  x4 = rnorm(100),
  x5 = rnorm(100),
  y = rnorm(100)
)

# Generate Model 2
generateModel2 <- function(df) {
  ones <- rep(1, nrow(df)) # Column of ones for bias
  model <- cbind(df$x4, df$x3^2, df$x5, ones) # Include x4, x3^2, x5, and bias
  colnames(model) <- c("x4", "x3_squared", "x5", "bias") # Name columns
  return(model)
}

# Compute Theta Hat using the formula
thetaHat <- function(model, y) {
  return(solve(t(model) %*% model) %*% t(model) %*% y)
}

# Generate the model for Model 2
model_2 <- generateModel2(df)

# Compute Theta Hat for Model 2
Model2_theta_hat <- thetaHat(model_2, df$y)
cat("Theta Hat for Model 2:\n")
print(Model2_theta_hat)

# Now you can proceed with the ABC algorithm
Num <- 100  # Number of iterations for the ABC algorithm
Arr_1 <- numeric(Num)  # Array to store accepted P1 values
Arr_2 <- numeric(Num)  # Array to store accepted P2 values
F_value <- NULL         # Final matrix of P1 values
S_value <- NULL         # Final matrix of P2 values

# Extract parameters from Model 2
Theta_bias <- Model2_theta_hat[4]
Theta_one <- Model2_theta_hat[1]
Theta_two <- Model2_theta_hat[2]
Theta_three <- Model2_theta_hat[3]
Theta_noise <- Model2_theta_hat[5]

# Define rejection threshold
Epsilon <- sum((df$y - mean(df$y))^2) * 2  # Adjust as necessary based on RSS

# ABC Algorithm with rejection sampling
Counter <- 0  # Counter for accepted samples

for (i in 1:Num) {
  # Generate random parameters within the range of the model
  P1 <- runif(1, -abs(Theta_bias), abs(Theta_bias))
  P2 <- runif(1, -abs(Theta_one), abs(Theta_one))
  
  # Generate a prediction using the sampled parameters
  Abc_theta_hat <- matrix(c(P1, P2, Theta_two, Theta_three, Theta_noise))
  Abc_y_hat <- as.vector(df$x1 * Abc_theta_hat[1] + (df$x2^2) * Abc_theta_hat[2] +
                           df$x3 * Abc_theta_hat[3] + df$x4 * Abc_theta_hat[4] + Abc_theta_hat[5])
  
  # Calculate the Residual Sum of Squares (RSS)
  Abc_RSS <- sum((df$y - Abc_y_hat)^2, na.rm = TRUE)  # Ignore NA values if any
  
  # Check for NA values in Abc_RSS
  if (!is.na(Abc_RSS) && Abc_RSS <= Epsilon) {
    Arr_1[i] <- P1
    Arr_2[i] <- P2
    Counter <- Counter + 1
  }
}

# Construct the results matrices
F_value <- matrix(Arr_1[1:Counter], ncol = 1)
S_value <- matrix(Arr_2[1:Counter], ncol = 1)

# Combine results into a data frame
Abc_results <- data.frame(F_value = F_value, S_value = S_value)

# Display results
print(Abc_results)

# Plot the joint and marginal posterior distributions
library(plotly)

plot_ly(Abc_results, x = ~F_value, y = ~S_value, type = "scatter", mode = "markers") %>%
  layout(
    plot_bgcolor = '',
    title = "Joint and Marginal Posterior Distribution",
    xaxis = list(title = "ABC Theta Bias"),
    yaxis = list(title = "ABC Theta One (x4)")
  )

# Plot the frequency distribution histogram for F_value
library(ggplot2)

# Histogram for F_value
ggplot(Abc_results, aes(x = F_value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Frequency Distribution of F_value", x = "F_value", y = "Frequency") +
  theme_minimal()

# Histogram for S_value
ggplot(Abc_results, aes(x = S_value)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Frequency Distribution of S_value", x = "S_value", y = "Frequency") +
  theme_minimal()
