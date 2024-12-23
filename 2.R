# Ensure you have the necessary libraries
library(ggplot2)
library(tidyr)  # for reshaping the data

# Read the CSV file
df <- read.csv("~/Downloads/BOOKS/Masters/Stats/data/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Reshape the data from wide to long format
df_long <- df %>%
  gather(key = "variable", value = "value", x1:x5)

# Add a time column for the x-axis
df_long$time <- rep(1:nrow(df), times = length(unique(df_long$variable)))



ggplot(df, aes(x = 1:nrow(df), y = x2)) + 
  geom_line(color = "blue") +
  ggtitle("Time Series of x2") +
  xlab("Time (Index)") + 
  ylab("x2 Value")


# Plot multiple variables on the same plot
ggplot(df_long, aes(x = time, y = value, color = variable)) + 
  geom_line() +
  ggtitle("Time Series of x1 to x5") +
  xlab("Time (Index)") + 
  ylab("Value") +
  scale_color_manual(values = c("x1" = "blue", "x2" = "red", "x3" = "green", "x4" = "purple", "x5" = "orange")) +
  theme_minimal()

ggplot(df, aes(x = 1:nrow(df), y = x2)) + 
  geom_line(color = "blue") +
  ggtitle("Time Series of x2") +
  xlab("Time (Index)") + 
  ylab("x2 Value")

