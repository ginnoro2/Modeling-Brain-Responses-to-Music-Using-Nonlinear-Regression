# Load necessary libraries
library(ggplot2)

# Read the dataset
df <- read.csv("~/Downloads/BOOKS/Masters/Stats/data/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

install.packages("tidyverse")
library(tidyverse)

df = as.tibble(df)
df
# Scatter plot with linear regression line for x1 to x2
ggplot(df, aes(x = x1, y = x2)) +
  geom_point(color = "blue") +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear regression line (no confidence interval)
  ggtitle("Linear Regression: x1 vs. x2") +
  xlab("x1") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12)
  )

# Scatter plot with linear regression line for x3 to x2
ggplot(df, aes(x = x3, y = x2)) +
  geom_point(color = "green") +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "green") +  # Linear regression line (no confidence interval)
  ggtitle("Linear Regression: x3 vs. x2") +
  xlab("x3") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12)
  )

# Scatter plot with linear regression line for x4 to x2
ggplot(df, aes(x = x4, y = x2)) +
  geom_point(color = "purple") +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "purple") +  # Linear regression line (no confidence interval)
  ggtitle("Linear Regression: x4 vs. x2") +
  xlab("x4") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12)
  )

# Scatter plot with linear regression line for x5 to x2
ggplot(df, aes(x = x4, y = x2)) +
  geom_point(color = "red") +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "orange") +  # Linear regression line (no confidence interval)
  ggtitle("Linear Regression: x5 vs. x2") +
  xlab("x5") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12)
  )


library(GGally)
df
View(df)

cor(df$x2,df$x1)
ggscatmat(df, x1,x2)
typeof(df$x2)
ggscatmat(df, x1,x2)

names(df)
colnames(df)
