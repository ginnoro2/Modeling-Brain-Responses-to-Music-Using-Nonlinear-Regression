
library(tidyverse)
library(GGally)

df = read.csv("~/Desktop/Analysis/data.csv")
view(df)




#correlation and scatter plot for x1 .....x5 to x2 
ggplot(df, aes(x = x1, y = x2)) +
  geom_point(color = "blue") +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Linear regression line (no confidence interval)
  ggtitle("Linear Regression: x1 vs. x2") +
  xlab("x1") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.grid.major = element_line(color = "grey", size = 0.5),  # Major grid lines in grey
    panel.grid.minor = element_line(color = "grey", size = 0.25), # Minor grid lines in grey (optional)
    axis.text = element_text(size = 12)
  )


ggplot(df, aes(x = x3, y = x2)) +
  geom_point(color = "red") +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Linear regression line (no confidence interval)
  ggtitle("Linear Regression: x1 vs. x2") +
  xlab("x1") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.grid.major = element_line(color = "grey", size = 0.5),  # Major grid lines in grey
    panel.grid.minor = element_line(color = "grey", size = 0.25), # Minor grid lines in grey (optional)
    axis.text = element_text(size = 12)
  )

ggplot(df, aes(x = x4, y = x2)) +
  geom_point(color = "green") +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Linear regression line (no confidence interval)
  ggtitle("Linear Regression: x4 vs. x2") +  # Title modified to match the correct axis labels
  xlab("x4") +  # x-axis label modified to match the data
  ylab("x2") +  # y-axis label modified to match the data
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.grid.major = element_line(color = "grey", size = 0.5),  # Major grid lines in grey
    panel.grid.minor = element_line(color = "grey", size = 0.25), # Minor grid lines in grey (optional)
    axis.text = element_text(size = 12)
  )


ggplot(df, aes(x = x5, y = x2)) +
  geom_point(color = "brown") +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Linear regression line (no confidence interval)
  ggtitle("Linear Regression: x1 vs. x2") +
  xlab("x1") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.grid.major = element_line(color = "grey", size = 0.5),  # Major grid lines in grey
    panel.grid.minor = element_line(color = "grey", size = 0.25), # Minor grid lines in grey (optional)
    axis.text = element_text(size = 12)
  )

#correlation and scatter for x1..5 to x2 
library(ggplot2)
library(tidyr)  # for pivot_longer

# Reshape the data from wide to long format for easier plotting
df_long <- df %>%
  pivot_longer(cols = c("x1", "x3", "x4", "x5"), names_to = "Variable", values_to = "Input")

# Create the scatter plot
ggplot(df_long, aes(x = Input, y = x2, color = Variable)) +
  geom_point(alpha = 0.6) +  # Scatter plot for each input against x2
  scale_color_manual(values = c("x1" = "red", "x3" = "green", "x4" = "blue", "x5" = "purple")) +  # Color for each variable
  geom_hline(yintercept = 0, color = "grey", size = 0.8) +  # Draw horizontal axis at y = 0 (grey)
  geom_vline(xintercept = 0, color = "grey", size = 0.8) +  # Draw vertical axis at x = 0 (grey)
  ggtitle("Scatter Plot: Input Variables vs. Output (x2)") +
  xlab("Input Variables (x1, x3, x4, x5)") + 
  ylab("Output (x2)") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "grey", size = 0.5),  # Grey axis lines
    axis.line.y = element_line(color = "grey", size = 0.5),  # Grey axis lines
    axis.ticks = element_line(color = "grey", size = 0.5),  # Grey ticks
    panel.grid.major = element_line(color = "grey", size = 0.2),  # Grey major gridlines
    panel.grid.minor = element_line(color = "grey", size = 0.1),  # Grey minor gridlines
    axis.text = element_text(size = 12),
    legend.title = element_blank()  # Hide legend title
  )


