# Load necessary libraries
library(ggplot2)

# Read the dataset
df <- read.csv("~/Downloads/BOOKS/Masters/Stats/data/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Scatter plot: x1 vs x2 with axes crossing at (0,0)
ggplot(df, aes(x = x1, y = x2)) +
  geom_point(color = "blue") +
  ggtitle("Scatter Plot: x1 vs. x2") +
  xlab("x1") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 1),  # Dark black x-axis line
    axis.line.y = element_line(color = "black", size = 1),  # Dark black y-axis line
    axis.ticks = element_line(color = "black", size = 0.5),  # Dark ticks
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Keep major gridlines
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  # Keep minor gridlines
    axis.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1) +   # Draw the x-axis at y = 0
  geom_vline(xintercept = 0, color = "black", size = 1) +    # Draw the y-axis at x = 0
  coord_cartesian()  # Automatically adjust axis limits based on data

# Scatter plot: x3 vs x2 with axes crossing at (0,0)
ggplot(df, aes(x = x3, y = x2)) +
  geom_point(color = "green") +
  ggtitle("Scatter Plot: x3 vs. x2") +
  xlab("x3") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 1),  # Dark black x-axis line
    axis.line.y = element_line(color = "black", size = 1),  # Dark black y-axis line
    axis.ticks = element_line(color = "black", size = 0.5),  # Dark ticks
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Keep major gridlines
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  # Keep minor gridlines
    axis.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1) +   # Draw the x-axis at y = 0
  geom_vline(xintercept = 0, color = "black", size = 1) +    # Draw the y-axis at x = 0
  coord_cartesian()  # Automatically adjust axis limits based on data

# Scatter plot: x4 vs x2 with axes crossing at (0,0)
ggplot(df, aes(x = x4, y = x2)) +
  geom_point(color = "purple") +
  ggtitle("Scatter Plot: x4 vs. x2") +
  xlab("x4") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 1),  # Dark black x-axis line
    axis.line.y = element_line(color = "black", size = 1),  # Dark black y-axis line
    axis.ticks = element_line(color = "black", size = 0.5),  # Dark ticks
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Keep major gridlines
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  # Keep minor gridlines
    axis.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1) +   # Draw the x-axis at y = 0
  geom_vline(xintercept = 0, color = "black", size = 1) +    # Draw the y-axis at x = 0
  coord_cartesian()  # Automatically adjust axis limits based on data

# Scatter plot: x5 vs x2 with axes crossing at (0,0)
ggplot(df, aes(x = x5, y = x2)) +
  geom_point(color = "orange") +
  ggtitle("Scatter Plot: x5 vs. x2") +
  xlab("x5") + 
  ylab("x2") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 1),  # Dark black x-axis line
    axis.line.y = element_line(color = "black", size = 1),  # Dark black y-axis line
    axis.ticks = element_line(color = "black", size = 0.5),  # Dark ticks
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Keep major gridlines
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  # Keep minor gridlines
    axis.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1) +   # Draw the x-axis at y = 0
  geom_vline(xintercept = 0, color = "black", size = 1) +    # Draw the y-axis at x = 0
  coord_cartesian()  # Automatically adjust axis limits based on data


install.packages("GGally")
library(GGally)
ggscatmat()