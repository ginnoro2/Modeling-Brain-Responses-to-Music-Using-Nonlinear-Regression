# Load necessary libraries
library(ggplot2)
library(tidyr)

# Read the dataset
df <- read.csv("~/Downloads/BOOKS/Masters/Stats/data/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")
view(df)
# Reshape the dataset to gather columns for plotting
df_long <- df %>%
  gather(key = "x_variable", value = "x_value", x1, x3, x4, x5)  # Reshaping to long format

# Scatter plot of x1, x3, x4, x5 vs x2 in one plot
ggplot(df_long, aes(x = x_value, y = x2, color = x_variable)) +
  geom_point() +
  ggtitle("Scatter Plot: x1, x3, x4, x5 vs x2") +
  xlab("Independent Variables (x1, x3, x4, x5)") + 
  ylab("Dependent Variable (x2)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Keep major gridlines
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  # Keep minor gridlines
    axis.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1) +   # Draw the x-axis at y = 0
  geom_vline(xintercept = 0, color = "black", size = 1) +    # Draw the y-axis at x = 0
  scale_color_manual(values = c("blue", "green", "purple", "orange"))  # Assign colors to variables
