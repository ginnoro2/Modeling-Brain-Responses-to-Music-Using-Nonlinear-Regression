# Load necessary libraries
library(ggplot2)
library(corrplot)

# Assuming your data is loaded into a dataframe called df
# df <- read.csv("your_data.csv")  # If it's a CSV, for example

# 1. Time Series Plot (Using row index as time)
ggplot(df, aes(x = 1:nrow(df), y = x2)) + 
  geom_line(color = "darkblue") +
  ggtitle("Time Series of x2") +
  xlab("Time (Index)") + 
  ylab("x2 Value")

# 2. Distribution Plot for Inputs and Output (Histogram with KDE)
ggplot(df, aes(x = x2)) + 
  geom_histogram(aes(y = ..density..), color = "blue", fill = "blue", alpha = 0.4) + 
  geom_density(color = "green") +
  ggtitle("Distribution of x2") +
  xlab("x2 Value") +
  ylab("Density")

# Plot for other input variables
input_columns <- setdiff(names(df), "x2")
for (col in input_columns) {
  ggplot(df, aes_string(x = col)) + 
    geom_histogram(aes(y = ..density..), color = "green", fill = "green", alpha = 0.4) + 
    geom_density(color = "green") +
    ggtitle(paste("Distribution of", col)) +
    xlab(paste(col, "Value")) + 
    ylab("Density") +
    print()
}

# 3. Correlation Plot
correlation_matrix <- cor(df)
corrplot(correlation_matrix, method = "circle", type = "full", tl.col = "black", tl.srt = 45)

# 4. Scatter Plot (between inputs and x2)
for (col in input_columns) {
  ggplot(df, aes_string(x = col, y = "x2")) + 
    geom_point(color = "red") +
    ggtitle(paste("Scatter Plot:", col, "vs x2")) +
    xlab(col) + 
    ylab("x2") +
    print()
}
