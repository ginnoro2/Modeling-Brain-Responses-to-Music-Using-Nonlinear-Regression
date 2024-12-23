library(tidyverse)
library(GGally)

df = read.csv("~/Desktop/Analysis/data.csv")
view(df)

df = df %>%
  select(x1,x2,x3,x4,x5)
view(df)
sum(complete.cases(df))
sum(colSums(df))

#histogram 
ggplot(df, aes(x=x1))+
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histogram: x1")+
  xlab("x1")+
  ylab("Frequency")+
  theme_minimal()

#density plot 
ggplot(df, aes(x = x1)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
  ggtitle("Density Plot: x1") +
  xlab("x1") +
  ylab("Density") +
  theme_minimal()

#histogram with density plot
ggplot(df, aes(x = x1)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", alpha = 0.3) +
  ggtitle("Histogram and Density Plot: x1") +
  xlab("x1") +
  ylab("Density") +
  theme_minimal()

ggplot(df, aes(x = x2)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "green", alpha = 0.3) +
  ggtitle("Histogram and Density Plot: x2") +
  xlab("x2") +
  ylab("Density") +
  theme_minimal()

ggplot(df, aes(x = x3)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", alpha = 0.3) +
  ggtitle("Histogram and Density Plot: x3") +
  xlab("x3") +
  ylab("Density") +
  theme_minimal()

ggplot(df, aes(x = x4)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "black", alpha = 0.3) +
  ggtitle("Histogram and Density Plot: x4") +
  xlab("x4") +
  ylab("Density") +
  theme_minimal()

ggplot(df, aes(x = x5)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "brown", alpha = 0.3) +
  ggtitle("Histogram and Density Plot: x5") +
  xlab("x5") +
  ylab("Density") +
  theme_minimal()


#histogram and density plot of x1......x5 in one frame
color_map <- c("x1" = "lightblue", 
               "x2" = "lightgreen", 
               "x3" = "lightyellow", 
               "x4" = "lightpink", 
               "x5" = "lightgray")

ggplot() +
  # Histogram for x1
  geom_histogram(data = df, aes(x = x1, y = ..density..), binwidth = 1, fill = color_map["x1"], color = "black", alpha = 0.6) +
  # Density for x1
  geom_density(data = df, aes(x = x1), color = "blue", alpha = 0.3) +
  
  # Histogram for x2
  geom_histogram(data = df, aes(x = x2, y = ..density..), binwidth = 1, fill = color_map["x2"], color = "black", alpha = 0.6) +
  # Density for x2
  geom_density(data = df, aes(x = x2), color = "green", alpha = 0.3) +
  
  # Histogram for x3
  geom_histogram(data = df, aes(x = x3, y = ..density..), binwidth = 1, fill = color_map["x3"], color = "black", alpha = 0.6) +
  # Density for x3
  geom_density(data = df, aes(x = x3), color = "orange", alpha = 0.3) +
  
  # Histogram for x4
  geom_histogram(data = df, aes(x = x4, y = ..density..), binwidth = 1, fill = color_map["x4"], color = "black", alpha = 0.6) +
  # Density for x4
  geom_density(data = df, aes(x = x4), color = "red", alpha = 0.3) +
  
  # Histogram for x5
  geom_histogram(data = df, aes(x = x5, y = ..density..), binwidth = 1, fill = color_map["x5"], color = "black", alpha = 0.6) +
  # Density for x5
  geom_density(data = df, aes(x = x5), color = "black", alpha = 0.3) +
  
  ggtitle("Histogram and Density Plot: x1 to x5") +
  xlab("Values") +
  ylab("Density") +
  theme_minimal() +
  scale_fill_manual(values = color_map) + # Assign colors to the fill aesthetic
  scale_color_manual(values = c("blue", "green", "orange", "red", "black")) # Assign colors to the density lines


library(tidyr)
library(ggplot2)

# Reshape only numeric columns (x1 to x5) to long format
df_long <- df %>%
  pivot_longer(cols = x1:x5, names_to = "Variable", values_to = "Value")

# Custom colors for variables
custom_colors <- c("x1" = "red", "x2" = "blue", "x3" = "green", "x4" = "purple", "x5" = "orange")

# Plot histograms and density curves
ggplot(df_long, aes(x = Value, color = Variable)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.7, size = 1) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  facet_wrap(~ Variable, scales = "free") +
  ggtitle("Histograms and Density Curves for x1 to x5") +
  xlab("Values") +
  ylab("Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

