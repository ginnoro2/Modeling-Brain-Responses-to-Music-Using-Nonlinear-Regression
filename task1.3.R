library(tidyverse)
library(GGally)

df = read.csv("~/Desktop/Analysis/data.csv")
view(df)

df = df %>%
  select(x1,x2,x3,x4,x5)
view(df)
sum(complete.cases(df))
sum(colSums(df))


cor(df$x1,df$x2)

ggplot(df, aes(x1))+
  geom_histo()

library(GGally)

# Add a dummy grouping column (replace with your actual grouping column if it exists)
df$species <- sample(c("Group1", "Group2", "Group3"), nrow(df), replace = TRUE)

# Define a color palette manually for your groups
custom_colors <- c("Group1" = "red", "Group2" = "blue", "Group3" = "green")

# Create the scatterplot matrix
ggscatmat(df, columns = 1:5, color = "species") +
  scale_color_manual(values = custom_colors) +  # Assign custom colors
  theme_minimal() +
  ggtitle("Scatterplot Matrix with Custom Colors") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

library(GGally)
ggscatmat(df, columns = 1:5)

# Ensure only the columns x1 to x5 are included
df_subset <- df[, c("x1", "x2", "x3", "x4", "x5")]

# Define a color palette for the variables
custom_colors <- c("x1" = "red", "x2" = "blue", "x3" = "green", "x4" = "purple", "x5" = "orange")

# Create the scatterplot matrix
ggscatmat(df_subset, columns = 1:5) +
  theme_minimal() +
  ggtitle("Scatterplot Matrix for fMRI Brain Images") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )


