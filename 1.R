# Load necessary libraries
library(ggplot2)

# Read your dataset
df <- read.csv("~/Downloads/BOOKS/Masters/Stats/data/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Create a 'time' column (Sequential Index as Time)
df$time <- 1:nrow(df)  # Or customize this if you want real dates or times

# View the first few rows to confirm the 'time' column has been added
head(df)

# Now, plot 'time' vs 'x2'
ggplot(df, aes(x = time, y = x5)) +
  geom_line(color = "blue") +
  ggtitle("Time Series of x2") +
  xlab("Time (Index)") + 
  ylab("x2 Value") +
  theme_minimal()
