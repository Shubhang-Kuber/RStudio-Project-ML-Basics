# Install necessary package to read Excel files (run once)
# install.packages("readxl")

# Load libraries
library(readxl)
library(ggplot2)

# Read the Excel file (corrected path)
data <- read_excel("C:\\Shubhang Kuber\\Engineering 2024-2028\\1st Year All Docs\\2nd Semester\\Skill Lab Projects\\RStudio files.xlsx")

# Preview the data
print(head(data))

# Generate Revenue if missing
set.seed(42)
data$Revenue <- round(runif(nrow(data), min=100000, max=150000), 2)

# Calculate Profit
data$Profit <- data$Revenue - data$Expenses

# Calculate Profit After Tax (30%)
data$PAT <- round(data$Profit * (1 - 0.30), 2)

# Plot Profit After Tax over Month
ggplot(data, aes(x = Month, y = PAT)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(title = "Profit After Tax Over Months", x = "Month", y = "Profit After Tax") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
