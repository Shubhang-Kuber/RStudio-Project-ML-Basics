# Load necessary libraries

library(ggplot2)  

# Set seed for reproducibility
set.seed(42)

# Generate 200 months of data
months <- paste0("Month_", 1:200)

# Randomly generate revenues between 80,000 and 150,000
revenue <- round(runif(200, min = 80000, max = 150000), 2)

# Randomly generate expenses between 50,000 and 120,000
expenses <- round(runif(200, min = 50000, max = 120000), 2)

# Create dataframe
data <- data.frame(
  Month = months,
  Revenue = revenue,
  Expenses = expenses
)

# Step 1: Calculate Profit
data$Profit <- data$Revenue - data$Expenses

# Step 2: Profit After Tax (30%)
data$PAT <- round(data$Profit * (1 - 0.30), 2)

# Step 3: Profit Margin
data$ProfitMargin <- round(data$PAT / data$Revenue, 2)

# Step 4: Mean Profit After Tax
mean_pat <- mean(data$PAT)

# Step 5: Good and Bad Months
data$GoodMonth <- data$PAT > mean_pat
data$BadMonth <- data$PAT < mean_pat

# Step 6: Best and Worst Month
data$BestMonth <- data$PAT == max(data$PAT)
data$WorstMonth <- data$PAT == min(data$PAT)

# Step 7: Print final data
print(head(data, 10))  # Print first 10 rows for preview
write.csv(data, "financial_metrics_output.csv", row.names = FALSE)  # Optional: save to CSV

# Step 8: Plot Profit After Tax
ggplot(data, aes(x = Month, y = PAT, fill = GoodMonth)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  geom_hline(yintercept = mean_pat, linetype = "dashed", color = "blue") +
  labs(title = "Profit After Tax per Month", y = "PAT", x = "Month") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())  # Hide x-axis labels for clarity
