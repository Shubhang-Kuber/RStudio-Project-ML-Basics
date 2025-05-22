# --- 0. Install and Load Required Packages ---
# This section checks if packages are installed and installs them if not.
# Then, it loads them.

required_packages <- c("ggplot2", "tidyr", "RColorBrewer")
new_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(new_packages) > 0) {
  cat("Installing missing packages:", paste(new_packages, collapse=", "), "\n")
  install.packages(new_packages)
} else {
  cat("All required packages are already installed.\n")
}

# Load the packages
cat("Loading packages...\n")
library(ggplot2)
library(tidyr)
library(RColorBrewer)
cat("Packages loaded successfully.\n\n")

# Set seed for reproducibility of any random data generation
set.seed(123)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assignment 1: Financial Metrics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print("--- Assignment 1: Financial Metrics ---")

# Input: Sample monthly revenue and expenses for a financial year (12 months)
# (Making up some random data for this assignment)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthly_revenue <- round(runif(12, min = 100000, max = 250000), 0)
monthly_expenses <- round(monthly_revenue * runif(12, min = 0.4, max = 0.7), 0) # Expenses between 40-70% of revenue

cat("Sample Monthly Revenue:\n"); print(setNames(monthly_revenue, months))
cat("Sample Monthly Expenses:\n"); print(setNames(monthly_expenses, months))

# Task: Calculate the following financial metrics:

# 1. Profit for each month
profit_each_month <- monthly_revenue - monthly_expenses
print("Profit for each month:")
print(setNames(profit_each_month, months))

# 2. Profit after tax for each month (tax rate: 30%)
tax_rate <- 0.30
profit_after_tax_each_month <- profit_each_month * (1 - tax_rate)
print("Profit after tax for each month:")
print(setNames(profit_after_tax_each_month, months))

# 3. Profit margin for each month - equals to profit after tax divided by revenue.
# Handle cases where revenue might be zero to avoid NaN/Inf
profit_margin_each_month <- ifelse(monthly_revenue == 0, 0, profit_after_tax_each_month / monthly_revenue)
print("Profit margin for each month (%):")
print(setNames(profit_margin_each_month * 100, months))

# 4. Good months - where the profit after tax was greater than mean for the year
mean_profit_after_tax_year <- mean(profit_after_tax_each_month)
good_months_indices <- which(profit_after_tax_each_month > mean_profit_after_tax_year)
good_months <- months[good_months_indices]
print(paste("Mean profit after tax for the year:", round(mean_profit_after_tax_year, 2)))
print("Good months (profit after tax > mean):")
print(good_months)

# 5. Bad months - where the profit after tax was less than the mean for the year
bad_months_indices <- which(profit_after_tax_each_month < mean_profit_after_tax_year)
bad_months <- months[bad_months_indices]
print("Bad months (profit after tax < mean):")
print(bad_months)

# 6. Best month - where the profit after tax was max for the year
best_month_index <- which.max(profit_after_tax_each_month)
best_month <- months[best_month_index]
print(paste("Best month (max profit after tax):", best_month, "with profit after tax of", round(profit_after_tax_each_month[best_month_index],2)))

# 7. Worst month - where the profit after tax was min for the year
worst_month_index <- which.min(profit_after_tax_each_month)
worst_month <- months[worst_month_index]
print(paste("Worst month (min profit after tax):", worst_month, "with profit after tax of", round(profit_after_tax_each_month[worst_month_index],2)))

# Consolidate into a data frame for better readability
financial_summary <- data.frame(
  Month = months,
  Revenue = monthly_revenue,
  Expenses = monthly_expenses,
  Profit_Before_Tax = profit_each_month,
  Profit_After_Tax = profit_after_tax_each_month,
  Profit_Margin_Percent = round(profit_margin_each_month * 100, 2)
)
print("Financial Summary Table:")
print(financial_summary)

cat("\n\n") # Add some space before the next assignment output

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assignment 2: Basketball Statistics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print("--- Assignment 2: Basketball Statistics ---")

# --- Data Preparation (as provided in OCR, plus FT and FTA) ---
#Seasons
Seasons <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")
#Players
Players <- c("KobeBryant","JoeJohnson","LeBronJames","CarmeloAnthony","DwightHoward","ChrisBosh","ChrisPaul","KevinDurant","DerrickRose","DwayneWade")

#Salaries
KobeBryant_Salary <- c(15946875,17718750,19490625,21262500,23034375,24806250,25244493,27849149,30453805,23500000)
JoeJohnson_Salary <- c(12000000,12744189,13488377,14232567,14976754,16324500,18038573,19752645,21466718,23180790)
LeBronJames_Salary <- c(4621800,5828090,13041250,14410581,15779912,14500000,16022500,17545000,19067500,20644400)
CarmeloAnthony_Salary <- c(3713640,4694041,13041250,14410581,15779912,17149243,18518574,19450000,22407474,22458000)
DwightHoward_Salary <- c(4493160,4806720,6061274,13758000,15202590,16647180,18091770,19536360,20513178,21436271)
ChrisBosh_Salary <- c(3348000,4235220,12455000,14410581,15779912,14500000,16022500,17545000,19067500,20644400)
ChrisPaul_Salary <- c(3144240,3380160,3615960,4574189,13520500,14940153,16359805,17779458,18668431,20068563)
KevinDurant_Salary <- c(0,0,4171200,4484040,4796880,6053663,15506632,16669630,17832627,18995624)
DerrickRose_Salary <- c(0,0,0,4822800,5184480,5546160,6993708,16402500,17632688,18862875)
DwayneWade_Salary <- c(3031920,3841443,13041250,14410581,15779912,14200000,15691000,17182000,18673000,15000000)
Salary <- rbind(KobeBryant_Salary, JoeJohnson_Salary, LeBronJames_Salary, CarmeloAnthony_Salary, DwightHoward_Salary, ChrisBosh_Salary, ChrisPaul_Salary, KevinDurant_Salary, DerrickRose_Salary, DwayneWade_Salary)
rm(KobeBryant_Salary, JoeJohnson_Salary, CarmeloAnthony_Salary, DwightHoward_Salary, ChrisBosh_Salary, LeBronJames_Salary, ChrisPaul_Salary, DerrickRose_Salary, DwayneWade_Salary, KevinDurant_Salary)
colnames(Salary) <- Seasons
rownames(Salary) <- Players

#Games
KobeBryant_G <- c(80,77,82,82,73,82,58,78,6,35)
JoeJohnson_G <- c(82,57,82,79,76,72,60,72,79,80)
LeBronJames_G <- c(79,78,75,81,76,79,62,76,77,69)
CarmeloAnthony_G <- c(80,65,77,66,69,77,55,67,77,40)
DwightHoward_G <- c(82,82,82,79,82,78,54,76,71,41)
ChrisBosh_G <- c(70,69,67,77,70,77,57,74,79,44)
ChrisPaul_G <- c(78,64,80,78,45,80,60,70,62,82)
KevinDurant_G <- c(35,35,80,74,82,78,66,81,81,27)
DerrickRose_G <- c(40,40,40,81,78,81,39,0,10,51)
DwayneWade_G <- c(75,51,51,79,77,76,49,69,54,62)
Games <- rbind(KobeBryant_G, JoeJohnson_G, LeBronJames_G, CarmeloAnthony_G, DwightHoward_G, ChrisBosh_G, ChrisPaul_G, KevinDurant_G, DerrickRose_G, DwayneWade_G)
rm(KobeBryant_G, JoeJohnson_G, CarmeloAnthony_G, DwightHoward_G, ChrisBosh_G, LeBronJames_G, ChrisPaul_G, DerrickRose_G, DwayneWade_G, KevinDurant_G)
colnames(Games) <- Seasons
rownames(Games) <- Players

#Minutes Played
KobeBryant_MP <- c(3277,3140,3192,2960,2835,2779,2232,3013,177,1207)
JoeJohnson_MP <- c(3340,2359,3343,3124,2886,2554,2127,2642,2575,2791)
LeBronJames_MP <- c(3361,3190,3027,3054,2966,3063,2326,2877,2902,2493)
CarmeloAnthony_MP <- c(2941,2486,2806,2277,2634,2751,1876,2482,2982,1428)
DwightHoward_MP <- c(3021,3023,3088,2821,2843,2935,2070,2722,2396,1223)
ChrisBosh_MP <- c(2751,2658,2425,2928,2526,2795,2007,2454,2531,1556)
ChrisPaul_MP <- c(2808,2353,3006,3002,1712,2880,2181,2335,2171,2857)
KevinDurant_MP <- c(1255,1255,2768,2885,3239,3038,2546,3119,3122,913)
DerrickRose_MP <- c(1168,1168,1168,3000,2871,3026,1375,0,311,1530)
DwayneWade_MP <- c(2892,1931,1954,3048,2792,2823,1625,2391,1775,1971)
MinutesPlayed <- rbind(KobeBryant_MP, JoeJohnson_MP, LeBronJames_MP, CarmeloAnthony_MP, DwightHoward_MP, ChrisBosh_MP, ChrisPaul_MP, KevinDurant_MP, DerrickRose_MP, DwayneWade_MP)
rm(KobeBryant_MP, JoeJohnson_MP, CarmeloAnthony_MP, DwightHoward_MP, ChrisBosh_MP, LeBronJames_MP, ChrisPaul_MP, DerrickRose_MP, DwayneWade_MP, KevinDurant_MP)
colnames(MinutesPlayed) <- Seasons
rownames(MinutesPlayed) <- Players

#Field Goals
KobeBryant_FG <- c(978,813,775,800,716,740,574,738,31,266)
JoeJohnson_FG <- c(632,536,647,620,635,514,423,445,462,446)
LeBronJames_FG <- c(875,772,794,789,768,758,621,765,767,624)
CarmeloAnthony_FG <- c(756,691,728,535,688,684,441,669,743,358)
DwightHoward_FG <- c(468,526,583,560,510,619,416,470,473,251)
ChrisBosh_FG <- c(549,543,507,615,600,524,393,485,492,343)
ChrisPaul_FG <- c(407,381,630,631,314,430,425,412,406,568)
KevinDurant_FG <- c(306,306,587,661,794,711,643,731,849,238)
DerrickRose_FG <- c(208,208,208,574,672,711,302,0,58,338)
DwayneWade_FG <- c(699,472,439,854,719,692,416,569,415,509)
FieldGoals <- rbind(KobeBryant_FG, JoeJohnson_FG, LeBronJames_FG, CarmeloAnthony_FG, DwightHoward_FG, ChrisBosh_FG, ChrisPaul_FG, KevinDurant_FG, DerrickRose_FG, DwayneWade_FG)
rm(KobeBryant_FG, JoeJohnson_FG, LeBronJames_FG, CarmeloAnthony_FG, DwightHoward_FG, ChrisBosh_FG, ChrisPaul_FG, KevinDurant_FG, DerrickRose_FG, DwayneWade_FG)
colnames(FieldGoals) <- Seasons
rownames(FieldGoals) <- Players

#Field Goal Attempts
KobeBryant_FGA <- c(2173,1757,1690,1712,1569,1639,1336,1595,73,713)
JoeJohnson_FGA <- c(1395,1139,1497,1420,1386,1161,931,1052,1018,1025)
LeBronJames_FGA <- c(1823,1621,1642,1613,1528,1485,1169,1354,1353,1279)
CarmeloAnthony_FGA <- c(1572,1453,1481,1207,1502,1503,1025,1489,1643,806)
DwightHoward_FGA <- c(881,873,974,979,834,1044,726,813,800,423)
ChrisBosh_FGA <- c(1087,1094,1027,1263,1158,1056,807,907,953,745)
ChrisPaul_FGA <- c(947,871,1291,1255,637,928,890,856,870,1170)
KevinDurant_FGA <- c(647,647,1366,1390,1668,1538,1297,1433,1688,467)
DerrickRose_FGA <- c(436,436,436,1208,1373,1597,695,0,164,835)
DwayneWade_FGA <- c(1413,962,937,1739,1511,1384,837,1093,761,1084)
FieldGoalAttempts <- rbind(KobeBryant_FGA, JoeJohnson_FGA, LeBronJames_FGA, CarmeloAnthony_FGA, DwightHoward_FGA, ChrisBosh_FGA, ChrisPaul_FGA, KevinDurant_FGA, DerrickRose_FGA, DwayneWade_FGA)
rm(KobeBryant_FGA, JoeJohnson_FGA, LeBronJames_FGA, CarmeloAnthony_FGA, DwightHoward_FGA, ChrisBosh_FGA, ChrisPaul_FGA, KevinDurant_FGA, DerrickRose_FGA, DwayneWade_FGA)
colnames(FieldGoalAttempts) <- Seasons
rownames(FieldGoalAttempts) <- Players

#Points
KobeBryant_PTS <- c(2832,2430,2323,2201,1970,2078,1616,2133,83,782)
JoeJohnson_PTS <- c(1653,1426,1779,1688,1619,1312,1129,1170,1245,1154)
LeBronJames_PTS <- c(2478,2132,2250,2304,2258,2111,1683,2036,2089,1743)
CarmeloAnthony_PTS <- c(2122,1881,1978,1504,1943,1970,1245,1920,2112,966)
DwightHoward_PTS <- c(1292,1443,1695,1624,1503,1784,1113,1296,1297,646)
ChrisBosh_PTS <- c(1572,1561,1496,1746,1678,1438,1025,1232,1281,928)
ChrisPaul_PTS <- c(1258,1104,1684,1781,841,1268,1189,1186,1185,1564)
KevinDurant_PTS <- c(903,903,1624,1871,2472,2161,1850,2280,2593,686)
DerrickRose_PTS <- c(597,597,597,1361,1619,2026,852,0,159,904)
DwayneWade_PTS <- c(2040,1397,1254,2386,2045,1941,1082,1463,1028,1331)
Points <- rbind(KobeBryant_PTS, JoeJohnson_PTS, LeBronJames_PTS, CarmeloAnthony_PTS, DwightHoward_PTS, ChrisBosh_PTS, ChrisPaul_PTS, KevinDurant_PTS, DerrickRose_PTS, DwayneWade_PTS)
rm(KobeBryant_PTS, JoeJohnson_PTS, LeBronJames_PTS, CarmeloAnthony_PTS, DwightHoward_PTS, ChrisBosh_PTS, ChrisPaul_PTS, KevinDurant_PTS, DerrickRose_PTS, DwayneWade_PTS)
colnames(Points) <- Seasons
rownames(Points) <- Players

# --- NEW DATA: Generating plausible sample data for Free Throws (FT) and Free Throw Attempts (FTA) ---
cat("Generating sample Free Throw and Free Throw Attempts data...\n")
# For each player, generate FTAs, then FTs as a percentage of FTAs
# Base FTAs will be somewhat proportional to Points to be plausible
# FTs will be a percentage of FTAs (e.g., 60-90% accuracy, with some variance)

# Initialize empty lists to store vectors
ft_list <- list()
fta_list <- list()

for (player in Players) {
  # Estimate baseline FTA based on Points (very rough heuristic)
  player_points <- Points[player, ]
  # Assume FTAs are roughly 20-40% of total points if 1 point per FT
  # Adjust for seasons with 0 points (e.g., Derrick Rose 2012)
  base_fta <- ifelse(player_points == 0, 0, round(player_points * runif(length(Seasons), 0.2, 0.4)))
  
  # For seasons with 0 Games (e.g. Derrick Rose 2012), FT and FTA should be 0
  player_games <- Games[player, ]
  base_fta[player_games == 0] <- 0 # Ensure FTA is 0 if games played is 0
  
  # Add some randomness to FTA
  player_fta <- pmax(0, base_fta + round(rnorm(length(Seasons), 0, 50))) # Ensure non-negative
  
  # Generate FTs based on FTAs and a random accuracy
  # Accuracy can vary by player and season (e.g., 0.6 to 0.9)
  # Dwight Howard is known for lower FT%, so let's give him a lower range
  if (player == "DwightHoward") {
    accuracy <- runif(length(Seasons), 0.45, 0.65)
  } else {
    accuracy <- runif(length(Seasons), 0.70, 0.90)
  }
  player_ft <- round(player_fta * accuracy)
  player_ft <- pmin(player_ft, player_fta) # Ensure FT <= FTA
  player_ft[player_games == 0] <- 0 # Ensure FT is 0 if games played is 0
  
  ft_list[[player]] <- player_ft
  fta_list[[player]] <- player_fta
}

FreeThrows <- do.call(rbind, ft_list)
FreeThrowAttempts <- do.call(rbind, fta_list)

colnames(FreeThrows) <- Seasons
rownames(FreeThrows) <- Players
colnames(FreeThrowAttempts) <- Seasons
rownames(FreeThrowAttempts) <- Players

# --- Sanity Check: Ensure all FT <= FTA ---
if(any(FreeThrows > FreeThrowAttempts, na.rm = TRUE)) {
  stop("Error: Free Throws Made (FT) cannot be greater than Free Throws Attempted (FTA). Check data generation.")
} else {
  print("Sample FT and FTA data generated and validated (FT <= FTA).")
}
# --- End Data Preparation ---


# --- Task: Create three plots ---
# Helper function to convert matrix to long format for ggplot
matrix_to_long_df <- function(matrix_data, value_name) {
  df <- as.data.frame(matrix_data)
  df$Player <- rownames(df)
  df_long <- pivot_longer(df,
                          cols = -Player,
                          names_to = "Season",
                          values_to = value_name)
  df_long$Season <- factor(df_long$Season, levels = Seasons) # Ensure correct order
  return(df_long)
}

# Get a nice color palette
num_players <- length(Players)
player_colors <- brewer.pal(max(3, min(num_players, 9)), "Set1")
if (num_players > 9) {
  player_colors <- colorRampPalette(brewer.pal(9, "Set1"))(num_players)
}


# 1. Free Throw Attempts per game
# Handle division by zero (when Games == 0, result should be 0 or NA)
FTA_per_Game <- FreeThrowAttempts / Games
FTA_per_Game[is.nan(FTA_per_Game)] <- 0
FTA_per_Game[is.infinite(FTA_per_Game)] <- 0

fta_pg_df <- matrix_to_long_df(FTA_per_Game, "FTA_per_Game")

plot1 <- ggplot(fta_pg_df, aes(x = Season, y = FTA_per_Game, group = Player, color = Player)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = player_colors) +
  labs(title = "Free Throw Attempts per Game by Player",
       x = "Season",
       y = "Free Throw Attempts per Game") +
  theme_minimal(base_size = 10) + # Adjusted base_size for potentially smaller plot windows
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
print(plot1)


# 2. Accuracy of Free Throws
# Handle division by zero (when FreeThrowAttempts == 0, result should be 0 or NA)
FT_Accuracy <- FreeThrows / FreeThrowAttempts
FT_Accuracy[is.nan(FT_Accuracy)] <- 0
FT_Accuracy[is.infinite(FT_Accuracy)] <- 0

ft_accuracy_df <- matrix_to_long_df(FT_Accuracy, "FT_Accuracy")

plot2 <- ggplot(ft_accuracy_df, aes(x = Season, y = FT_Accuracy, group = Player, color = Player)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = player_colors) +
  labs(title = "Accuracy of Free Throws by Player",
       x = "Season",
       y = "Free Throw Accuracy") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
print(plot2)


# 3. Player playing style (2 vs 3 points preference) excluding Free Throws*
# Hint: (Points - FreeThrows) / FieldGoals
Points_from_FG <- Points - FreeThrows
Avg_Points_per_FG <- Points_from_FG / FieldGoals
Avg_Points_per_FG[is.nan(Avg_Points_per_FG)] <- 0
Avg_Points_per_FG[is.infinite(Avg_Points_per_FG)] <- 0
# Ensure no negative values if FT > Points (should not happen with correct data)
Avg_Points_per_FG[Avg_Points_per_FG < 0] <- 0


player_style_df <- matrix_to_long_df(Avg_Points_per_FG, "Avg_Points_per_FG")

plot3 <- ggplot(player_style_df, aes(x = Season, y = Avg_Points_per_FG, group = Player, color = Player)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = player_colors) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "grey50") +
  annotate("text", x = Seasons[1], y = 2.05, label = "Avg 2 Pts / Made FG", hjust = 0, vjust = -0.5, size=3, color="grey30") +
  labs(title = "Player Playing Style (Avg Points per Made Field Goal, excl. FTs)",
       subtitle = "Higher values indicate more 3-point shots contributing to FG points",
       x = "Season",
       y = "Average Points per Made Field Goal") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
print(plot3)

print("--- End of Script ---")

