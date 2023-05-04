# Load required libraries
library(dplyr)
library(ggplot2)
library(Hmisc)

# Load data - assuming you have a dataset named "cancer_data" with relevant variables
# Replace the variable names and dataset name with your own data

# Create a 4-level indicator variable for Median Income
Cancer$Median_Income_Indicator <- cut(Cancer$medIncome, 
                                           breaks = c(0, 25000, 50000, 75000, Inf), 
                                           labels = c("Very Low", "Low", "High", "Very High"))

# Conduct descriptive analysis of cancer incidence and death rates by Median Income Indicator
summary(Cancer$incidenceRate)
summary(Cancer$deathRate)

# Compare cancer incidence and death rates by Median Income Indicator
Cancer %>%
  group_by(Median_Income_Indicator) %>%
  summarise(mean_Incidence_Rate = mean(incidenceRate),
            mean_Death_Rate = mean(deathRate)) %>%
  ggplot(aes(x = Median_Income_Indicator, y = mean_Incidence_Rate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(mean_Incidence_Rate, 2)), vjust = -0.5) +
  labs(title = "Mean Cancer Incidence Rate by Median Income Indicator",
       x = "Median Income Indicator",
       y = "Mean Incidence Rate") +
  theme_minimal()

ggplot(aes(x = Median_Income_Indicator, y = mean_Death_Rate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(mean_Death_Rate, 2)), vjust = -0.5) +
  labs(title = "Mean Cancer Death Rate by Median Income Indicator",
       x = "Median Income Indicator",
       y = "Mean Death Rate") +
  theme_minimal()

# Compare cancer incidence and death rates by location
Cancer %>%
  group_by(State) %>% 
  summarise(mean_Incidence_Rate = mean(incidenceRate)) %>%
  arrange(desc(mean_Incidence_Rate)) %>%
  ggplot(aes(x = reorder(State, mean_Incidence_Rate), y = mean_Incidence_Rate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(mean_Incidence_Rate, 2)), vjust = -0.5) +
  labs(title = "Mean Cancer Incidence Rate by Location",
       x = "Location",
       y = "Mean Incidence Rate") +
  theme_minimal()


# Correlation

install.packages("corrplot")
# Load necessary libraries
library(corrplot)

# Select variables of interest from the 'Cancer' dataset
selected_vars <- Cancer[, c("incidenceRate", "deathRate", "medIncome", "PovertyEst", "avgDeathsPerYear", "avgAnnCount", "popEst2015")]

# Calculate correlation matrix
corr_matrix <- cor(selected_vars)

# Create a heatmap of correlation
print (corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black", col = colorRampPalette(c("#D73027", "#FFFFBF", "#1A9850"))(100), main = "Correlation Heatmap")
)
