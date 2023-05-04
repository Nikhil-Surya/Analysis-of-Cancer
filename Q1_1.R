#1
library(dplyr)
library(ggplot2)


# Filter for columns of interest
filtered_data <- Cancer %>%
  select(countyCode, State,Name, incidenceRate)

# Sort by incidence rate in descending order
sorted_data <- filtered_data %>%
  arrange(desc(incidenceRate))

# Get top 10 counties
top_10_counties <- sorted_data %>%
  head(10)

# Create a bar chart
bar_chart <- ggplot(top_10_counties, aes(x = Name, y = incidenceRate, fill = State)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Top 10 Counties Most Prone to Cancer",
       x = "County Code", y = "Incidence Rate") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set3")

# Display the bar chart
print(bar_chart)


# for death rate


# Filter for columns of interest
dfiltered_data <- Cancer %>%
  select(countyCode, State,Name, deathRate)

# Sort by incidence rate in descending order
dsorted_data <- dfiltered_data %>%
  arrange(desc(deathRate))

# Get top 10 counties
dtop_10_counties <- dsorted_data %>%
  head(10)

# Create a bar chart
bar_chart <- ggplot(dtop_10_counties, aes(x = Name, y = deathRate, fill = State)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Top 10 Counties Most Prone to Cancer",
       x = "County Code", y = "Death Rate") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set3")

# Display the bar chart
print(bar_chart)

# for regions:

bar_chart <- ggplot(region_avg, aes(x = Region, y = Average_Death_Rate, fill = Region)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Top 10 Counties Most Prone to Cancer",
       x = "Region", y = "death Rate") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set3")

print(bar_chart)



data <- data.frame(
  Region = c("Midwest", "Northeast", "South", "West"),
  Average_Incidence_Rate = c(447.6289, 483.4178, 459.7986, 415.2173),
  Average_Death_Rate = c(173.2646, 170.4144, 189.4270, 159.0033)
)

# Convert data to long format
data_long <- tidyr::gather(data, key = "Rate_Type", value = "Rate", -Region)

# Create a two-valued bar chart
two_valued_bar_chart <- ggplot(data_long, aes(x = Region, y = Rate, fill = Rate_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Comparison of Average Incidence Rate and Average Death Rate by Region",
       x = "Region", y = "Rate") +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "coral")) +
  theme(legend.position = "bottom")

# Display the two-valued bar chart
print(two_valued_bar_chart)
