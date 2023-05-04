
us<- data.frame(df1$State,df1$avg_incidence,df1$avg_death)
us

us1 <- data.frame(df1$State,df1$avg_incidence*100/df1$mean_pop,df1$avg_death*100/df1$mean_pop)
us1
#Northeast: Includes Connecticut, Maine, Massachusetts, New Hampshire, New Jersey, New York, Pennsylvania, Rhode Island, and Vermont
#Midwest: Includes Illinois, Indiana, Iowa, Kansas, Michigan, Minnesota, Missouri, Nebraska, North Dakota, Ohio, South Dakota, and Wisconsin
#South: Includes Alabama, Arkansas, Delaware, District of Columbia, Florida, Georgia, Kentucky, Louisiana, Maryland, Mississippi, North Carolina, Oklahoma, South Carolina, Tennessee, Texas, Virginia, and West Virginia
#West: Includes Alaska, Arizona, California, Colorado, Hawaii, Idaho, Montana, Nevada, New Mexico, Oregon, Utah, Washington, and Wyoming


df1 <- df1 %>% mutate(Region = case_when(
  State %in% c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT") ~ "Northeast",
  State %in% c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI") ~ "Midwest",
  State %in% c("AL", "AR", "DE", "DC", "FL", "GA", "KY", "LA", "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV") ~ "South",
  State %in% c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")~ "West",
  TRUE ~ NA_character_
))
df1

us <- us %>% mutate(Region = case_when(
  df1.State %in% c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT") ~ "Northeast",
  df1.State %in% c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI") ~ "Midwest",
  df1.State %in% c("AL", "AR", "DE", "DC", "FL", "GA", "KY", "LA", "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV") ~ "South",
  df1.State %in% c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")~ "West",
  TRUE ~ NA_character_
))
us
Cancer <- Cancer %>% mutate(Region = case_when(
  State %in% c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT") ~ "Northeast",
  State %in% c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI") ~ "Midwest",
  State %in% c("AL", "AR", "DE", "DC", "FL", "GA", "KY", "LA", "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV") ~ "South",
  State %in% c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")~ "West",
  TRUE ~ NA_character_
))

region_avg <- us %>% 
  group_by(Region) %>% 
  summarize(Average_Incidence_Rate = mean(df1.avg_incidence, na.rm = TRUE),Average_Death_Rate = mean(df1.avg_death, na.rm = TRUE))

# Scatter plot for Q1_c

incidence_model <- lm(incidenceRate ~ PovertyEst + medIncome + popEst2015, data = Cancer)
death_model <- lm(deathRate ~ PovertyEst + medIncome + popEst2015, data = Cancer)

plot(incidence_model, which = 1, pch = 16, col = "red", main = "Residuals vs. Fitted Values (Incidence Rate)")
plot(death_model, which = 1, pch = 16, col = "red", main = "Residuals vs. Fitted Values (Death Rate)")
plot(incidence_model, which = 3, pch = 16, col = "red", main = "Scale-Location (Incidence Rate)")
plot(death_model, which = 3, pch = 16, col = "red", main = "Scale-Location (Death Rate)")
