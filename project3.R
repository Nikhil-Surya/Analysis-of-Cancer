#measure of the amount of multicollinearity in regression analysis

install.packages("car")
library(car)
fit1 = lm(deathRate ~ PovertyEst + popEst2015 + medIncome + incidenceRate  + Region + medIncome  ,  data = Cancer)
summary(fit1)
vif(fit1)

fit1 = lm(incidenceRate ~ PovertyEst + popEst2015 + medIncome + deathRate  + Region + medIncome  ,  data = Cancer)
summary(fit1)
vif(fit1)

#ANOVA
library(tidyverse)

# Load the dataset


# Perform ANOVA analysis
# Fit a one-way ANOVA model
anova_model <- aov(incidenceRate ~ State, data = Cancer)

anova_model <- aov(incidenceRate ~ Region, data = Cancer)
anova_model <- aov(incidenceRate ~ deathRate, data = Cancer)
anova_model <- aov(incidenceRate ~ PovertyEst, data = Cancer)
# Summary of the ANOVA model
summary(anova_model)

# Check ANOVA assumptions
# Residual plot
plot(anova_model, 1)