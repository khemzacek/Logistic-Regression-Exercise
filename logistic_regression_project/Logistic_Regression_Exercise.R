
# Exercise: logistic regression -------------------------------------------


##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.


# Load packages
library(dplyr)
library(effects)

# Load data
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels


# Examine variables
str(NH11[c("everwrk", "age_p", "r_maritl")])

# everwrk
levels(NH11$everwrk)
summary(NH11$everwrk)
NH11$everwrk <- factor(NH11$everwrk, c("1 Yes", "2 No"))
# all missing values-> NA
summary(NH11$everwrk) 
# LOTS of NAs (before and after refactoring)

# age_p
hist(NH11$age_p)
summary(NH11$age_p)
# mostly normally distributed, but no kids under 18, and
# 80-85 bin bigger than expected and 85 is max age <- likely product of survey
# methods (anyone 85 or older coded as 85?); cut off that bin
NH11 <- filter(NH11, age_p <= 80)
summary(NH11$age_p)

# r_maritl
levels(NH11$r_maritl)
summary(NH11$r_maritl)
NH11$r_maritl <- factor(NH11$r_maritl, 
                        c("1 Married - spouse in household",
                          "2 Married - spouse not in household",
                          "4 Widowed", "5 Divorced", "6 Separated",
                          "7 Never married", "8 Living with partner"))
# assign missing values as NAs, remove empty levels
summary(NH11$r_maritl)


# Create logistic regression model
wrk_mod <- glm(everwrk ~ age_p + r_maritl, data = NH11, family = "binomial")
summary(wrk_mod)


# Predict everwrk for each level of r_maritl
predData_maritl <- with(NH11, expand.grid(r_maritl = levels(r_maritl),
                                          age_p = mean(age_p, na.rm = TRUE)))
wrkPred_maritl <- predict(wrk_mod, type = "response", newdata = predData_maritl)
cbind(predData_maritl, wrkPred_maritl)

# Plot using effects package
plot(effect("r_maritl", wrk_mod))
