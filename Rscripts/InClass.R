# Read in data
Credit <- read.csv("http://statlearning.com/s/Credit.csv")
mod_null <- lm(Rating ~ 1, data = Credit)
mod_full <- lm(Rating ~ ., data = Credit)
library(MASS)
stepAIC(mod_null, scope = list(lower = mod_null, upper = mod_full), direction = "forward")
####
mod_aic <- lm(Rating ~ Limit + Cards + Married + Student + Education, data = Credit)
summary(mod_aic)

mod_for <- mod_null
?add1

