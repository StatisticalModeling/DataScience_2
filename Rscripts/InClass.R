# Read in data
Credit <- read.csv("http://statlearning.com/s/Credit.csv")
mod_null <- lm(Balance ~ 1, data = Credit)
mod_full <- lm(Balance ~ ., data = Credit)
library(MASS)
stepAIC(mod_null, scope = list(lower = mod_null, upper = mod_full), direction = "forward")
####
mod_aic <- lm(Balance ~ Rating + Income + Student + Limit + Cards + Age, data = Credit)
summary(mod_aic)

mod_for <- mod_null
SCOPE <- (~ Income + Limit + Rating + Cards + Age + Education + Own + Student + Married + Region)

add1(mod_for, scope = SCOPE, test = "F")
mod_for <- update(mod_for, .~. + Rating)
add1(mod_for, scope = SCOPE, test = "F")
mod_for <- update(mod_for, .~. + Income)
add1(mod_for, scope = SCOPE, test = "F")
mod_for <- update(mod_for, .~. + Student)
add1(mod_for, scope = SCOPE, test = "F")
mod_for <- update(mod_for, .~. + Limit)
add1(mod_for, scope = SCOPE, test = "F")
mod_for <- update(mod_for, .~. + Cards)
add1(mod_for, scope = SCOPE, test = "F")
mod_for <- update(mod_for, .~. + Age)
add1(mod_for, scope = SCOPE, test = "F")

####################
mod_be <- lm(Balance ~ ., data = Credit)
drop1(mod_be, scope = SCOPE, test = "F")
mod_be <- update(mod_be, .~. - Education)
drop1(mod_be, test = "F")
mod_be <- update(mod_be, .~. - Region)
drop1(mod_be, test = "F")
mod_be <- update(mod_be, .~. - Married)
drop1(mod_be, test = "F")
mod_be <- update(mod_be, .~. - Own)
drop1(mod_be, test = "F")
