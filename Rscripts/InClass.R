# Read in data
Credit <- read.csv("http://statlearning.com/s/Credit.csv")
names(Credit)
library(tidyverse)
Credit %>% 
  summarize(MB = mean(Balance),
          n = n(),
          SD = sd(Balance))
ggplot(data = Credit, aes(x = Balance)) + 
  geom_histogram(fill = "blue", color = "purple") +
  theme_bw()

mod_null <- lm(Balance ~ 1, data = Credit)
mod_full <- lm(Balance ~ ., data = Credit)
library(MASS)
stepAIC(mod_null, scope = list(lower = mod_null, upper = mod_full), 
        direction = "forward")
####
mod_aic <- lm(Balance ~ Rating + Income + Student + Limit + 
                Cards + Age, data = Credit)
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

###################
## Answering the questions

mod_null <- lm(Rating ~ 1, data = Credit)
mod_full <- lm(Rating ~ ., data = Credit)
mod_for <- stepAIC(mod_null, scope = list(lower = mod_null, upper = mod_full), direction = "forward")
summary(mod_for)
mod_bac <- stepAIC(mod_full, scope = list(lower = mod_null, upper = mod_full), direction = "backward")
summary(mod_bac)
####
mod_five <- lm(Rating ~ Limit + Cards + Married + Student + Education, data = Credit)
summary(mod_five)
####
# Credit Rating
predict(mod_five, newdata = data.frame(Limit = c(6000, 12000), 
                                       Cards = c(4, 4), 
                                       Married = c("Yes", "Yes"),
                                       Student = c("No", "No"), 
                                       Education = c(16, 8)),
        interval = "conf", level = .99)

predict(mod_five, newdata = data.frame(Limit = c(6000, 12000), 
                                       Cards = c(4, 4), 
                                       Married = c("Yes", "Yes"),
                                       Student = c("No", "No"), 
                                       Education = c(16, 8)),
        interval = "pred", level = .99)

#### Linear Algebra

X <- model.matrix(mod_five)
X
XTX <- t(X)%*%X
XTX
XTXI <- solve(XTX)
zapsmall(XTXI %*% XTX)
y <- Credit$Rating
betahat <- XTXI%*%t(X)%*%y
betahat
summary(mod_five)$coef
### recall that the better way to get XTXI is with cov.unscaled
summary(mod_five)$cov.unscaled -> XTXI
XTXI
MSE <- summary(mod_five)$sigma^2
MSE
####
anova(mod_five)
anova(mod_five)[6,3] -> MSE
####
varcov <- MSE*XTXI
varcov

