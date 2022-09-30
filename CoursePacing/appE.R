## ---- label = "SETUP", echo = FALSE, message = FALSE, warning = FALSE----
knitr::opts_chunk$set(comment = NA, message = FALSE, warning = FALSE, fig.align = "center")
library(tidyverse)
library(mosaic)


## ---------------------------------------------------------------
library(mdsr)
library(mosaicData)
glimpse(RailTrail)


## ---------------------------------------------------------------
mod <- lm(volume ~ hightemp, data = RailTrail)
coef(mod)


## ---------------------------------------------------------------
plotModel(mod, system = "ggplot2") + 
  theme_bw()
####
ggplot(data = RailTrail, 
       mapping = aes(x = hightemp, y = volume)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

## ---------------------------------------------------------------
slr <- lm(volume ~ hightemp, data = RailTrail)
coef(slr)
null <- lm(volume ~ 1, data = RailTrail)
coef(null)
library(broom)
ndf1 <- augment(slr)
ndf2 <- augment(null)
ndf2$hightemp <- RailTrail$hightemp
ndf <- bind_rows(ndf1, ndf2) %>% 
  mutate(model = rep(c("slr", "null"), each = 90))


## ---- label = "SP1", fig.cap = "Scatterplot of number of trail crossings as a function of highest daily temperature (in degrees Fahrenheit)."----
library(ggplot2)
ndf %>% 
  filter(model == "slr") %>% 
ggplot(aes(x = hightemp, y = volume)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()


## ---- label = "RESIDS", fig.width = 10, echo = FALSE, fig.cap = "At left, the model based on the overall average high temperature.  At right, the simple linear regression model."----
ggplot(data = ndf, aes(x = hightemp, y = volume)) +
  facet_grid(cols = vars(model)) + 
  geom_line(aes(x = hightemp, y = .fitted), color = "gray") + 
  theme_bw() + 
  geom_segment(aes(x = hightemp, xend = hightemp, y = volume, yend = .fitted), size = 0.25) + 
  geom_point(color = "#cc00ff") 


## ---------------------------------------------------------------
n <- nrow(RailTrail)
SST <- var(~volume, data = RailTrail) * (n - 1)
SSE <- var(residuals(mod)) * (n - 1)
1 - SSE / SST
# Or
rsquared(mod)
# Or by the formula...
# sum(residuals(slr)^2)



## ---------------------------------------------------------------
ndf %>% 
  group_by(model) %>% 
  summarize(Rsq = 1 - sum((volume - .fitted)^2)/sum((volume - mean(volume))^2))


## ---------------------------------------------------------------
modc <- lm(volume ~ weekday, data = RailTrail)
coef(modc)


## ---------------------------------------------------------------
RailTrail %>% 
  group_by(weekday) %>% 
  summarize(MEAN = mean(volume))
# Approach used in book
library(mosaic)
MS <- mean(volume ~ weekday, data = RailTrail)
MS
diff(MS)


## ---------------------------------------------------------------
RailTrail <- RailTrail %>% 
  mutate(day = ifelse(weekday == 1, "weekday", "weekend/holiday"))


## ---------------------------------------------------------------
modc1 <- lm(volume ~ day, data = RailTrail)
coef(modc1)


## ---------------------------------------------------------------
mod_parallel <- lm(volume ~ hightemp + weekday, data = RailTrail)
coef(mod_parallel)
summary(mod_parallel)


## ---- label = "PS", fig.cap = "Visualization of parallel slopes for the rail trail data."----
DF4 <- augment(mod_parallel)
ggplot(data = DF4, aes(x = hightemp, y = volume, color = weekday)) + 
  geom_point() +
  geom_line(aes(x = hightemp, y = .fitted)) + 
  theme_bw() + 
  scale_color_manual(values = c("blue", "green"))
####################
library(moderndive)
ggplot(data = DF4, aes(x = hightemp, y = volume, color = weekday)) + 
  geom_point() +
  geom_parallel_slopes(se = FALSE) + 
  theme_bw() + 
  scale_color_manual(values = c("blue", "green"))
## ---------------------------------------------------------------
DF4 <- augment(mod_parallel)
DF5 <- modelr::data_grid(DF4, hightemp, weekday) %>% 
  modelr::gather_predictions(mod_parallel)
ggplot(data = DF4, aes(x = hightemp, y = volume, color = weekday)) + 
  geom_point() +
  geom_line(data = DF5, aes(y = pred)) + 
  theme_bw() + 
  scale_color_manual(values = c("blue", "green"))


## ---------------------------------------------------------------
plotModel(mod_parallel, system = "ggplot2") + 
  theme_bw() + 
  labs(color = "weekday") + 
  scale_color_manual(values = c("blue", "green"))


## ---------------------------------------------------------------
plotModel(lm(volume ~ hightemp + day, data = RailTrail), system = "ggplot2") + 
  theme_bw() + 
  labs(color = "Day") + 
  scale_color_manual(values = c("green", "blue"))


## ---------------------------------------------------------------
mod_planes <- lm(volume ~ hightemp + precip, data = RailTrail)
coef(mod_planes)


## ---------------------------------------------------------------
RailTrail %>% 
  mutate(Rain = ifelse(precip == 0.00, "No", "Yes")) %>% 
  group_by(Rain) %>% 
  summarize(Median = median(precip))


## ---------------------------------------------------------------
mod_p_planes <- lm(volume ~ hightemp + precip + weekday, data = RailTrail)
coef(mod_p_planes)


## ---------------------------------------------------------------
mod_interact <- lm(volume ~ hightemp + weekday + hightemp:weekday, data = RailTrail)
coef(mod_interact)
summary(mod_interact)


## ---- label = "IM", fig.cap = "Visualization of interaction model for the rail trail data."----
ggplot(data = RailTrail, aes(x = hightemp, y = volume, color = weekday)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()


## ---------------------------------------------------------------
plotModel(mod_interact, system = "ggplot2") + 
  labs(color = "Weekday") + 
  theme_bw()


## ---- label = "LP", fig.cap = "Scatterplot of height as a function of age with superimposed linear model (blue) and smoother (green)."----
library(NHANES)
set.seed(21)
NHANES %>% 
  sample(300) %>% 
  filter(Gender == "female") %>% 
  ggplot(aes(x = Age, y = Height)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE) + 
  stat_smooth(method = "loess", se = FALSE, color = "green") +
  labs(x = "Age (in years)", y = "Height (in cm)") + 
  theme_bw()


## ---- label = "CLL", fig.cap = "Scatterplot of volume as a function of high temperature with superimposed linear and smooth models for the rail trail data."----
ggplot(data = RailTrail, aes(x = hightemp, y = volume)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  stat_smooth(method = "loess", color = "green") + 
  labs(y = "Number of trail crossings", x = "High temperature (F)") + 
  theme_bw()


## ---------------------------------------------------------------
summary(mod_p_planes)


## ---------------------------------------------------------------
CIS <- confint(mod_p_planes, level = 0.95)
CIS


## ---- label = "resid", fig.cap = "Assessing linearity using a scatterplot of residuals versus fitted (predicted) values."----
mplot(mod_p_planes, which = 1, system = "ggplot2")


## ---- label = "nqq", fig.cap = "Assessing normality assumption using a Q-Q plot."----
mplot(mod_p_planes, which = 2, system = "ggplot2") 


## ---- label = "locscale", fig.cap = "Assessing equal variance using a scale-location plot."----
mplot(mod_p_planes, which = 3, system = "ggplot2")


## ---- label = "cook", fig.cap = "Cook's distance for rail trail model"----
mplot(mod_p_planes, which = 4, system = "ggplot2")


## ---------------------------------------------------------------
library(broom)
augment(mod_p_planes) %>% 
  filter(.cooksd > 0.4)


## ---------------------------------------------------------------
NHANES <- NHANES %>% 
  mutate(has_diabetes = as.numeric(Diabetes == "Yes"))
log_plot <- ggplot(data = NHANES, aes(x = Age, y = has_diabetes)) + 
  geom_jitter(alpha = 0.1, height = 0.05) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"))


## ---- label = "logreg", fig.cap = "Scatterplot of diabetes as a function of age with superimposed smoother."----
log_plot +
  labs(x = "Age (in years)", y = "Diabetes status")


## ---- label = "logreg2", fig.cap = "Scatterplot of diabetes as a function of BMI with superimposed smoother."----
log_plot +
  aes(x = BMI) + 
  labs(x = "BMI (body mass index)", y = "Diabetes status")


## ---------------------------------------------------------------
logreg <- glm(has_diabetes ~ BMI + Age, family = "binomial", data = NHANES)
summary(logreg)


## ---------------------------------------------------------------
ages <- range(~Age, data = NHANES)
bmis <- range(~BMI, data = NHANES, na.rm = TRUE)
res <- 100
fake_grid <- expand.grid(
  Age = seq(from = ages[1], to = ages[2], length.out = res),
  BMI = seq(from = bmis[1], to = bmis[2], length.out = res)
)
y_hats <- fake_grid %>% 
  mutate(y_hat = predict(logreg, newdata = ., type = "response"))


## ---- label = "gofigure", fig.cap = "Predicted probabilities for diabetes as a function of BMI and age."----
ggplot(data = NHANES, aes(x = Age, y = BMI)) + 
  geom_tile(data = y_hats, aes(fill = y_hat), color = NA) + 
  geom_count(aes(color = as.factor(has_diabetes)), alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "dodgerblue") + 
  scale_color_manual("Diabetes", values = c("green", "red")) + 
  scale_size(range = c(0, 2)) +
  labs(fill = expression(hat(y)))

