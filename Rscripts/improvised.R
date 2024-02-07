library(BSDA)
Gpa
library(tidyverse)
ggplot(data = Gpa, aes(x = hsgpa, y = collgpa)) + 
  geom_point(color = "blue", size = 5) + 
  geom_smooth(method = "lm")

values <- Gpa %>% 
  mutate(y_ybar = collgpa - mean(collgpa),
         x_xbar = hsgpa - mean(hsgpa),
         z_y = y_ybar/sd(collgpa),
         z_x = x_xbar/sd(hsgpa))
values %>% 
  summarize(r = 1/9*sum(z_x*z_y))
Gpa %>% 
  summarize(R = cor(hsgpa, collgpa))
(r <- cor(Gpa$hsgpa, Gpa$collgpa))
(b1 <- r*sd(Gpa$collgpa)/sd(Gpa$hsgpa))
(b0 <- mean(Gpa$collgpa) - b1*mean(Gpa$hsgpa))


model <- lm(collgpa ~ hsgpa, data = Gpa)
model
summary(model)
summary(model)$coef
(tstar <- summary(model)$coef[2, 3])
(1 - pt(tstar, 8))*2
(pt(tstar, 8, lower = FALSE))*2
predict(model)
resid(model)
