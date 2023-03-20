library(simputation)
library(tidyverse)
?airquality
summary(airquality)
aq <- airquality %>% 
  impute_lm(Ozone ~ Solar.R + Wind + Temp) %>% 
  impute_lm(Solar.R ~ Wind + Temp) %>% 
  impute_lm(Ozone ~ Solar.R + Wind + Temp)
summary(aq)
