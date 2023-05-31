library(tidyverse)
library(stringr)
library(readr)

## Do ANOVA between groups for Drinking Age and Religion to see which subgroups
## you can combine. Then, do it again with new groups (in a sensible way). Next,
## after one-hot coding them as binary for multiple regression, select the reference
## category from each variable to be the one that stands apart from the others.
## Consider doing this for civil liberties and regime as well. 
## Also, center consumption variables (subtract of means).
## Finally, check model for normality conditions.

alc.inc.2019 <- DrugDataSetFull %>% 
  filter(year == 2019) %>%
  distinct() %>%
  filter(sex == "Both" & measure == "Incidence" & cause == "Alcohol use disorders") %>%
  select(7:11,13:15,17:18)

## ANOVAS

Christianity <- alc.inc.2019 %>% 
  filter(Religion == "Christianity")
Islam <- alc.inc.2019 %>% 
  filter(Religion == "Islam")
Other <- alc.inc.2019 %>% 
  filter(Religion == "Other")

sum(Christianity$val/135)
sum(Islam$val/49)
sum(Other$val/21)
religion <- lm(val ~ Religion, data = alc.inc.2019)
anova(religion)
summary(religion)

drinkage <- lm(val ~ AgeToDrink.2016, data = alc.inc.2019)
anova(drinkage)
summary(drinkage)
