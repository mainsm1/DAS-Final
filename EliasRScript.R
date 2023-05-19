library(tidyverse)
library(readr)
library(stringr)

data <- DrugDataSetFull %>% 
  filter(year == 2019 | year == 2009 | year == 1999) %>%
  mutate_at(vars(Religion, civlibs_fh), as.factor) %>%
  distinct()

# ALCOHOL INCIDENCE dataframes
alc.inc.2019 <- data %>%
  filter(sex == "Both" & measure == "Incidence" & cause == "Alcohol use disorders" & year == 2019)

alc.inc.2009 <- data %>%
  filter(sex == "Both" & measure == "Incidence" & cause == "Alcohol use disorders" & year == 2009)

alc.inc.1999 <- data %>%
  filter(sex == "Both" & measure == "Incidence" & cause == "Alcohol use disorders" & year == 1999)
 

  y1999 <- DrugFullDataset %>%
  filter(year == 1999)

y2009 <- DrugFullDataset %>%
  filter(year == 2009)

y2019 <- DrugFullDataset %>%
  filter(year == 2019)


# Drinking Age Groups 
# Burkina Faso is the only 13 y.o.
# Mali is the only 15 y.o.
# 16 countries have 16 y.o.
# Cyprus and Malta have 17 y.o.
# 114 countries have 18 y.o.
# Canada and Korea have 19 y.o.
# 5 countries have 20 y.o.
# 17 countries have 21 y.o.
# Eritrea is the only 25 y.o.
# 10 countries have Total Ban
# 12 countries have None

##do initial analysis with the five categories and then if things look like 
## some of the smaller groups are similar you can combine them

drinkage <- mutate_at(drinkage, vars(Age), as.factor)
##join partitioned drinkage with huge set
bigboy <- DrugDataSetFull %>%
  select(-9) %>%
  mutate_at(vars(Religion, civlibs_fh), as.factor) %>%
  left_join(drinkage, by = c("location" = "Country")) %>%
  mutate_at(vars(Group,Age), as.factor)


## these chi-square tests show that all these variables are NOT independent.
chisq.test(alc.inc.2019$Religion,alc.inc.2019$regime_row_owid)
chisq.test(alc.inc.2019$Religion,alc.inc.2019$civlibs_fh)
chisq.test(alc.inc.2019$regime_row_owid,alc.inc.2019$civlibs_fh)

alc.inc.2019


