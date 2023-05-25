library(tidyverse)
library(readr)
library(stringr)

data <- DrugDataSetFull %>% 
  filter(year == 2019 | year == 2009 | year == 1999) %>%
  distinct()

colnames(data)[colnames(data)=="Unemployment, total (% of total labor force) (modeled ILO estimate)"] <- "Unemployment"

# ALCOHOL INCIDENCE dataframes
alc.inc.2019 <- data %>%
  filter(sex == "Both" & measure == "Incidence" & cause == "Alcohol use disorders" & year == 2019) %>%
  select(-12,-17)

alc.inc.2009 <- data %>%
  filter(sex == "Both" & measure == "Incidence" & cause == "Alcohol use disorders" & year == 2009)

alc.inc.1999 <- data %>%
  filter(sex == "Both" & measure == "Incidence" & cause == "Alcohol use disorders" & year == 1999)
 


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

drinkage <- mutate_at(drinkage, vars(Group), as.factor)
##join partitioned drinkage with huge set
bigboy <- DrugDataSetFull %>%
  select(-18,-19) %>%
  left_join(drinkage, by = c("location" = "Country")) %>%
  select(-19)

boxplot(alc.inc.2019$val~alc.inc.2019$Religion, main="Global AUD for Countries by Religion (2019)", ylab="AUD per 100k", xlab='Religious Majority', col='cyan')
boxplot(alc.inc.2019$val~alc.inc.2019$Group, main="Global AUD for Countries by MLDA (2019)", ylab="AUD per 100k", xlab='Minimum Legal Drinking Age', col='cyan')

## these chi-square tests show that all these variables are NOT independent.
chisq.test(alc.inc.2019$Religion,alc.inc.2019$regime_row_owid)
chisq.test(alc.inc.2019$Religion,alc.inc.2019$civlibs_fh)
chisq.test(alc.inc.2019$regime_row_owid,alc.inc.2019$civlibs_fh)

# alc.inc.2019

model1 <- lm(val ~ Group, data = alc.inc.2019)
anova(model1)
summary(model1)

model2 <- lm(val ~ Religion, data = alc.inc.2019)
anova(model2)
summary(model2)

# once Religion is introduced, Total ban is no longer a significant predictor,
# presumably because Islam covers that
model3 <- lm(val ~ Group + Religion, data = alc.inc.2019)
anova(model3)
summary(model3)

# alc.inc.2019.2 (without factors, using only numeric)
alc.inc.2019.2 <- alc.inc.2019 %>% select(7,11:15)
colnames(alc.inc.2019.2)[colnames(alc.inc.2019.2)=="Unemployment, total (% of total labor force) (modeled ILO estimate)"] <- "Unemployment"
model4 <- lm(val ~ ., data = alc.inc.2019.2)
anova(model4)
summary(model4)

# this one does fairly well with r^2 of 0.3411. it only uses the liters consumed
# though which seems like a no-brainer
model5 <- lm(val ~ . - Unemployment - GDPPerCapita, data = alc.inc.2019.2)
anova(model5)
summary(model5)

# this one does worse than the one with no Religion or Group
model6 <- lm(val ~ Group + Religion + AvgBeerConsump.L + AvgWineConsump.L + AvgSpiritConsump.L, data = alc.inc.2019)
anova(model6)
summary(model6)

# model using everything has r^2 of 0.39
modelALL <- lm(val ~ Group + Religion + AvgBeerConsump.L + AvgWineConsump.L + AvgSpiritConsump.L + GDPPerCapita + Unemployment + regime_row_owid + civlibs_fh, data = alc.inc.2019)
anova(modelALL)
summary(modelALL)

# after some playing around, this model seems to have the highest adj. r^2 of 0.3254
# notably, unemployment isn't a significant predictor, but it highly increases r^2 (??)
model <- lm(val ~ Group + Religion + AvgBeerConsump.L + AvgWineConsump.L + AvgSpiritConsump.L + Unemployment, data = alc.inc.2019)
anova(model)
summary(model)


# ALCOHOL DEATHS dataframes
alc.death.2019 <- data %>%
  filter(sex == "Both" & measure == "Deaths" & cause == "Alcohol use disorders" & year == 2019) %>%
  select(-12,-17)


boxplot(alc.death.2019$val~alc.death.2019$Religion, main="Global AUD for Countries by Religion (2019)", ylab="AUD per 100k", xlab='Religious Majority', col='cyan')


# alc.death.2019

# this model using all to predict deaths has adj. r^2 only 0.1398
modelALL.2 <- lm(val ~ Group + Religion + AvgBeerConsump.L + AvgWineConsump.L + AvgSpiritConsump.L + GDPPerCapita + Unemployment + regime_row_owid + civlibs_fh, data = alc.death.2019)
anova(modelALL.2)
summary(modelALL.2)

# adj. r^2 .187
model.2 <- lm(val ~ Religion + AvgBeerConsump.L + AvgWineConsump.L + AvgSpiritConsump.L + Unemployment, data = alc.death.2019)
anova(model.2)
summary(model.2)

# spirits consumption ALONE accounts for 17.11% of variability in alc deaths (whoah)
modelSpirit <- lm(val ~ AvgSpiritConsump.L, data = alc.death.2019)
anova(modelSpirit)
summary(modelSpirit)



