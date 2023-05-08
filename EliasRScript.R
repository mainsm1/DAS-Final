library(tidyverse)
library(readr)
library(stringr)



# HYPOTHESIS TESTING BY SEX (for 2019)!!!
data <- DrugFullDataset %>% 
  filter(year == 2019) %>%
  distinct()
  
# ALCOHOL INCIDENCE
females <- data %>% filter(sex == "Female" & measure == "Incidence" & cause == "Alcohol use disorders")
males <- data %>% filter(sex == "Male" & measure == "Incidence" & cause == "Alcohol use disorders")
t.test(females$val,males$val)

# ALCOHOL DEATHS
females <- data %>% filter(sex == "Female" & measure == "Deaths" & cause == "Alcohol use disorders")
males <- data %>% filter(sex == "Male" & measure == "Deaths" & cause == "Alcohol use disorders")
t.test(females$val,males$val)

# CANNABIS INCIDENCE
females <- data %>% filter(sex == "Female" & measure == "Incidence" & cause == "Cannabis use disorders")
males <- data %>% filter(sex == "Male" & measure == "Incidence" & cause == "Cannabis use disorders")
t.test(females$val,males$val)

# CANNABIS DEATHS 
## There are none in the data!

################################################################################

# ALCOHOL INCIDENCE MODEL 

alc.inc.2019 <- data %>%
  filter(sex == "Both" & measure == "Incidence" & cause == "Alcohol use disorders")

model <- glm(data = alc.inc.2019, val ~ `GDP Measure`)
summary(model)  
# Conclusion: no significant relationship between GDP and alcohol incidence (2019)

thing <- alc.inc.2019 %>%
  left_join(LegalDrinkingAge, by = c("location" = "Country"))

model1 <- glm(data = thing, val ~ DrinkingAge.y)
summary(model1)  
# Conclusion: yes significant relationship between drinkage and alcohol incidence (2019)


################################################################################

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



