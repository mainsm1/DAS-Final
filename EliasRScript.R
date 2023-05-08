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

p <- DrugFullDataset %>%
  