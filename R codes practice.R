install.packages("dplyr")
#Package used for data manipulation that provides a consistent set of verbs, 
# making it easier to solve common data manipulation problems like selecting, filtering, summarizing, and arranging data
install.packages("tidyr")
# help reshape data by converting it from a "wide" format to a "long" format or vice versa, making it easier to work with for analysis.
install.packages("broom")

install.packages("knitr")
#Package used to generate reports in R. It allows users to create dynamic and reproducible reports by combining code 
# and text in the same document.

install.packages("aod")

#setwd("/Users/angekakpo/Downloads") To set working directory

library(dplyr)
library(tidyr)
library(broom)
library(knitr)
library(aod)

# Load the mtcars dataset
data(mtcars)

#You could also use
read.csv("path to dataset on your dataset")

# Generate descriptive statistics
summary_df <- mtcars %>% 
  summarize_all(list(mean = mean, sd = sd, min = min, median = median, max = max)) %>% 
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value")

# Pivot the table to have variables in rows
summary_df <- summary_df %>% 
  separate(Statistic, c("Variable", "Statistic"), sep = "_", extra = "merge") %>% 
  pivot_wider(names_from = Statistic, values_from = Value)

# Output the descriptive statistics in a nice table
kable(summary_df, format = "markdown")


# Run linear regression between mpg and other variables
simpleLM <- lm(mpg ~ am + carb + cyl, data=mtcars)
summary(simpleLM)

#Run a probit regression between vs and other variables
myprobit <- glm(vs ~ am + carb + cyl, family = binomial(link = "probit"), 
                data = mtcars)

summary(myprobit)

