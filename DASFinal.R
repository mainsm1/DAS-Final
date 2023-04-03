library(tidyverse)
library(openintro)
library(plotly)
library("rnaturalearth")
library("rnaturalearthdata")
library(readxl)
library(shiny)
library(readr)


DrugDataSetFull <- read_csv("IHME-GBD_2019_DATA-671c0649-1.csv")
Iso_code_countries <- read_excel("Copy of IHME_GBD_2019_AIR_POLLUTION_1990_2019_ISO3_CODES_Y2021M10D08.xlsx")

DrugDataSetFull <- DrugDataSetFull %>%
  arrange(desc(measure), location, year, age, cause)

world <- ne_countries(scale = "medium", returnclass = "sf")

DrugDataSetFull <- left_join(DrugDataSetFull, Iso_code_countries, by=c('location' = 'location_name'))

DrugDataSetFull <- left_join(world, DrugDataFull, by = c("iso_a3" = "ISO3")) %>%
  select(measure, admin, iso_a3, year, sex, age, cause, val, geometry)

colnames(DrugDataSetFull)[8] = "estimate"

DrugDataSetFull <- DrugDataSetFull %>%
  arrange(admin, year, desc(age), cause)

