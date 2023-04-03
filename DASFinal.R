library(tidyverse)
library(openintro)
library(plotly)
library("rnaturalearth")
library("rnaturalearthdata")
library(readxl)
library(shiny)
library(readr)


DrugDataSetFull <- read_csv("IHME-GBD_2019_DATA-671c0649-1.csv")

DrugDataSetFull <- DrugDataSetFull %>%
  arrange(desc(measure), location, year, cause)

world <- ne_countries(scale = "medium", returnclass = "sf")

DrugDataSetFull <- left_join(world, DrugDataSetFull, by = c("admin" = "location")) %>%
  select(measure, admin, iso_a3, year, sex, age, cause, val, geometry)

colnames(DrugDataSetFull)[8] = "estimate"

DrugDataSetFull <- DrugDataSetFull %>%
  sort(admin, year, age, cause)

unique(DrugDataSetFull$location)
unique(world$name_long)
unique(world$admin)
