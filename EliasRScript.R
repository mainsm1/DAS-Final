library(tidyverse)
library(plotly)
library("rnaturalearth")
library("rnaturalearthdata")
library(readr)
library(stringr)

data <- read_csv("Religion and DrinkAge with ISO3.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")
WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

mapdata <- data %>% left_join(world, by = c("ISO3" = "iso_a3"))

p <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=geometry),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  geom_map(data = mapdata, map=WorldData,
           aes(fill=DrinkingAge, map_id=geometry),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Title", x="", y="") +
  theme_bw()
p

mapdata %>% ggplot() +
  geom_sf(aes(fill=DrinkingAge))



HYPOTHESIS TESTING BY SEX!!!!
  
ALCOHOL INCIDENCE
females <- data %>% filter(sex == "Female" & measure == "Incidence" & cause == "Alcohol use disorders")
males <- data %>% filter(sex == "Male" & measure == "Incidence" & cause == "Alcohol use disorders")
t.test(females$val,males$val)

ALCOHOL DEATHS
females <- data %>% filter(sex == "Female" & measure == "Deaths" & cause == "Alcohol use disorders")
males <- data %>% filter(sex == "Male" & measure == "Deaths" & cause == "Alcohol use disorders")
t.test(females$val,males$val)

CANNABIS INCIDENCE
females <- data %>% filter(sex == "Female" & measure == "Incidence" & cause == "Cannabis use disorders")
males <- data %>% filter(sex == "Male" & measure == "Incidence" & cause == "Cannabis use disorders")
t.test(females$val,males$val)

CANNABIS DEATHS
females <- data %>% filter(sex == "Female" & measure == "Deaths" & cause == "Cannabis use disorders")
males <- data %>% filter(sex == "Male" & measure == "Deaths" & cause == "Cannabis use disorders")
t.test(females$val,males$val)
