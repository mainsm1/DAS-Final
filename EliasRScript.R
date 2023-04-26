library(tidyverse)
library(plotly)
library("rnaturalearth")
library("rnaturalearthdata")
library(readr)

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




