### TidyTuesday: 2021_45_MakingMaps ###

## Setup ----------
library(tidyverse)
library(sf)
library(spData)
library(osmdata)
library(ggfx)

## Data ----------
# Data from spData package
bikes <- cycle_hire %>% 
  mutate(n = nbikes + nempty) %>% 
  st_transform(crs = 27700)

london_line <- lnd %>% 
  st_cast(to = "MULTILINESTRING") %>% 
  st_transform(crs = 27700)

london_centroids <- lnd %>% 
  st_transform(crs = 27700) %>% 
  st_point_on_surface() %>% 
  filter(NAME %in% c("Westminster", "City of London", "Tower Hamlets", "Lewisham", "Southwark", "Lambeth", "Wandsworth", "Hammersmith and Fulham", "Kensington and Chelsea", "Camden", "Islington", "Hackney")) %>% 
  mutate(NAME = as.character(NAME),
         NAME = case_when(NAME == "Hammersmith and Fulham" ~ "Hammersmith & Fulham",
                          NAME == "Kensington and Chelsea" ~ "Kensington & Chelsea",
                          TRUE ~ NAME))

# OpenStreetMap Data
# Make bbox
x <- c(-0.290741, 0.057115)
y <- c(51.401262, 51.573761)

ldn_bbox <- rbind(x,y) 
colnames(ldn_bbox) <- c("min", "max")

# get OSM data
big_roads <- ldn_bbox %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("primary", "secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_roads <- ldn_bbox %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

railways <- ldn_bbox %>%
  opq()%>%
  add_osm_feature(key = "railway", value = "rail") %>%
  osmdata_sf() 

bike_lanes <- ldn_bbox %>%
  opq() %>%
  add_osm_feature(key = "route", 
                  value = c("bicycle")) %>%
  osmdata_sf()

# tidy OSM data
big_roads_tidy <- big_roads$osm_lines %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) 

small_roads_tidy <- small_roads$osm_lines %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) 

railways_tidy <- railways$osm_lines %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) 

bike_lanes_tidy <- bike_lanes$osm_lines %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) 

## Plot ----------
bike_plot <- 
  ggplot() +
  with_shadow(geom_sf(data = london_line,
                      colour = "white",
                      size = 0.75),
              x_offset = 0,
              y_offset = 0,
              sigma = 15,
              colour = "grey30") +
  geom_sf(data = railways_tidy,
          colour = "grey92",
          size = 0.4) +
  geom_sf(data = small_roads_tidy,
          colour = "grey92",
          size = 0.4) +
  geom_sf(data = big_roads_tidy,
          colour = "grey92",
          size = 0.9) +
  with_outer_glow(geom_sf(data = bike_lanes_tidy,
                          colour = "#9FD6F0",
                          size = 0.4),
                  colour = "#9FD6F0",
                  expand = 2, 
                  sigma = 10) +
  geom_sf(data = bikes,
          aes(size = n),
          shape = 16,
          alpha = 0.4,
          colour = "#00AEEF") +
  geom_sf(data = london_line,
          colour = "white",
          size = 0.75) +
  geom_sf_text(data = london_centroids,
               aes(label =  NAME),
               family = "Roboto",
               size = 6,
               colour = "grey50",
               alpha = 0.9) +
  annotate("text", 
           x = 530500,
           y = 172400,
           label = "London Cycle Hire",
           size = 16,
           colour = "#00AEEF",
           family = "Bebas") +
  annotate("text", 
           x = 530500,
           y = 171900,
           label = "Bike hire locations & cycling routes in London",
           size = 5.15,
           colour = "grey30",
           family = "Roboto") +
  annotate("text", 
           x = 530500,
           y = 171150,
           label = "Visualisation: Joshua Copping | Data: GOV.UK & OpenStreetMap",
           size = 3.2,
           colour = "grey30",
           family = "Roboto") +
  coord_sf(expand = F,
           xlim = c(520000, 541000),
           ylim = c(171000, 186000)) +
  scale_size_continuous(range = c(1, 10),
                        name = "Number of hire bikes") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        plot.margin = margin(0, 0, 0, 0),
        legend.position = c(0.5, 0.035),
        legend.direction = "horizontal",
        legend.title = element_text(family = "Roboto",
                                    size = 14,
                                    colour = "grey15"),
        legend.text = element_text(family = "Roboto",
                                   size = 14,
                                   colour = "grey15"))

## Save ----------
path <- paste("2021_45_MakingMaps")
ggsave(here::here(path, glue::glue("{path}.png")),
       width = 16.8,
       height = 12,
       dpi = 300)

