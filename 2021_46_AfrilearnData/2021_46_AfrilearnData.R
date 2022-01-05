### TidyTuesday: 2021_46_AfrilearnData ###

## Setup ----------
library(tidyverse)
library(afrilearndata)
library(sf)
library(terra)
library(paletteer)
library(ggfx)
library(patchwork)
library(ggtext)

## Data ----------
# large airports
airport_l <- afriairports %>% 
  filter(type == "large_airport") %>% 
  st_intersection(africontinent)

ap_l_sp <- as(airport_l, "Spatial")
ac_sp <- as(africontinent, "Spatial")

ap_l_v <- vect(ap_l_sp)
ac_v <- vect(ac_sp)

vor <- voronoi(ap_l_v, ac_v)
vor <- crop(vor, ac_v)

vor_ap_l <- st_as_sf(vor) %>% 
  mutate(id = sample(40))

# medium 
airport_m <- afriairports %>% 
  filter(type == "medium_airport") %>% 
  st_intersection(africontinent)

ap_m_sp <- as(airport_m, "Spatial")

ap_m_v <- vect(ap_m_sp)

vor <- voronoi(ap_m_v, ac_v)
vor <- crop(vor, ac_v)

vor_ap_m <- st_as_sf(vor) %>% 
  mutate(id = sample(397))

# small 
airport_s <- afriairports %>% 
  filter(type == "small_airport") %>% 
  st_intersection(africontinent)

ap_s_sp <- as(airport_s, "Spatial")

ap_s_v <- vect(ap_s_sp)

vor <- voronoi(ap_s_v, ac_v)
vor <- crop(vor, ac_v)

vor_ap_s <- st_as_sf(vor) %>% 
  mutate(id = sample(2722))

# all airports 
airport_all <- afriairports %>% 
  st_intersection(africontinent)

ap_sp <- as(airport_all, "Spatial")
ap_v <- vect(ap_sp)

vor <- voronoi(ap_v, ac_v)
vor <- crop(vor, ac_v)

vor_ap <- st_as_sf(vor) %>% 
  st_join(airport_all)

## Plot ----------
# small airports plot 
ap_s_plot <- 
  ggplot() +
  geom_sf(data = vor_ap_s,
          fill = "#0E8C7C",
          colour = "grey90",
          size = 0.2) +
  geom_sf(data = airport_s,
          colour = "white",
          shape = 16,
          size = 0.2,
          alpha = 0.7) +
  coord_sf(crs = 3857) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey25",
                                       colour = "grey25"),
        panel.background = element_rect(fill = "grey25",
                                        colour = "grey25"),
        plot.margin = margin(0, 0, 0, 0)) 

# medium airports plot 
ap_m_plot <- 
  ggplot() +
  geom_sf(data = vor_ap_m,
          fill = "#D9B016",
          colour = "grey90",
          size = 0.2) +
  geom_sf(data = airport_m,
          colour = "white",
          shape = 16,
          size = 0.5,
          alpha = 0.85) +
  coord_sf(crs = 3857) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey25",
                                       colour = "grey25"),
        panel.background = element_rect(fill = "grey25",
                                        colour = "grey25"),
        plot.margin = margin(0, 0, 0, 0)) 

# large airports plot 
ap_l_plot <- 
  ggplot() +
  geom_sf(data = vor_ap_l,
          fill = "#CF34C9",
          colour = "grey90",
          size = 0.2) +
  geom_sf(data = airport_l,
          colour = "white",
          shape = 16,
          size = 2,
          alpha = 0.85) +
  coord_sf(crs = 3857) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey25",
                                       colour = "grey25"),
        panel.background = element_rect(fill = "grey25",
                                        colour = "grey25"),
        plot.margin = margin(0, 0, 0, 0)) 

# all airoprts plot 
ap_all_plot <- 
  ggplot() +
  with_shadow(geom_sf(data = vor_ap,
                      aes(fill = type.x),
                      colour = "grey80",
                      size = 0.15),
              x_offset = 0,
              y_offset = 0,
              colour = "black",
              sigma = 40) +
  geom_sf(data = airport_all,
          aes(size = type),
          colour = "white",
          fill = "white",
          shape = 16,
          alpha = 0.8) +
  geom_sf(data = africountries %>% st_cast("MULTILINESTRING"),
          colour = "white",
          fill = NA,
          alpha = 0.85) +
  coord_sf(crs = 3857) +
  scale_fill_manual(values = c("#CF34C9", "#D9B016", "#0E8C7C")) +
  scale_size_manual(values = c(2, 1, 0.5)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey25",
                                       colour = "grey25"),
        panel.background = element_rect(fill = "grey25",
                                        colour = "grey25"),
        plot.margin = margin(0, 0, 0, 0)) 

# all plots combined 
airports_plot <- 
  ap_all_plot * (ap_s_plot/ap_m_plot/ap_l_plot) +
  plot_annotation(title = "African Airports",
                  subtitle = "Voronoi Polygons showing the catchment of <b><span style ='color: #0E8C7C'>small</span></b>, <b><span style ='color: #D9B016'>medium</span></b> & <b><span style ='color: #CF34C9'>large</span></b> airports throughout Africa",
                  caption = "Visualisation: Joshua Copping | Data: afrimapr - github.com/afrimapr") +
  plot_layout(widths = c(9, 3)) & 
  theme(plot.background = element_rect(fill = "grey25",
                                       colour = "grey25"),
        panel.background = element_rect(fill = "grey25",
                                        colour = "grey25"),
        plot.margin = margin(10, 20, 10, 20),
        plot.title = element_text(hjust = 0.5, 
                                  family = "Louis George Cafe",
                                  size = 56,
                                  colour = "white",
                                  margin = margin(20, 0, 0, 0)),
        plot.subtitle = element_markdown(hjust = 0.5,
                                         family = "Louis George Cafe",
                                         size = 20,
                                         colour = "white",
                                         margin = margin(5, 0, 20, 0)),
        plot.caption = element_text(hjust = 0.5,
                                    family = "Roboto",
                                    colour = "grey80",
                                    margin = margin(5, 0, 0, 0)))

## Save ----------
path <- paste("2021_46_AfrilearnData")
ggsave(plot = airports_plot,
       here::here(path, glue::glue("{path}.png")),
       width = 16,
       height = 12,
       dpi = 300)
