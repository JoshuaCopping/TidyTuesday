### TidyTuesday: 2021_30_USDroughts ###

## Setup ----------
library(tidyverse)
library(lubridate)
library(geofacet)
library(ggtext)
library(ggfx)
library(patchwork)

## Data ----------
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

states_drought <- drought %>% 
  select(-map_date, -stat_fmt) %>% 
  mutate(year = year(valid_start)) %>% 
  filter(year == 2020) %>%
  mutate(drought_level = case_when(drought_lvl == "None" ~ "No drought",
                                   drought_lvl == "D0" ~ "Abnormally dry",
                                   drought_lvl == "D1" ~ "Moderate",
                                   drought_lvl == "D2" ~ "Severe",
                                   drought_lvl == "D3" ~ "Extreme",
                                   drought_lvl == "D4" ~ "Exceptional drought"),
         drought_level = factor(drought_level, levels = c("Exceptional drought", "Extreme", "Severe", "Moderate", "Abnormally dry", "No drought"))) %>% 
  filter(state_abb != "PR")

usa_drought <- states_drought %>% 
  group_by(valid_start, drought_level) %>%
  summarise(average = mean(area_pct)/100) 

shadow_points <- tibble(
  state_abb = unique(states_drought$state_abb),
  date = as.Date("2020-07-04"),
  pct = 50
)

label_legend <- tibble(
  drought_level = unique(usa_drought$drought_level),
  x = as.Date("2020-12-31"),
  y = c(0.97, 0.91, 0.84, 0.77, 0.63, 0.35)
)

## Plot ----------
# Inspired by the USGS Streamflow cartogram :https://github.com/USGS-VIZLAB/viz-scratch/tree/main/flow_cartogram

state_plot <- 
  ggplot(data = states_drought) +
  with_shadow(geom_point(data = shadow_points,
                         aes(x = date, y = pct),
                         shape = 15,
                         size = 27),
              x_offset = 0,
              y_offset = 0,
              sigma = 35,
              colour = "grey30") +
  geom_area(data = states_drought,
            aes(x = valid_start, y = area_pct, 
                fill = drought_level,
                colour = drought_level),
            size = 0.05) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B",
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#C8586C", "#DC7176", "#EE8A82",  "#F5BA98", "#FBE6C5", "grey80"))  +
  scale_colour_manual(values = c("#C8586C", "#DC7176", "#EE8A82",  "#F5BA98", "#FBE6C5", "grey80")) +
  coord_fixed(ratio = 3.5,
              clip = "off") +
  facet_geo(~state_abb) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(family = "Megan June",
                                  size = 16,
                                  colour = "grey25",
                                  margin = margin(0, 0, 2, 0)),
        plot.margin = margin(10, 30, 30, 10),
        panel.spacing.x = unit(0.3, "cm"),
        plot.background = element_rect(fill = "grey95",
                                       colour = "grey95"))

geofacet_grob <- get_geofacet_grob(state_plot)

usa_plot <- 
  ggplot() +
  geom_area(data = usa_drought,
            aes(x = valid_start, y = average, 
                fill = drought_level,
                colour = drought_level),
            size = 0.05) +
  geom_text(data = label_legend,
            aes(x = x, y = y,
                label = drought_level),
            hjust = 0,
            family = "Megan June",
            size = 7,
            colour = "grey25") +
  annotate("text", 
           x = as.Date("2020-07-01"), y = 1.05,
           label = "National Average",
           family = "Megan June",
           colour = "grey25",
           size = 7)+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b",
               limits = as.Date(c("2020-01-01", "2020-12-50")),
               expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0.025, 0.5, 0.975),
                     labels = c("0%", "50%", "100%"),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("#C8586C", "#DC7176", "#EE8A82",  "#F5BA98", "#FBE6C5", "grey80"))  +
  scale_colour_manual(values = c("#C8586C", "#DC7176", "#EE8A82",  "#F5BA98", "#FBE6C5", "grey80")) +
  coord_fixed(ratio = 350,
              clip = "off") +
  labs(title = "Drought Level\nIn 2020") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(family = "Megan June",
                                   size = 18,
                                   colour = "grey25",
                                   angle = 45,
                                   hjust = 1,
                                   margin = margin(0, 0, 0, 0)),
        axis.text.y = element_text(family = "Megan June",
                                   size = 18,
                                   colour = "grey25",
                                   margin = margin(r = 0)),
        plot.title = element_text(family = "Megan June",
                                  size = 60,
                                  colour = "grey10",
                                  lineheight = 0.8,
                                  hjust = 0.5,
                                  margin = margin(0, 0, 0, 0)),
        plot.margin = margin(50, 160, 120, 30)) 

drought_plot <- usa_plot + geofacet_grob  +
  plot_layout(ncol = 2, widths = c(1, 3)) +
  plot_annotation(caption = "Visualisation: Joshua Copping | Data: US Drought Monitor") &
  theme(plot.caption = element_text(hjust = 0.5,
                                    family = "Megan June",
                                    size = 14,
                                    colour = "grey40"),
        plot.background = element_rect(fill = "grey95",
                                       colour = "grey95"))

## Save ----------
path <- paste("2021_30_USDroughts")
ggsave(here::here(path, glue::glue("{path}.png")),
       width = 18,
       height = 10,
       dpi = 300)
