### TidyTuesday: 2021_30_USDroughts ###

## Setup ----------
library(tidyverse)
library(ggtext)

## Data ----------
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

drought <- drought %>% 
  select(-map_date, -stat_fmt) %>%
  filter(state_abb == "CA") %>% 
  mutate(drought_level = case_when(drought_lvl == "None" ~ "No drought",
                                   drought_lvl == "D0" ~ "Abnormally dry",
                                   drought_lvl == "D1" ~ "Moderate",
                                   drought_lvl == "D2" ~ "Severe",
                                   drought_lvl == "D3" ~ "Extreme",
                                   drought_lvl == "D4" ~ "Exceptional drought"),
         drought_level = factor(drought_level, levels = c("Exceptional drought", "Extreme", "Severe", "Moderate", "Abnormally dry", "No drought"))) %>% 
  group_by(valid_start) %>%
  slice_max(area_pct, n = 1, with_ties = FALSE) 

## Plot ----------
ggplot(drought, 
       aes(valid_start, 
           fill = reorder(drought_level, desc(drought_level)))) +
  geom_bar(width = 10) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y",
               minor_breaks = "1 month",
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("grey90", "#FBE6C5", "#F5BA98", "#EE8A82", "#DC7176", "#C8586C")) +
  ggprism::annotation_ticks(sides = "b",
                            type = "minor",
                            outside = TRUE,
                            lineend = "square",
                            size = 0.25,
                            colour = "grey40") +
  coord_cartesian(clip = "off") +
  labs(title = "PERIODS OF DROUGHT IN CALIFORNIA",
       subtitle = "<b style='font-size: 48px'>THE PREDOMINANT DROUGHT CLASSIFICATION PER WEEK, BETWEEN JULY  2001 & JULY  2021</b> <br>
       <b style='color: #C8586C'>EXCEPTIONAL DROUGHT</b> <b style='color: grey30'>&bull;</b> <b style='color: #DC7176'>EXTREME</b> <b style='color: grey30'>&bull;</b> <b style='color: #EE8A82'>SEVERE</b> <b style='color: grey30'>&bull;</b> <b style='color: #F5BA98'>MODERATE</b> <b style='color: grey30'>&bull;</b> <b style='color: #FBE6C5'>ABNORMALLY DRY</b> <b style='color: grey30'>&bull;</b> <b style='color: grey85'>NO DROUGHT</b> ",
       caption = "Visualisation: Joshua Copping | Data: US Drought Monitor") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   family = "Roboto",
                                   face = "bold",
                                   vjust = 0.5,
                                   size = 14),
        plot.title = element_text(size = 64,
                                  family = "SF Movie Poster",
                                  face = "bold",
                                  hjust = 0.5,
                                  colour = "grey30",
                                  margin = margin(10, 10, 20, 10)),
        plot.subtitle = element_markdown(hjust = 0.5,
                                         family = "SF Movie Poster",
                                         size = 32,
                                         colour = "grey30",
                                         margin = margin(0, 0, 10, 0)),
        plot.caption = element_text(hjust = 0.5,
                                    family = "Roboto Medium",
                                    size = 10,
                                    colour = "grey30",
                                    margin = margin(25, 0, 0, 0)),
        plot.margin = margin(10, 30, 2, 30))

## Save ----------
path <- paste("2021_30_USDroughts")
ggsave(here::here(path, glue::glue("{path}.png")),
       width = 16,
       height = 6,
       dpi = 300)

