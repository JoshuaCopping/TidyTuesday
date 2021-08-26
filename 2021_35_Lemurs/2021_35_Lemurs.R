### TidyTuesday: 2021_35_Lemurs ###

## Setup ----------
library(tidyverse)
library(ggtext)

## Data ----------
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')
taxonomy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/taxonomy.csv')

lemurs <- lemurs %>% 
  mutate(taxon = fct_recode(taxon, "CMEAD" = "CMED")) %>% 
  left_join(taxonomy, by="taxon") %>% 
  select(dlc_id, taxon, common_name, age_at_death_y, sex) %>% 
  mutate(common_name = str_remove(common_name, " lemur")) %>% 
  distinct() %>% 
  filter(sex != "ND") %>%
  drop_na(age_at_death_y) %>%
  group_by(common_name) %>% 
  mutate(mean_death = mean(age_at_death_y),
         min_death = min(age_at_death_y),
         min_death = replace(min_death, row_number() != 1, NA),
         max_death = max(age_at_death_y),
         max_death = replace(max_death, row_number() != 1, NA))

## Plot ----------
ggplot(lemurs,
       aes(age_at_death_y,reorder(common_name, (mean_death)))) +
  geom_linerange(aes(xmin = min_death,xmax = max_death),
                 alpha = 0.5,
                 size = 1,
                 colour = "grey70") +
  geom_point(aes(colour = sex),
             size = 12,
             alpha = 0.3,
             position = position_jitter(height = 0.15)) +
  geom_point(aes(mean_death, reorder(common_name, (mean_death))),
             size = 32, 
             colour = "white") +
  geom_point(aes(mean_death, reorder(common_name, (mean_death))),
             size = 30, 
             colour = "#009476") +
  geom_text(aes(mean_death, reorder(common_name, (mean_death)), 
                label = sprintf("%0.1f", round(mean_death, digits = 1))),
            size = 12,
            colour = "white",
            family = "Louis George Cafe") +
  scale_x_continuous(expand = c(0.02, 0.02)) +
  scale_colour_manual(values = c("#AD8CAE", "#4F93B8")) +
  labs(title = "The Life of Lemurs",
       subtitle = "The lifespan of <b style='color: #4F93B8'>male</b> and <b style='color: #AD8CAE'>female</b> lemurs, and the <b style='color: #009476'>average</b> per species",
       caption = "Visualisation: Joshua Copping &bull; Data: Duke Lemur Center &bull; #TidyTuesday Week 35 2021",
       x = "Lifespan in Years") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_line(colour = "grey30",
                                   size = 0.5),
        axis.text = element_text(family = "Louis George Cafe",
                                 colour = "grey30",
                                 size = 30,
                                 face = "bold"),
        axis.title.x = element_text(family = "Louis George Cafe",
                                    colour = "grey30",
                                    size = 42,
                                    face = "bold"),
        plot.title = element_text(family = "Louis George Cafe Bold",
                                  hjust = 0.5,
                                  size = 78,
                                  colour = "grey30",
                                  margin = margin(20, 0, 20, 0)),
        plot.subtitle = element_markdown(family = "Louis George Cafe",
                                         hjust = 0.5,
                                         colour = "grey30",
                                         face = "bold",
                                         size = 54,
                                         margin = margin(0, 10, 20, 10)),
        plot.caption = element_markdown(hjust = 0.5,
                                        family = "Poppins",
                                        colour = "grey30",
                                        size = 28,
                                        margin = margin(25, 0, 0, 0)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "#F4F4F2"),
        panel.background = element_rect(fill = "#F4F4F2"),
        plot.margin = margin(10, 50, 5, 50))

## Save ----------
path <- paste("2021_35_Lemurs")
ggsave(here::here(path, glue::glue("{path}.png")),
       width = 25,
       height = 30,
       dpi = 300)
