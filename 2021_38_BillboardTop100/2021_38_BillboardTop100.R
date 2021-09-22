### TidyTuesday: 2021_38_BillboardTop100 ###

## Setup ----------
library(tidyverse)
library(lubridate)
library(ggridges)

## Data ----------
billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
audio <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

music <- left_join(billboard, audio, by = c('song_id', 'song', 'performer')) %>% 
  mutate(date = mdy(week_id),
         year = as.factor(year(date))) %>% 
  select(song_id, tempo, year) %>% 
  drop_na()

## Plot ----------
ggplot(music, 
       aes(tempo, year)) + 
  geom_density_ridges(scale = 8, 
                      colour = "#FFFFFF", 
                      fill = "#000000") +
  coord_cartesian(clip = "off") +
  labs(title = "SONG TEMPO",
       x = "LEFT: LOW TEMPO | RIGHT: HIGH TEMPO | BOTTOM: 1970 | TOP: 2021",
       caption = "VISUALISATION: JOSHUA COPPING | DATA: DATA.WORLD") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#000000"),
        plot.margin = margin(80,160,120,160),
        text = element_text(family = "Helvetica Light",
                            colour = "#FFFFFF"),
        plot.title = element_text(size = 50,
                                  hjust = 0.5),
        axis.title.x = element_text(size = 11,
                                    hjust = 0.5,
                                    margin = margin(10, 0, 0, 0)),
        plot.caption = element_text(size = 11,
                                    hjust = 0.5))

## Save ----------
path <- paste("2021_38_BillboardTop100")
ggsave(here::here(path, glue::glue("{path}.png")),
       width = 10,
       height = 10,
       dpi = 300)
