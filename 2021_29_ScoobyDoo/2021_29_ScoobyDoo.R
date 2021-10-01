### TidyTuesday: 2021_29_ScoobyDoo ###

## Setup ----------
library(tidyverse)
library(lubridate)
library(ggstream)
library(ggtext)
library(png)
library(cowplot)

## Data ----------
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

path <- paste("2021_29_ScoobyDoo")
scooby_logo <- readPNG(here::here(path, "images", "ScoobyLogo.png"), native = TRUE)
scooby_characters <- readPNG(here::here(path, "images", "ScoobyCharacters.png"), native = TRUE)

scooby_tidy <- scoobydoo %>% 
  select(date_aired, caught_fred:caught_scooby, unmask_fred:snack_scooby) %>% 
  mutate(across(c(caught_fred:snack_scooby), ~ case_when(. == "TRUE" ~ 1, 
                                                         . == "FALSE" ~ 0)),
         year = year(date_aired)) %>% 
  pivot_longer(caught_fred:snack_scooby,
               names_to = "character",
               values_to = "total") %>% 
  mutate(activity = case_when(str_detect(character, "caught") ~ "caught",
                              str_detect(character, "unmask") ~ "unmasked",
                              str_detect(character, "snack") ~ "snack"),
         activity = factor(activity, levels = c("caught", "unmasked", "snack")),
         character = str_remove_all(character, "caught_"),
         character = str_remove_all(character, "unmask_"),
         character = str_remove_all(character, "snack_")) %>% 
  select(-date_aired) %>% 
  group_by(year, character, activity) %>%
  summarise(total = sum(total))


labels <- tribble(
  ~year, ~total, ~activity, ~label,
  1958.5, 6.5, "caught", "Catching\nVillains",
  1958.5, 6, "unmasked", "Unmasking\nVillains",
  1958.5, 2.5, "snack", "Snacking"
) %>% 
  mutate(activity = factor(activity, levels = c("caught", "unmasked", "snack")))

text <- tribble(
  ~year, ~total, ~character, ~activity, ~text,
  1994, 9, "shaggy", "snack", "**Shaggy** snacked a lot and didn't do much else.", 
  2019, 15, "fred", "unmasked", "**Fred** carried out most of the work, catching and unmasking villains.", 
  1985, -12, "daphnie", "unmasked", "**Daphne** unmasked villains early in the series, but never caught many.", 
  2017, -17.5, "velma", "unmasked", "**Velma** unmasked villains, but didn’t start catching villains until the early 2000’s.", 
  1971, -19, "scooby", "caught", "**Scooby** caught villains but didn’t unmask many. Did the lack of opposable thumbs stop him?", 
) %>% mutate(activity = factor(activity, levels = c("caught", "unmasked", "snack")))

colours <- c("#6C007A", "#149992", "#8E6345", "#79AF30", "#D66B00")

## Plot ----------
scooby_plot <- ggplot(scooby_tidy, 
         aes(year, total, 
             colour = character,
             fill = character)) +
  geom_vline(data = tibble(x =seq(1970, 2020, by = 10)),
             aes(xintercept = x),
             color = "grey25", 
             size = 0.5,
             linetype = "dotted") +
  geom_stream(geom = "contour",
              size = 2,
              bw = 0.5,
              extra_span = 0.2,
              true_range = "none",
              colour = "grey25") +
  geom_stream(geom = "polygon",
              size = 1,
              bw = 0.5,
              extra_span = 0.2,
              true_range = "none") +
  facet_grid(activity ~ .,
             scales = "free") +
  geom_text(data = labels,
            aes(year, total, 
                label = label),
            inherit.aes = FALSE,
            family = "Roboto",
            fontface = "bold",
            hjust = 0,
            size = 7,
            color = "grey25",
            lineheight = 0.8) +
  geom_textbox(data = text, 
               aes(label = text, 
                   colour = character),
               fill = "white",
               size = 3.8,
               hjust = 0) +
  scale_fill_manual(values = colours) +
  scale_colour_manual(values = colours) +
  scale_x_continuous(position = "top",
                     breaks = seq(1970, 2020, by = 10)) +
  scale_y_continuous(expand = c(0.25, 0.25)) +
  coord_cartesian(clip = "off") +
  labs(title = "What Do the Characters in                          Spend Their Time Doing?",
       caption = "Visualisation: Joshua Copping | Data: Kaggle | Logo & Characters: Cartoon Network") +
  theme(panel.spacing = unit(0, "lines")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey90", 
                                       colour = NA),
        panel.background = element_rect(fill = NA, 
                                        colour = NA),
        legend.position = "none",
        axis.text.x = element_text(family = "Roboto",
                                   face = "bold",
                                   colour = "grey25",
                                   size = 16),
        strip.text = element_blank(),
        plot.margin = margin(10, 45, 0, 30),
        plot.caption = element_text(margin = margin(20, 0, 5, 0),
                                        hjust = 0.5,
                                        family = "Roboto",
                                        colour = "grey25",
                                        size = 10),
        plot.caption.position = "plot",
        plot.title = element_text(family = "Roboto Black",
                                  size = 28,
                                  hjust = 0.2,
                                  colour = "grey25",
                                  margin = margin(35, 0, 35, 0))) 

scooby_plot <- ggdraw(scooby_plot) +
  draw_image(scooby_logo,
             x = 0.36, y = 0.44,
             width = 0.15) +
  draw_image(scooby_characters,
             x = 0.8, y = 0.4,
             width = 0.18) +
  annotate("rect", 
           xmin = 0.8, xmax = 0.8356, 
           ymin = 0.795, ymax = 0.812,
           fill = "#149992") +
  annotate("rect", 
           xmin = 0.8356, xmax = 0.8712, 
           ymin = 0.795, ymax = 0.812,
           fill = "#D66B00") +
  annotate("rect", 
           xmin = 0.8712, xmax = 0.9068, 
           ymin = 0.795, ymax = 0.812,
           fill = "#8E6345") +
  annotate("rect", 
           xmin = 0.9068, xmax = 0.9424, 
           ymin = 0.795, ymax = 0.812,
           fill = "#79AF30") +
  annotate("rect", 
           xmin = 0.9424, xmax = 0.978, 
           ymin = 0.795, ymax = 0.812,
           fill = "#6C007A") +
  annotate("text", 
           x = 0.817, y = 0.804, 
           label = "Fred",
           family = "Roboto",
           colour = "white") +
  annotate("text", 
           x = 0.8534, y = 0.804, 
           label = "Velma",
           family = "Roboto",
           colour = "white") +
  annotate("text", 
           x = 0.889, y = 0.804, 
           label = "Scooby",
           family = "Roboto",
           colour = "white") +
  annotate("text", 
           x = 0.9246, y = 0.804, 
           label = "Shaggy",
           family = "Roboto",
           colour = "white") +
  annotate("text", 
           x = 0.9602, y = 0.804, 
           label = "Daphnie",
           family = "Roboto",
           colour = "white") 

## Save ----------
ggsave(plot = scooby_plot,
       here::here(path, glue::glue("{path}.png")),
       width = 16,
       height = 12,
       dpi = 300)
