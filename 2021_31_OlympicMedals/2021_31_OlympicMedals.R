### TidyTuesday: 2021_31_OlympicMedals ###

## Setup ----------
library(tidyverse)
library(patchwork)
library(cowplot)
library(ggtext)
library(png)

## Data ----------
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

path <- paste("2021_31_OlympicMedals")
logo <- readPNG(here::here(path, "logo", "TeamGB_logo.png"))

gb_olympics <- olympics %>%
  filter(noc == "GBR") %>% 
  drop_na(medal) %>% 
  mutate(number = 1,
         position = case_when(medal == "Gold" ~ 1,
                              medal == "Silver" ~ 2,
                              medal == "Bronze" ~ 3)) %>%
  group_by(name) %>%
  mutate(total = sum(number)) %>% 
  arrange(name, desc(position)) %>%
  mutate(order = seq_along(name)) %>% 
  arrange(desc(total)) %>%
  filter(total >= 5) %>% 
  mutate(colour = case_when(medal == "Gold" ~ "#FFD700",
                            medal == "Silver" ~ "#D6D6D6",
                            medal == "Bronze" ~ "#977547"),
         name = recode(name, "Bradley Marc Wiggins" = "Bradley Wiggins",
                       "Jason Francis Kenny" = "Jason Kenny",
                       "Christopher Andrew \"Chris\" Hoy"  = "Chris Hoy",
                       "Stephen Geoffrey \"Steven\" Redgrave" = "Steve Redgrave",
                       "Max Antony Whitlock" = "Max Whitlock",
                       "Charles Benedict \"Ben\" Ainslie" = "Ben Ainslie",
                       "Katherine Jane Grainger" = "Katherine Grainger",
                       "Kathleen \"Kitty\" McKane (-Godfree)" = "Kitty Godfree"),
         y_position = case_when(name == "Bradley Wiggins" ~ 11,
                                name == "Henry Taylor" ~ 10,
                                name == "Chris Hoy" ~ 9,
                                name == "Jason Kenny" ~ 8,
                                name == "Steve Redgrave" ~ 7,
                                name == "John Arthur Jarvis" ~ 6,
                                name == "Ben Ainslie" ~ 5,
                                name == "Jack Beresford" ~ 4,
                                name == "Max Whitlock" ~ 3,
                                name == "Katherine Grainger" ~ 2,
                                name == "Kitty Godfree" ~ 1),
         label = case_when(name == "Bradley Wiggins" ~ "Cycling",
                           name == "Henry Taylor" ~ "Swimming",
                           name == "Chris Hoy" ~ sport,
                           name == "Jason Kenny" ~ sport,
                           name == "Steve Redgrave" ~ sport,
                           name == "John Arthur Jarvis" ~ "Swimming & Water Polo",
                           name == "Ben Ainslie" ~ sport,
                           name == "Jack Beresford" ~ sport,
                           name == "Max Whitlock" ~ sport,
                           name == "Katherine Grainger" ~ sport,
                           name == "Kitty Godfree" ~ sport))

sports <- olympics %>% 
  filter(noc == "GBR") %>% 
  drop_na(medal) %>% 
  group_by(sport, medal) %>%
  count(name = "n_medals") %>%
  ungroup() %>% 
  pivot_wider(names_from = medal, 
              values_from = n_medals) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Total = rowSums(across(Gold:Silver)),
         Total2 = Total) %>% 
  arrange(desc(Total)) %>% 
  top_n(n = 5) %>% 
  pivot_longer(Gold:Total,
               names_to = "medal",
               values_to = "count") %>% 
  mutate(position = case_when(medal == "Total" ~ 4,
                              medal == "Gold" ~ 3,
                              medal == "Silver" ~ 2,
                              medal == "Bronze" ~ 1),
         colour = case_when(medal == "Total" ~ "#005288",
                            medal == "Gold" ~ "#FFD700",
                            medal == "Silver" ~ "#D6D6D6",
                            medal == "Bronze" ~ "#977547")) 

## Plot ----------
athletes <- 
  ggplot(data = gb_olympics,
         aes(x = order, y = reorder(name,(y_position)), 
             fill = colour,
             colour = colour)) +
  geom_point(shape = 21, 
             size = 7.5, 
             stroke = 1, 
             alpha = 0.5) +
  geom_text(aes(label = position,
                colour = colour),
            alpha = 0.8) +
  geom_text(aes(x = 0, y = name, 
                label = name),
            vjust = -0.25,
            hjust = 1,
            family = "Roboto black",
            colour = "#005288") +
  geom_text(aes(x = 0, y = name, 
                label = label),
            vjust = 1.25,
            hjust = 1,
            family = "Roboto light",
            fontface = "italic",
            colour = "#005288") +
  geom_richtext(aes(x = 10.7, y = 8), 
                label = 
                  "The number of <b style = 'color: #FFD700'>gold</b>, <b style = 'color: #D6D6D6'>silver</b>, and <b style = 'color: #977547'>bronze</b><br>
                  medals, Team GB won in the modern<br>
                  Olympic games between 1896 and 2016.<br> 
                  Showing Olympians with five or more<br>
                  medals and the five sports that <br>
                  Team GB has won the most medals in.",
                label.colour = NA,
                fill = NA,
                family = "Roboto",
                colour = "grey30",
                hjust = 0) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(limits = c(0, 22),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Great Britain's Olympic Medals",
       caption = "Visualisation: Joshua Copping &bull; Data: Kaggle") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(colour = "#005288",
                                  size = 24,
                                  family = "Roboto Black",
                                  hjust = -1.8,
                                  margin = margin(20,30,20,30)),
        plot.caption = element_markdown(colour = "grey30",
                                        hjust = 1,
                                        margin = margin(10, 0, 5, 0)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.margin = margin(0, 30, 0, 140)) 

medals <- 
  ggplot(sports,
         aes(x = reorder(sport,desc(Total2)), y = position)) + 
  geom_point(aes(colour = colour,
                 fill = colour,
                 size = count),
             shape = 1,
             stroke = 2) +
  geom_text(aes(label = count,
                colour = colour),
            fontface = "bold") +
  scale_shape_identity() +
  scale_fill_identity() +
  scale_colour_identity() + 
  scale_alpha_identity() +
  scale_size(range = c(7,18)) +
  scale_y_continuous(limits = c(0.5, 4.5),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  annotate("segment",
           x = 0.5, xend = 5.5, 
           y = 3.45, yend = 3.45,
           colour = "grey30") +
  annotate("segment",
           x = 0.5, xend = 5.5, 
           y = 0.55, yend = 0.55,
           colour = "grey30") +
  theme_void() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Roboto Black",
                                   colour = "#005288",
                                   size = 10,
                                   margin = margin(5, 0, 10, 0)))

final <- 
  athletes + 
  inset_element(medals, 0.48, 0.005, 1.02, 0.55) +  
  draw_image(logo, 
             x = 2.8, 
             y = 7.4, 
             scale = 2.5) 

## Save ----------
ggsave(here::here(path, glue::glue("{path}.png")),
       width = 8,
       height = 7,
       dpi = 300)
