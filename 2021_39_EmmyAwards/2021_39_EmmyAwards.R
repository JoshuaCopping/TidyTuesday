### TidyTuesday: 2021_39_EmmyAwards ###

## Setup ----------
library(tidyverse)
library(ggtext)

## Data ----------
nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')

netflix_nominations <- nominees %>%
  select(category, title, distributor, type, year) %>% 
  distinct(.keep_all = TRUE) %>%
  filter(year >= 2013) %>% 
  group_by(year, distributor) %>%
  count() %>%
  group_by(year) %>%
  mutate(nomination_rank = rank(-n, ties.method = "min")) %>%
  filter(distributor == "Netflix") %>%
  select(year, nomination_rank)

netflix_nominations_ordinal <- scales::ordinal(netflix_nominations[[2]])

netflix_wins <- nominees %>%
  select(category, title, distributor, type, year) %>% 
  filter(type == "Winner") %>%
  distinct(.keep_all = TRUE) %>%
  filter(year >= 2013) %>% 
  group_by(year, distributor) %>%
  count() %>%
  group_by(year) %>%
  mutate(win_rank = rank(-n, ties.method = "min")) %>%
  filter(distributor == "Netflix") %>%
  select(year, win_rank)

netflix_wins_ordinal <- scales::ordinal(netflix_wins[[2]])

top_nominees <- nominees %>% 
  select(category, title, distributor, type, year) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(distributor == "Netflix") %>%
  mutate(win = case_when(type == "Nominee" ~ 0,
                         type == "Winner" ~ 1)) %>%
  group_by(year) %>%
  summarise(total = n(),
            n_wins = sum(win)) %>%
  mutate(freq = n_wins / total) 

freq_pct <- top_nominees %>%
  select(freq) %>% 
  mutate(label = paste0(round(freq*100, 0),"%")) %>%
  pull(label)

point_text <- vctrs::vec_c(freq_pct, netflix_nominations_ordinal, netflix_wins_ordinal)

netflix_point_data <- top_nominees %>%
  select(-n_wins) %>% 
  cbind(nomination_rank = netflix_nominations$nomination_rank) %>%
  cbind(win_rank = netflix_wins$win_rank)%>% 
  pivot_longer(freq:win_rank, names_to = "type", values_to = "value") %>%
  arrange(type) %>% 
  cbind(label = point_text) %>% 
  mutate(alpha = case_when(type == "freq" ~ rank(value, ties.method = "min"),
                           type == "nomination_rank" ~ rank(-value, ties.method = "min"),
                           type == "win_rank" ~ rank(-value, ties.method = "min"))) %>% 
  group_by(type) %>% 
  mutate(alpha = scales::rescale(alpha, to = c(0.25, 1)),
         y_position = case_when(type == "freq" ~ total+6,
                                type == "nomination_rank" ~ total+19,
                                type == "win_rank" ~ total+27.4),
         colour = case_when(type == "freq" ~ "#E61E25",
                            type == "nomination_rank" ~ "#FFFFFF",
                            type == "win_rank" ~ "#FFFFFF"))


annotations <- tribble(
  ~year, ~y, ~text, ~x, ~xend, ~y2, ~y2end, ~curve,
  2022, 45, "number of wins", 9, 9.8, 40.5, 45, 0.4,
  2022, 80, "number of nominations", 9, 9.8, 118.5, 80, -0.15,
  2022, 120, "win/nomination ratio", 9.4, 9.8, 129, 120, 0.05,
  2022, 150, "nomination rank", 9.4, 9.8, 146, 152, -0.07,
  2022, 170,  "win rank", 9, 9.8, 157, 170, 0.4
)

## Plot ----------

  ggplot(top_nominees, 
       aes(x = as.factor(year))) +
  geom_bar(aes(y = total),
           stat = "identity",
           fill = "#B12025",
           colour = "#E61E25",
           alpha = 0.4,
           width = 0.7) +
  geom_bar(aes(y = n_wins),
           stat = "identity",
           fill = "#E61E25",
           width = 0.4,
           alpha = 0.9) +
  geom_text(aes(y = n_wins, 
                label = n_wins),
            colour = "#FFFFFF",
            size = 5,
            fontface = "bold",
            hjust = 1,
            nudge_y = 0) +
  geom_text(aes(y = total, 
                label = total),
            colour = "#FFFFFF",
            size = 5,
            fontface = "bold",
            hjust = 1,
            nudge_y = -0.2) +
  geom_point(data = netflix_point_data, 
             aes(y = y_position, 
                 alpha = alpha,
                 colour = colour),
             shape = 1,
             size = 14,
             stroke = 2) +
  geom_text(data = netflix_point_data,
            aes(y = y_position, 
                alpha = alpha,
                label = label),
            colour = "#FFFFFF",
            size = 4,
            family = "Roboto",
            fontface = "bold") +
  geom_text(data = annotations, 
            aes(y = y, 
                label = text),
            colour = "#FFFFFF",
            family = "Roboto",
            size = 4) +
  lapply(split(annotations, 1:nrow(annotations)), function(dat) {
    geom_curve(data = dat, 
               aes(x = x, xend = xend, y = y2, yend = y2end), 
               curvature = dat["curve"],
               arrow = arrow(length = unit(0.01, "npc"),
                             type = "open",
                             ends = "first"),
               colour = "#FFFFFF")}) +
  annotate("richtext", x = 2.5, y = 110, 
           label = "Netflix was first nominated for an Emmy Award<br>in 2013, receiving fourteen nominations and winning<br>three awards. Since then, Netflix's success has grown,<br> and in 2021 they were ranked first in terms of both<br>the number of nominations and wins received.<br>Will Netflix continue to dominate, next year?",
           label.colour = NA,
           fill = NA,
           size = 4,
           family = "Roboto",
           colour = "#FFFFFF",
           hjust = 0) +
  coord_flip(clip = "off") +
  scale_x_discrete(breaks = seq(2013, 2021, by = 1)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  scale_alpha_identity() +
  scale_colour_identity() +
  labs(title = "<span style = 'color: #FFFFFF;font-size: 42px;'>the history of</span> <span style = 'color: #E61E25;font-size: 88px;'>netflix</span> <span style = 'color: #FFFFFF;font-size: 42px;'>at the emmys</span>",
       caption = "Visualisation: Joshua Copping &bull; Data: Emmys") +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(10, 20, 10, 20),
        panel.background = element_rect(fill = "#292929",
                                        colour = "#292929"),
        plot.background = element_rect(fill = "#292929"),
        text = element_text(family = "Bebas"),
        plot.title = element_markdown(hjust = 0.5,
                                      margin = margin(10, 0, 10, 0)),
        plot.caption = element_markdown(family = "Roboto",
                                        size = 8,
                                        colour = "#FFFFFF",
                                        hjust = 1),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        axis.text.y = element_text(colour = "#FFFFFF",
                                   size = 24)) 

## Save ----------
path <- paste("2021_39_EmmyAwards")

ggsave(here::here(path, glue::glue("{path}.png")),
       width = 12,
       height = 8,
       dpi = 300)
