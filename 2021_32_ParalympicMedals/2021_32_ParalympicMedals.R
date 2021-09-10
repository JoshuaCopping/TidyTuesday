### TidyTuesday: 2021_32_ParalympicMedals ###

## Setup ----------
library(tidyverse)
library(countrycode)
library(ggflags) 
library(ggbump)
library(waffle)
library(ggtext)
library(patchwork)
library(png)

## Data ----------
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')

path <- paste("2021_32_ParalympicMedals")
logo <- readPNG(here::here(path, "logo", "IPC_logo.png"), native = TRUE)

ath_gender <- athletes %>%
  select(year, type, event, gender) %>% 
  filter(gender %in% c("Men", "Women")) %>% 
  group_by(year, gender) %>% 
  summarise(count = n()) %>%
  mutate(freq = round(count/sum(count)*100, 0)) 

top_countries_list <- athletes %>% 
  filter(medal == "Gold") %>% 
  mutate(abb = case_when(abb == "URS" ~ "RUS",
                         abb == "EUN" ~ "RUS",
                         abb == "GER" ~ "DEU",
                         abb == "FRG" ~ "DEU",
                         abb == "NED" ~ "NLD",
                         abb == "DEN" ~ "DNK",
                         abb == "IRI" ~ "IRN",
                         abb == "RSA" ~ "ZAF",
                         TRUE ~ abb)) %>%
  count(abb, sort = T) %>% 
  head(10) %>% 
  pull(abb)

top_countries <- athletes %>%
  mutate(abb = case_when(abb == "URS" ~ "RUS",
                         abb == "EUN" ~ "RUS",
                         abb == "GER" ~ "DEU",
                         abb == "FRG" ~ "DEU",
                         abb == "NED" ~ "NLD",
                         abb == "DEN" ~ "DNK",
                         abb == "IRI" ~ "IRN",
                         abb == "RSA" ~ "ZAF",
                         TRUE ~ abb)) %>%
  filter(medal == "Gold", 
         abb %in% top_countries_list) %>% 
  distinct(abb, year, event, .keep_all = T) %>% 
  count(abb, year,  sort = T) %>% 
  complete(abb, year, fill = list(n = 0)) %>%
  group_by(year) %>%
  mutate(rank = rank(-n, ties.method = "first")) %>%
  ungroup(year) %>%
  arrange(year) %>% 
  mutate(iso2=tolower(countrycode(abb, "iso3c", "iso2c")))

top <- c("USA","CHN")

newsport <- athletes %>% 
  select(year, event, type) %>% 
  drop_na() %>% 
  distinct() %>% 
  group_by(year, type) %>% 
  mutate(new = case_when(type == "Powerlifting" ~ "B",
                         type == "Rugby" ~ "B",
                         type == "Triathlon" ~ "B",
                         TRUE ~ "A")) %>%
  group_by(year, new)%>%
  summarise(n = n())

newsport_text <- tribble(
  ~label, ~year, ~x, ~y,
  "Powerlifting", 1984, 1,60,
  "Rugby", 1996, 4, 45,
  "Triathlon", 2016, 4, 40
)

newsport_arrow <- tribble(
  ~year, ~xmin, ~xmax, ~ymin, ~ymax,
  1984, 0.9, 2, 57, 59,
  1996, 0.8, 4, 36, 45,
  2016, 0.8, 4, 29, 40
)

## Plot ----------
theme_set(theme_void(base_family = "Poppins"))

theme_update(
  legend.position = "none",
  axis.text.x = element_text(size = 22),
  axis.text.y = element_text(size = 22),
  strip.text = element_text(size = 22),
  plot.title = element_markdown(hjust = 0.5,
                                family = "Poppins SemiBold",
                                size = 46,
                                margin = margin(20, 10, 20, 10)),
  plot.subtitle = element_markdown(hjust = 0.5,
                                   size = 24,
                                   margin = margin(0, 10, 10, 10)),
  plot.caption = element_text(size = 14),
  plot.margin = margin(30, 20, 20, 20),
  plot.background = element_rect(colour = "white",
                                 fill = "white")
)

gold <- ggplot(top_countries, 
               aes(year, rank)) +
  geom_bump(aes(colour = ifelse(abb %in% top, abb, NA),
                alpha = ifelse(abb %in% top, 1, 0.4),
                group = abb),
            size = 3) +
  geom_flag(aes(country = iso2),
            size = 12) +
  geom_point(aes(alpha = ifelse(abb %in% top, 0, 0.7)),
             size = 13,
             colour = "grey80") + 
  geom_point(aes(colour = ifelse(abb %in% top, abb, NA),
                 alpha = ifelse(abb %in% top, 1, 0.7)),
             shape = 1, 
             size = 13,
             stroke = 3) +
  scale_x_continuous(breaks = seq(1980, 2016, by = 4)) +
  scale_y_reverse(breaks = seq(1, 10, by = 1)) +
  scale_alpha_identity() +
  scale_color_manual(values = c("CHN" = "#EE334E",
                                "USA" = "#0081C8")) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "In the past 10 Paralympic games, there have been two nations battling for first place in the medals table:<br>The early dominance of the <b style='color: #0081C8'>USA</b>, now appears to have been replaced by <b style='color: #EE334E'>China</b>, who have equalled<br>the USA's top spot record, with four finishes in first place.")

events <- ggplot() +
  geom_waffle(data = newsport, 
              aes(fill = fct_rev(new), values = n),
              color = "white", 
              size = 0.25, 
              n_rows = 10, 
              flip = TRUE) +
  facet_wrap(~year, 
             nrow = 1, 
             strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10,
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#00A651", "grey80")) +
  coord_equal(clip = "off") +
  labs(subtitle = "The number of events available for Paralympians to compete in continues to change. Events and categories<br>are updated, and <b style='color: #00a651'>new sports</b> added to the games regularly. Since 1980, powerlifting, rugby, and triathlon<br>have all been included, with each event in that sport represented by 1 block in the plot below.<br> ") +
  theme(axis.text.y = element_blank(),
        panel.spacing = unit(1.8, "lines"),
        strip.text = element_text(margin = margin(t = 20))) +
  geom_text(data = newsport_text,
            aes(x, y, label = label),
            hjust = 0,
            size = 7) +
  geom_curve(data = newsport_arrow,
             aes(x = xmin, y = ymin, xend = xmax, yend = ymax),
             arrow = arrow(length = unit(0.1, "npc"),
                           type = "open",
                           ends = "first"),
             size = 0.8,
             curvature = -0.1)

gender <- ggplot(ath_gender, 
                 aes(x = year, y = freq, colour = gender)) +
  geom_line(aes(group = year), 
            colour = "grey80",
            size = 2,
            linetype = "dotted") +
  geom_point(shape = 21,
             size = 24,
             stroke = 3,
             fill = "white") +
  geom_text(aes(label = paste0(freq, "%")),
            size = 8,
            family = "Poppins",
            fontface = "bold") +
  scale_colour_manual(values = c("#0081c8", "#ee334e")) +
  scale_x_continuous(breaks = seq(1980, 2016, by = 4)) + 
  scale_y_continuous(expand = c(0.1, 0)) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "In 1988, the number of available medals for <b style='color: #0081C8'>men</b> was double the available medals for <b style='color: #EE334E'>women</b>.<br>Since then, the inequality in medal distribution has continually decreased, and in 2016 there were <br> nearly an equal number of medals available to male and female competitors.") +
  theme(axis.text.y = element_blank())

final <- gold/events/gender + 
  plot_layout(ncol = 1, heights = c(1, 1, 1)) +
  plot_annotation(title = "How Have The Paralympic Games Changed?",
                  caption = "Visualisation: Joshua Copping | Data: International Paralympic Committee") + 
  inset_element(logo, 0.89, 3.88, 0.99, 4.08)

## Save ----------
ggsave(plot = final,
       here::here(path, glue::glue("{path}.png")),
       width = 20,
       height = 30,
       dpi = 300)
