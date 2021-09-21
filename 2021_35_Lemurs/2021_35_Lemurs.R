### TidyTuesday: 2021_35_Lemurs ###

## Setup ----------
library(tidyverse)
library(png)
library(cowplot)

## Data ----------
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')
taxonomy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/taxonomy.csv')

path <- paste("2021_35_Lemurs")
lemur_img <- readPNG(here::here(path, "lemur_image", "lemur.png"), native = TRUE)

lemurs_tidy <- lemurs %>% 
  mutate(taxon = fct_recode(taxon, "CMEAD" = "CMED")) %>% 
  left_join(taxonomy, by="taxon") %>% 
  drop_na(age_at_death_y) %>% 
  filter(sex %in% c("M", "F")) %>% 
  filter(taxon %in% c("LCAT")) %>% 
  mutate(point_size = case_when(preg_status == "NP" ~ "a",
                                infant_lit_sz_if_preg	 == 1 ~ "b",
                                infant_lit_sz_if_preg	 == 2 ~ "c")) %>% 
  distinct()

age_text <- tribble(
  ~text, ~x, ~y,
  "Adults", 2.75, 1.4,
  "Young adults", 1.4, 0.6,
  "Infants/Juveniles", 0.5, 0.05
)

legend_data <- tribble(
  ~age_at_wt_y, ~weight_g, ~sex, ~point_size, ~text,
  14.1, 1620, "M", "a", "Male",
  14.1, 1500, "F", "a", "Non-pregnant female",
  25, 1620, "F", "b", "Pregnant female, 1 offspring",
  25, 1500, "F", "c", "Pregnant female, 2 offspring"
)

label_annotations <- tribble(
  ~x, ~y, ~text,
  24, 3.7, "Heaviest lemur: 3.62 kg\nName: Nemo", 
  28.5, 2.7, "Oldest lemur: 32.73 years\nName: Tugger"
)

## Plot ----------
lemur_plot <- 
  ggplot(lemurs_tidy, 
         aes(age_at_wt_y, weight_g/1000,)) +
  geom_point(aes(shape = sex,
                 colour = sex,
                 size = point_size,
                 stroke = ifelse(sex %in% "M", 1, 0)),
             alpha = 0.5) +
  annotate("segment", 
           x = 1.3, xend = 1.3, 
           y = 0.6, yend = 2.9, 
           colour = "grey40") +
  annotate("segment", 
           x = 2.625, xend = 2.625, 
           y = 1.4, yend = 3.7, 
           colour = "grey40") +
  geom_text(data = age_text, 
            aes(x = x, y = y, 
                label = text),
            angle = 90,
            hjust = 0,
            vjust = 1,
            colour = "grey40") +
  geom_text(aes(x = 14, y = 1.35),
            label = "The ring-tailed lemur (Lemur catta), named after it's long, black,\nand white ringed tail, is perhaps the most recognisable species\nof lemur. Like all lemurs, the ring-tailed lemur is endemic to the\nisland of Madagascar, living in arid, open areas and forests, in\nterritories that range from 15 to 55 acres. Data from the Duke\nLemur Center, North Carolina USA, shows that males\npredominantly live longer and weigh more than females, and\npregnant females in mid-adulthood tend to produce 2 offspring\nper pregnancy, compared to only 1 earlier and later in their lives.",
            size = 4, 
            lineheight = 1,
            hjust = 0,
            vjust = 1,
            colour = "grey40") + 
  draw_image(lemur_img,
             x = 14, 
             y = 0.1,
             scale = 22) +
  annotate("segment",
           x = 13, xend = 33, 
           y = 1.4, yend = 1.4, 
           size = 0.5, 
           colour = "grey40") +
  annotate("segment",
           x = 13.5, xend = 13.5,
           y = 1.45, yend = 0.7,
           size = 0.5,
           colour = "grey40") +
  geom_point(data = legend_data, 
             aes(shape = sex,
                 colour = sex,
                 size = point_size,
                 stroke = ifelse(sex %in% "M", 1, 0)),
             alpha = 0.7) +
  geom_text(data = legend_data, 
            aes(label = text),
            position = position_nudge(x = 0.9),
            hjust = 0,
            vjust = 0.4,
            size = 3.5,
            colour = "grey40") +
  geom_text(data = label_annotations,
            aes(x = x, y = y, 
                label = text),
            size = 3.5,
            colour = "grey40",
            hjust = 0,
            lineheight = 0.8) + 
  geom_curve(aes(x = 18.5, xend = 23.8, y = 3.63, yend = 3.71),
             arrow = arrow(length = unit(0.01, "npc"),
                           type = "open",
                           ends = "first"),
             size = 0.2,
             colour = "grey40",
             curvature = -0.1) + 
  geom_curve(aes(x = 33, xend = 33, y = 2.24, yend = 2.65),
             arrow = arrow(length = unit(0.01, "npc"),
                           type = "open",
                           ends = "first"),
             size = 0.2,
             colour = "grey40",
             curvature = 0.7) +
  scale_shape_manual(values = c(16, 1)) +
  scale_size_manual(values = c(2, 5, 10),
                    labels = c("not pregnant", "1", "2")) +
  scale_colour_manual(values = c( "#8515F0", "#37DEB7"),
                      guide = "none") +
  scale_x_continuous(limits = c(0, 35),
                     expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 4),
                     expand = c(0.02, 0.02)) +
  coord_equal(ratio = 10, 
              clip = "off") +
  labs(x = "Age (years)",
       y = "Weight (kg)",
       title = "The Life of Ring-Tailed Lemurs",
       caption = "Visualisation: Joshua Copping | Data: Kagge/Zehr et al, 2014") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Poppins"),
        axis.text = element_text(size = 12,
                                 colour = "grey30"),
        axis.title = element_text(size = 14,
                                  colour = "grey30"),
        plot.title = element_text(hjust = 0.5,
                                  size = 28,
                                  colour = "grey30",
                                  family = "Poppins SemiBold"),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 1.1,
                                    size = 8,
                                    colour = "grey60",
                                    margin = margin(15, 0, 5, 0)),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "white",
                                       colour = "white"),
        plot.margin = margin(20, 45, 0, 20)) 

## Save ----------
ggsave(plot = lemur_plot,
       here::here(path, glue::glue("{path}.png")),
       width = 9,
       height = 10,
       dpi = 300)
