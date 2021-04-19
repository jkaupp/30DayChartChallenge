library(tidyverse)
library(here)
library(jkmisc)
library(ggforce)
library(colorspace)
library(rvest)
library(janitor)
library(patchwork)


pokemon <- read_csv(here("day17", "data", "pokemon.csv"))

pokemon_type_colors <- read_html("http://www.pokemonaaah.net/artsyfartsy/colordex/") %>%
  html_node(".typecolor") %>%
  html_table(header = TRUE) %>%
  clean_names() %>%
  mutate(type = tolower(str_remove(type, "[a-z]\\s")),
         type_color = str_extract(all_mix_hex_hsb, "\\#.{6}"))

plot_data <- pokemon %>%
  mutate(ad_ratio = attack/defense,
         fill = case_when(ad_ratio < 1 ~ "below",
                          ad_ratio > 1 ~ "above",
                          TRUE ~ "at")) %>%
  left_join(pokemon_type_colors, by = c("type1" = "type"))

upper <- tibble(x = c(0, 250, 0),
            y = c(0, 250, 250))

lower <- tibble(x = c(0, 250, 250),
                y = c(0, 0, 250))

abline <- tibble(x = seq(0, 250, 50),
                 y = seq(0, 250, 50))

all_pokemon <- ggplot(plot_data, aes(x = defense, y = attack)) +
  geom_segment(data = abline, aes(x = 0, xend = 250, y = 0, yend = 250)) +
  geom_point(aes(fill = fill, color = fill), shape = 21, stroke = 1, size = 3, show.legend = FALSE) +
  annotate("text", x = 200, y = 200, label = spaced_title("Offensive"), family = "Oswald Bold", angle = 45, vjust = -0.5, size = 8, color = "#2a75bb") +
  annotate("text", x = 200, y = 200, label = spaced_title("Defensive"), family = "Oswald Bold", angle = 45, vjust = 1.5, size = 8, color = "#ffcb05") +
  labs(x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("#2a75bb", "grey60", "#ffcb05")) +
  scale_color_manual(values = darken(c("#2a75bb", "grey60", "#ffcb05"))) +
  theme_jk(grid = "XY") +
  coord_equal()

types <- ggplot(plot_data, aes(x = defense, y = attack)) +
  geom_segment(data = abline, aes(x = 0, xend = 250, y = 0, yend = 250)) +
  geom_point(aes(fill = fill, color = fill), shape = 21, stroke = 1, size = 3, show.legend = FALSE) +
  labs(x = NULL,
       y = NULL) +
  facet_wrap(~type1, nrow = 3, labeller = as_labeller(function(x)  spaced_title(x))) +
  scale_fill_manual(values = c("#2a75bb", "grey60", "#ffcb05")) +
  scale_color_manual(values = darken(c("#2a75bb", "grey60", "#ffcb05"))) +
  theme_jk(grid = "XY") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()

final <- all_pokemon + types + plot_annotation(title = "Are Pokemon All Balanced Between Attack and Defense Or Are There Type-Specific Differences?",
                                               subtitle = "Illustrated below are scatterplots of attack stats versus defense stats for all pokemon generations 1-7. The first larger plot show every pokemon, with each pokemon type broken out into small multiples.  Clearly, some pokemon types are more geared for attacking or defense moreso than others.",
                                               caption = "**Data**: @edubaschool c/o data.world | **Graphic**: @jakekaupp",
                                               theme = theme_jk(plot_title_size = 30))

ggsave(here('day17', "tdcc_day17.png"), final, width = 26, height = 10)
