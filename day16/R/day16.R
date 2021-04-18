library(tidyverse)
library(jkmisc)
library(here)
library(janitor)
library(jkmisc)
library(colorspace)
library(ggforce)
library(glue)
library(ggalt)

tree_data <- here("day16", "data", "TS3_Raw_tree_data.csv") %>%
  read_csv() %>%
  clean_names() %>%
  mutate(diameter = dbh_cm/100,
         height = tree_ht_m) %>%
  filter(height > 0)

big_trees <- filter(tree_data, height == max(height) | diameter == max(diameter) | tree_id == 9193)


plot <- ggplot(tree_data, aes(x = diameter, y = height)) +
  annotate("text", x = 0.9, y = 51, label = "E", family = "StateFace", size = 20, color = "grey60") +
  annotate("text", x = 2.9, y = 20.5, label = "K", family = "StateFace", size = 20, color = "grey60") +
  annotate("text", x = 2.2, y = 41, label = "u", family = "StateFace", size = 20, color = "grey60") +
  geom_hex(bins = 100, show.legend = FALSE) +
  labs(x = NULL,
       y = NULL,
       title = "The Relationship between the Height and Diameter of America's Trees",
       subtitle = glue("Illustrated below are height against diameter from 14,000 urban street and park trees from 1998-2020 from cities in 17 states.  The measurements<br>are presented as a hexagonal heatmap to show {highlight_text('high frequency combinations', '#c7e9c0', 'b')} and central tendencies."),
       caption = "**Data**: US Forest Service | **Graphic**: @jakekaupp") +
  geom_mark_circle(data = big_trees, aes(label = common_name, description = glue("in {city}\n{height}m tall, {diameter}m wide")), expand = unit(0.1, "mm"), label.family = c("Bebas Neue", "Source Serif Pro"), label.fontsize = c(18, 12)) +
  scale_fill_distiller(palette = "Greens", direction = -1) +
  scale_y_continuous(labels = paste0(seq(0, 60, 10), c(rep("", 6), " meters")), breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_x_continuous(labels = paste0(seq(0, 3, 1), c(rep("", 3), " meters")), breaks = seq(0, 3, 1)) +
  theme_jk(plot_title_family = "Bebas Neue",
           plot_title_size = 30,
           base_family = "Bebas Neue",
           base_size = 14,
           subtitle_family = "Source Serif Pro",
           subtitle_size = 12,
           markdown = TRUE)

ggsave(here("day16", "tdcc_day16.png"), plot, width = 11.5, height = 9, device = ragg::agg_png())

altText::alt_text(plot)
