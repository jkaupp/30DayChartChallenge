library(tidyverse)
library(here)
library(jkmisc)
library(ggstream)
library(colorspace)
library(lubridate)
library(ggtext)

cards <- here("week1", "data", "cards.csv") %>%
  read_csv()

sets <- here("week1", "data", "sets.csv") %>%
  read_csv() %>%
  rename(set_name = name,
         set_id = id,
         set_index = index)

full <- cards %>%
  left_join(sets, by = c("setCode" = "code"))

streams <- full %>%
  separate_rows(types, sep = ",") %>%
  filter(types %in% c("Artifact", "Creature", "Instant", "Sorcery", "Enchantment", "Planeswalker", "Land")) %>%
  count(releaseDate, types) %>%
  arrange(releaseDate) %>%
  group_by(types) %>%
  mutate(rolling_n = cumsum(n)) %>%
  ungroup() %>%
  mutate(idx = as.numeric(factor(releaseDate)))

set_breaks <- sets %>%
  filter(block == "Core Set", str_starts(code, "P", negate = TRUE)) %>%
  distinct(code, releaseDate, set_name) %>%
  arrange(releaseDate) %>%
  left_join(distinct(streams, releaseDate, idx)) %>%
  mutate(label = sprintf("%s\n%s", set_name, year(releaseDate))) %>%
  filter(idx %notin% c(2, 3, 9), code != "FBB") %>%
  mutate(label = case_when(code == "LEA" ~ "ABU\n1993",
                           code == "3ED" ~ "Revised\n1994",
                           TRUE ~ label))

labels <- tibble(types = c("Artifact", "Creature", "Instant", "Sorcery", "Enchantment", "Planeswalker", "Land"),
                 x = rep(310, 7),
                 y = c(0.95, 0.70, 0.40, 0.27, 0.18, 0.115, 0.05))


ggplot(streams, aes(x = idx, y = rolling_n, fill = types, color = types)) +
  geom_stream(type = "proportional", show.legend = FALSE) +
  geom_text(data = labels, aes(x = x, y = y, label = spaced_title(types)), color = "white", family = "Oswald", hjust = 0) +
  labs(x = NULL,
       y = NULL,
       title = "#30DayChartChallenge | Day 1 | Part to Whole: Distribution of Types in Magic: The Gathering History",
       subtitle = "A stacked area chart illustrating the cummulative part-to-whole view of card types over the history of Magic: The Gathering.",
       caption = "**Data**: mtgjson.com | **Graphic**: @jakekaupp") +
  scale_fill_manual(values = semiotic_pal) +
  scale_color_manual(values = darken(semiotic_pal)) +
  scale_x_continuous(breaks = set_breaks$idx, labels = set_breaks$label, expand = c(0.01,0)) +
  scale_y_continuous(labels = scale_percent_labels, expand = c(0.01,0)) +
  theme_jk(dark = TRUE,
           grid = FALSE,
           markdown = TRUE,
           ticks = TRUE) +
  theme(axis.ticks = element_line(color = "#E5E9F0"),
        axis.text.x = element_markdown(hjust = 0))
