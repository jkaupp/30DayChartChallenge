library(tidyverse)
library(here)
library(jkmisc)
library(janitor)
library(ggtext)
library(ggfx)
library(patchwork)

water_access <- here("day19", "data", "number-without-access-water-source.csv") %>%
  read_csv() %>%
  clean_names() %>%
  set_names(c("entity", "code", "year", "no_access"))


countries <- filter(water_access, entity %in% c("North America",
                                                "Europe & Central Asia",
                                                "Latin America & Caribbean",
                                                "Middle East & North Africa",
                                                "South Asia",
                                                "East Asia & Pacific",
                                                "Sub-Saharan Africa"))

world <- filter(water_access, code == "OWID_WRL") %>%
  select(year, world = no_access)


plot_data <- countries %>%
  left_join(world) %>%
  mutate(percent = no_access/world)


top <- ggplot(plot_data, aes(x = year, y = percent)) +
  as_reference(geom_text(aes(label = "GLOBAL ACCESS TO WATER", x = 1990, y = 0), size = 40, family = "Anton", color = "#FBFEF9", hjust = 0, vjust = 0),
    id = "area") +
  with_blend(
    geom_area(aes(group = entity, fill = entity), show.legend = FALSE),
    bg_layer = "area",
    blend_type = "atop") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer("Blues") +
  labs(x = NULL, y = NULL) +
  theme_jk(dark = TRUE,
           grid = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#2E3440", color = NA))

facets <- ggplot(plot_data, aes(x = year, y = percent)) +
  as_reference(
    geom_text(aes(label = entity, x = 1990, y = 0), size = 20, family = "Anton", color = "#FBFEF9", hjust = 0, vjust = 0),
    id = "area") +
  with_blend(
    geom_area(aes(group = entity, fill = entity), show.legend = FALSE),
    bg_layer = "area",
    blend_type = "xor") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 0.4, 0.2), labels = c("0", "20", "40%")) +
  scale_fill_brewer("Blues") +
  facet_wrap(~entity, ncol = 1) +
  labs(x = NULL,
       y = NULL,
       subtitle = "The Area chart below shows the percentage of the global population living without access to an improved water source by region.  This regional breakdown has changed considerably, declining over most\nregions except for Sub-Saharan Africa where percentage of people without access to water has been steadily rising.") +
  theme_jk(dark = TRUE,
           grid = "X") +
  theme(strip.text = element_blank(),
        plot.background = element_rect(fill = "#2E3440", color = NA))

plot <- top / facets + plot_layout(height = c(0.15, 0.85)) + plot_annotation(theme = theme_jk(dark = TRUE,
                                                                                              markdown = TRUE),
                                                                             caption = "**Data**: OurWorldinData | **Graphic**: @jakekaupp")

ggsave(here('day19', 'tdcc_day19.png'), plot, width = 16, height = 12, device = ragg::agg_png(), dpi = 300)

altText::alt_text(plot)
