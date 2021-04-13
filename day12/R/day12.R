library(tidyverse)
library(here)
library(jkmisc)
library(ggfx)
library(glue)
library(ggtext)

hfx_temp <- read_csv(here("day12", "data", "mm8202251.txt")) %>%
  slice(-1) %>%
  select(-starts_with("X")) %>%
  select(Year, matches(month.abb), Annual) %>%
  mutate(across(everything(), ~replace(.x, .x == -9999.9, NA))) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(anom = Annual - mean(Annual, na.rm = TRUE))

plot <- ggplot()+
  as_reference(geom_text(aes(x = 1868, y = 1, label = "HALIFAX"), family = "Passion One", size = 100, hjust = 0, color = "white"),
                   id = 'text') +
  with_blend(geom_tile(data = hfx_temp, aes(x = Year, y = 1, fill = Annual)),
            bg_layer = "text",
            blend_type = "in",
            id = "blended") +
  with_shadow("blended", sigma = 5) +
  annotate('text', x = 1945.5, y = 1.3, label = spaced_title("Average Annual Temperatures in"), hjust = 0.5, family = "Oswald Light", size = 6) +
  annotate('text', x = 1935.5, y = 0.65, label = spaced_title('from 1871 to 2020'), family = "Oswald Light", hjust = 0.5, size = 6) +
  annotate(GeomRichtext, x = 1962.5, y = 0.646, label = glue("{highlight_text('3 . 7 ° C', '#053061', size = 22)} - {highlight_text('8 . 6 ° C', '#67001F',   size = 22)}"), family = "Oswald", hjust = 0.5, fill = NA, label.color = NA,
           label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_fill_distiller("Temperature Anomaly", palette = "RdBu") +
  labs(x = NULL,
       y = NULL,
       caption = "**Data**: Environment Canada | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           plot_title_family = "Oswald Light") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_markdown(),
        legend.position = "none") +
  guides(fill = guide_colorbar(title.position = "top"))

ggsave(here("day12", "30dcc_day12.png"), plot, width = 14, height = 6, device = ragg::agg_png())
