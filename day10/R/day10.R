library(tidyverse)
library(ggvoronoi)
library(here)
library(jkmisc)
library(ggtext)
library(magick)

n <- 200000

normal_circle <- tibble(x = runif(n, -1, 1),
       y = runif(n, -1, 1),
       d = x^2 + y^2 < 1)

plot <- slice_sample(circle, n = 1000) %>%
ggplot(aes(x = x, y = y, fill = d), color = "#77878B") +
  geom_voronoi(show.legend = FALSE) +
  scale_fill_manual(values = c("#373E40", "#305252")) +
  labs(x = NULL,
       y = NULL) +
  annotate("text", x = 0, y = 0.3, label = "Voronoi Tesselation", family = "Anton", color = "#77878B", size = 16) +
  annotate("text", x = 0, y = 0.13, label = "of a", family = "Oswald", color = "#77878B", size = 8) +
  annotate("text", x = 0, y = 0, label = expression("Monte Carlo Estimation of"~pi), family = "Lobster", color = "#77878B", size = 8) +
  annotate(GeomRichText, x = 1.15, y = -1.15, label = "**Graphic**:@jakekaupp", family = "Lato", hjust = 1, vjust = 0, color = "#77878B", fill = "transparent", label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  coord_equal() +
  theme_jk(grid = FALSE,
           ticks = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())


ggsave(here("day10", "30dcc_day10.png"), plot, width = 10, height = 10, device = ragg::agg_png())

image_read(here("day10", "30dcc_day10.png")) %>%
  image_trim() %>%
  image_write(here("day10", "30dcc_day10.png"))
