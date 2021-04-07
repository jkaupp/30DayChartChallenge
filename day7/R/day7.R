library(tidyverse)
library(jkmisc)
library(here)
library(ggtext)
library(glue)
library(viridis)
library(patchwork)

data <- read_rds(here("day7", "data", "LLCP2019.rds"))

pal <- cividis(8)

stem_leaf_counts <- data %>%
  select(height = HEIGHT3) %>%
  filter(height < 7777) %>%
  separate(height, c("stem", "leaf"), 1, convert = TRUE) %>%
  arrange(stem, leaf) %>%
  count(stem, leaf)

legend_data <- tibble(level = 1:8) %>%
  mutate(label = c("0 to 5000",
                   "5001 to 10000",
                   "10001 to 15000",
                   "15001 to 20000",
                   "20001 to 25000",
                   "25001 to 30000",
                   "30001 to 35001",
                   "35001 to 40000+")) %>%
  mutate(color = cividis(8))


color_stem <- stem_leaf_counts %>%
  count(stem, wt = n) %>%
  mutate(cat = pal[cut(n, labels = FALSE, breaks = pretty(n, 8))])


color_leaves <- stem_leaf_counts %>%
  mutate(cat = pal[cut(n, labels = FALSE, breaks = pretty(n, 10))]) %>%
  group_by(stem) %>%
  summarize(leaf = paste0(glue("<span style='color:{cat}'>{leaf}</span>"), collapse = ""))


legend <- ggplot(legend_data, aes(x = 0, y = level, fill = color)) +
  geom_tile() +
  geom_text(aes(label = label, x = 1), family = "Lato", size = 3, hjust = 0) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(-2,6)) +
  coord_equal(clip = "off") +
  theme_void()

plot <- ggplot(stem_leaf_counts, aes(x = 0, y = stem)) +
  geom_text(data = color_stem, aes(label = stem), family = "Anton", size = 6, color = pal[1]) +
  geom_richtext(data = color_leaves, aes(label = leaf, x = 0.1), hjust = 0, size = 6, family = "Anton", fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), vjust = 0.55) +
  geom_vline(xintercept = 0.07,size = 0.1, color = pal[1]) +
  annotation_custom(ggplotGrob(legend), xmin = 1, xmax = 1.7, ymin = 4, ymax = 7) +
  scale_x_continuous(limits = c(0,2)) +
  scale_color_identity() +
  labs(x = NULL,
       y = NULL,
       title = "US Height Distribution from the 2019 BRFSS",
       subtitle = "A modified stem-and-leaf plot of reported respodent height<br>from the Behavioral Risk Factor Surveillance System Survey.",
       caption = "**Data**: Centers for Disease Control and Prevention | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           ticks = FALSE,
           plot_title_size = 20,
           plot_title_family = "Anton") +
  theme(plot.background = element_rect(fill = "#FBFBF2", color = "#FBFBF2"),
        plot.title = element_markdown(color = pal[1]),
        plot.subtitle = element_markdown(color = pal[1]),
        plot.caption = element_markdown(color = pal[1]),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave(here("day7", "30dcc_day7.png"), plot, width = 5, height = 5, device = ragg::agg_png())

altText::alt_text(plot)
