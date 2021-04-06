library(tidyverse)
library(ggtext)
library(jkmisc)
library(here)
library(glue)

search_data <- tibble(term = c("in Life After Love",
                               "in Magic",
                               "in Miracles",
                               "in Love"),
                      interest = c(5, 12, 2, 18),
                      x = rep(0, 4),
                      y = 0:3*-0.3) %>%
  rowwise() %>%
  mutate(color = if_else(term == "in Magic", "firebrick", sample(grey.colors(1000),1)))

ticks <- tibble(x = seq(0, 20, 5),
                xend = x,
                y = -1.1,
                yend = -1.12)

plot <- ggplot(search_data, aes(x = 0, y = 0)) +
  geom_text(label = "Do you believe", family = "Julius Sans One", size = 15, hjust = 1.1, vjust = 0.5) +
  geom_segment(aes(x = x, xend = interest, y = y, yend = y, color = color), size = 20, show.legend = FALSE) +
  geom_text(aes(label = term, x = x, y = y), hjust = 0, family = c("Rockwell", "Monoton", "Schoolbell", "Pacifico"), size = 12, vjust = 0.5) +
  geom_segment(y = -1.1, yend = -1.1, x = 0, xend = 20, size = 1) +
  geom_segment(y = -1.1, yend = -1.1, x = 0, xend = 20, size = 1) +
  geom_segment(x = 0, xend = 0, y = -1.1, yend = 0.5) +
  geom_segment(data = ticks, aes(x = x, xend = xend, y = y, yend = yend)) +
  geom_text(data = ticks, aes(x = x, y = yend, label = seq(0, 20,5)), vjust = 1.1, family = "Staatliches") +
  scale_color_identity() +
  labs(x = NULL,
       y = NULL,
       title = glue("<span style = 'font-family:Rockwell'>**Love**</span> and <span style = 'font-family:Monoton'>{highlight_text('Magic', 'firebrick', 'b', size = 55)}</span>: the Things People Need to Believe In"),
       subtitle = "  Google trends data using the autocomplete results of 'Do you believe' presented as a bar chart comparing average interest over time from 2004 to the present.",
       caption = "**Data**: Google Trends | **Graphic**: @jakekaupp") +
  expand_limits(x = c(-10,20), y = c(-1.2, 0.5)) +
  theme_jk(grid = FALSE,
           ticks = FALSE,
           markdown = TRUE,
           plot_title_family = "Julius Sans One",
           subtitle_family = "Julius Sans One",
           plot_title_size = 40) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5))

ggsave(here("day4", "30dcc_day4.png"), plot, device = ragg::agg_png(units = "px",  width = 1728, height = 589))

alt_text <- altText::alt_text(plot)
