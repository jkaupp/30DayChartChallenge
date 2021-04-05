library(tidyverse)
library(googlesheets4)
library(jkmisc)
library(ggtext)
library(glue)
library(here)

playfair_revolution <- read_sheet("https://docs.google.com/spreadsheets/d/1h9tr4BgVc6j_NaS7BIpmtsnmEol5KS9Jui6uNWcGIPE/edit#gid=0")

plot <- ggplot(playfair_revolution, aes(x = year, y = interest)) +
  geom_area(color = "#BE2343", fill = "#F3E7CD", size = 1, alpha = 0.6) +
  geom_text(aes(label = label), angle = 90, hjust = c(-0.1, rep(-0.05, 9)), vjust = c(1, rep(0,9)), family = "Charm Bold") +
  geom_segment(aes(x = year, xend = year, y = 0, yend = 20), size = 0.2) +
  annotate("text", x = 1723, y = 1, label = 'Interest of the National Debt.',family = "Playfair Display SC Bold", size = 7, hjust = 0) +
  scale_y_continuous(limits = c(0, 20), breaks = 0:20, labels = c("", 1:20), position = "right", expand = c(0,0)) +
  scale_x_continuous(limits = c(1687, 1786), expand = c(0,0), breaks = playfair_revolution$year) +
  labs(x = "The Bottom Line is in Years, those on the Right hand Millions of Pounds",
       y = NULL,
       title = glue("Interest of the {highlight_text('NATIONAL DEBT', style = 'b', size = 22)} from the Revolution"),
       caption = "**Original**: William Playfair 1786 | **Reproduction**: @jakekaupp 2021") +
  theme_jk(grid = "Y",
           markdown = TRUE,
           base_family = "Playfair Display SC",
           plot_title_family = "Playfair Display",
           caption_family = "Playfair Display",
           axis_title_family = "Charm",
           axis_title_size = 14,
           caption_size = 12,
           plot_title_size = 20) %+replace%
  theme(panel.grid.major = element_line(color = "black", size = 0.2)) +
  theme(plot.title = element_markdown(hjust = 0.5),
        axis.text = element_markdown(size = 12),
        plot.caption = element_markdown(hjust = 1),
        panel.border = element_rect(fill = NA, size = 4))


ggsave(here("day3", "30dcc_day3.png"), plot, width = 12, height = 8, device = ragg::agg_png())

