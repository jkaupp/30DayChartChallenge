library(tidyverse)
library(here)
library(jkmisc)
library(ggtext)
library(ggforce)

plot <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 12, ymin = -10.5, ymax = 0), fill = "firebrick", color = "white") +
  geom_vline(aes(xintercept = c(3,6,9))) +
  stat_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 10, start = pi, end = pi/2), fill = "white", geom = "polygon") +
  geom_arc(aes(x0 = 0, y0 = 0, r = 10, start = pi, end = pi/2), color = "black") +
  geom_segment(aes(x = 0, xend = 0, y = -10.5, yend = -10)) +
  geom_segment(aes(x = 12, xend = 12, y = -10.5, yend = 0)) +
  geom_segment(aes(x = 0, xend = 12, y = -10.5, yend = -10.5)) +
  geom_segment(aes(x = 10, xend = 12, y = 0, yend = 0)) +
  labs(x = NULL,
       y = NULL,
       title = "DISCO RECORD<br>SALES",
       caption = "Data: DiscoStu | Graphic: @jakekaupp") +
  scale_x_continuous(limits = c(0, 12), expand = c(0,0.01), breaks = c(1.5, 4.5, 7.5, 10.5), labels = c(73:76)) +
  scale_y_continuous(limits = c(-10.5, 0), expand = c(0,0)) +
  theme_jk(base_size = 40,
           plot_title_size = 80,
           markdown = TRUE) +
  theme(axis.text.y = element_blank(),
        plot.title = element_markdown(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "#9BA135", color = NA))

ggsave(here("day26", "tdcc_day26.png"), plot, width = 8, height = 11)
