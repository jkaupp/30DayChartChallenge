library(tidyverse)
library(here)
library(scales)
library(ggfx)
library(ggtext)
library(glue)
library(colorspace)


data <- tibble(left =  rbeta(10000,5,2),
       right = rbeta(10000,2,5),
       centre = rbeta(10000,5,5)) %>%
  pivot_longer(everything()) %>%
  mutate(name = factor(name, levels =  c("left", "centre", "right"))) %>%
  arrange(name)


labels <- tibble(label = c("skewed left", "skewed right", "symmetric"),
                 name = c("left", "right", "centre"),
                 x = c(0.5, 0.5, 0.5),
                 y = 0.01) %>%
  mutate(name = factor(name, levels =  c("left", "centre", "right")))


plot <- ggplot(data) +
  as_reference(
    geom_text(data = labels, aes(label = toupper(label), x = x, y = y), size = 60, family = "Anton", color = "#FBFEF9", hjust = 0.5, vjust = 0),
    id = "text") +
  with_blend(
    geom_density(aes(x = value, fill = name, color = name), show.legend = FALSE),
    bg_layer = "text",
    blend_type = "xor") +
  labs(x = NULL,
       y = NULL,
       title = "The Shape of Distributions",
       caption = "**Graphic**: @jakekaupp") +
  facet_wrap(~name, ncol = 1) +
  scale_x_continuous(limits = c(-0.1, 1.1)) +
  scale_fill_manual(values = c("#0081a7","#00afb9","#f07167")) +
  scale_color_manual(values = darken(c("#0081a7","#00afb9","#f07167"))) +
  theme_jk(dark = TRUE,
           grid = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        plot.caption = element_markdown(),
        plot.title = element_markdown(hjust = 0.5, family = "Alata"))


ggsave(here("day9", "30dcc_day9.png"), plot, height = 9, width = 14, units = 'in', device = ragg::agg_png())
