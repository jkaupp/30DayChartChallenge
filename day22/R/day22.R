library(tidyverse)
library(jkmisc)
library(here)
library(scales)
library(packcircles)
library(imager)
library(ggtext)
library(gganimate)
library(glue)

trees <- tibble(year = 2015:2217) %>%
  mutate(trees = 3040-15*(year-2015))

im <- load.image(here("day22", "data", "tree.png"))

im_df <- im %>%
  as.data.frame(wide = "c") %>% ## so that rgb value is in separate column.
  rename(im_x = x,
         im_y = y) %>%
  mutate(hex = rgb(c.1,c.2,c.3))


pack_layout <- circleProgressiveLayout(rbeta(3040,1,2), sizetype = 'area') %>%
  mutate(im_x = floor(rescale(x, to = range(im_df$im_x))),
         im_y = floor(rescale(y, to = range(im_df$im_y))),
         id = row_number()) %>%
  inner_join(select(im_df, im_x,im_y,hex), by = c("im_x","im_y"))

data_gg <- circleLayoutVertices(pack_layout) %>%
  inner_join(pack_layout %>% select(id,hex), by=c("id")) %>%
  filter(hex != "#000000")

idx <- unique(data_gg$id)

sample <- rerun(nrow(trees), sample(idx, 15))

r_sample <- accumulate(sample, ~c(.x, .y))

plot_data <- trees %>%
  mutate(r_s = r_sample,
         tree_plot = list(data_gg)) %>%
  mutate(alpha_tree = map2(tree_plot, r_sample, ~mutate(.x, hex = if_else(id %in% .y, alpha(hex, 0.1), hex)))) %>%
  select(year, alpha_tree) %>%
  unnest(c(alpha_tree))

labels <- tibble(year = seq(2015, 2210, 10),
                 label = as.character(year))

plot <- filter(plot_data, year %in% seq(2015, 2210, 10)) %>%
  ggplot(aes(x = x,y = y)) +
  geom_polygon(aes(fill = hex, group = id), show.legend = FALSE) +
  geom_text(data = labels, aes(x = 11, y = 17, label = label), family = "Anton", size = 8) +
  labs(x = NULL,
       y = NULL,
       title = "Unless someone like you cares a whole awful lot, nothing is going to get better. It's not.",
       subtitle = str_wrap("In a 2015 study, published in Nature, Thomas Crowther and colleagues mapped tree density across the world. They estimated that there were approximately 3.04 trillion trees in the world. The authors also estimated that over 15 billion trees are cut down each year, and the global number of trees has fallen by almost half (46%) since the start of human civilization.  Each dot on the trees below represent one billion trees. Each year 15 dots are faded out to illustrate the progressive loss. At this rate, our forests will evaporate within 200 years.", 100),
       caption = "**Data**: ourworldindata.org/forests | **Article**: doi.org/10.1038/nature14967 | **Graphic**: @jakekaupp") +
  scale_fill_identity() +
  coord_equal() +
  scale_y_reverse() +
  scale_x_continuous(limits = (c(-30,30))) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.6, family = "Anton"),
        plot.subtitle = element_text(hjust = 0.6, family = "Lato"),
        plot.caption = element_markdown(hjust = 0.6, family = "Lato"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

anim <- plot + transition_states(year,
                    transition_length = 20,
                    state_length = 1)

anim_save(here('day22', 'tdcc_day22.gif'), animation = anim + exit_fade(), renderer = gifski_renderer(loop = FALSE), width = 8, height = 8, units = 'in', res = 150)

