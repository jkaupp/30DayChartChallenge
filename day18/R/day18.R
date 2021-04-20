library(tidyverse)
library(here)
library(tidygraph)
library(rgexf)
library(ggraph)
library(graphlayouts)
library(colorspace)
library(ggtext)

meta_data <- read_tsv(here('day18', 'data', 'network_metadata.tab'))

fg_id <- meta_data %>%
  filter(Title == "The Apartment") %>%
  pull(GexfID)

network <- here("day18", "data", "gexf", paste0(fg_id, ".gexf")) %>%
  read.gexf() %>%
  gexf.to.igraph() %>%
  as_tbl_graph()

network <- network %>%
  activate(nodes) %>%
  mutate(name = if_else(name == "BUD", "", name))

layout <- create_layout(network, 'focus', focus = node_is_center())

ring_layout <- layout %>%
  mutate(angle = node_angle(x, y),
         distance = if_else(distance > 0, distance + 0.15, distance),
         distance = if_else(name %in% c('SYLVIA'), distance + 0.05, distance),
         distance = if_else(name %in% c('BLONDE'), distance - 0.05, distance),
         x_new = distance * cos(angle * pi/180),
         y_new = distance * sin(angle * pi/180))


plot <- ggraph(network, 'focus', focus = node_is_center()) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = r), data.frame(r = 1:2), colour = 'grey', size = 0.4) +
  geom_edge_link(width = 0.7, alpha = 0.3, color = "#88C0D0") +
  geom_node_point(aes(fill = color, size = size), shape = 21) +
  geom_text(data = ring_layout, aes(x = x_new, y = y_new, label = name), size = 6, family = "Staatliches", color = lighten("#ECEFF4"), hjust = "outward") +
  annotate("text", x = 0, y = 0, label = "BUD", size = 20, family = "Anton", color = lighten("#ECEFF4")) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_area(max_size = 15) +
  scale_x_continuous(limits = c(-3, 3)) +
  labs(x = NULL,
       y = NULL,
       title = "A Bud-Centric View of the Characters and Their Connections in the 1960 Grammy Award Winning Film 'The Apartment'",
       subtitle = "Each character and their connection to Bud are visualized as a network in a focused layout.  The color of each node differentiates the four different communities identified through analysis",
       caption = "**Data**: Kaminski, Jermain; Schober, Michael; Albaladejo, Raymond; Zastupailo, Oleksandr; Hidalgo, César, 2018, 'Moviegalaxies - Social Networks in Movies', doi.org/10.7910/DVN/T4HBA3, Harvard Dataverse, V3 | **Graphic**: @jakekaupp") +
  theme_jk(dark = TRUE,
           grid = FALSE,
           plot_title_family = "Anton",
           plot_title_size = 26) +
  theme(legend.position = "none",
        plot.caption = element_markdown(hjust = 0.5 ),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()



ggsave(here("day18", "tdcc_day18.png"), plot, width = 18, height = 18, device = ragg::agg_png())
