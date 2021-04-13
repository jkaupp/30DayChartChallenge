library(tidyverse)
library(here)
library(jkmisc)
library(janitor)
library(ggtext)

diseases <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/diseases.csv")

the_disease <- "Measles"

plot_data <- diseases %>%
  filter(state %notin% c("Hawaii","Alaska"), disease == "Measles") %>%
  mutate(rate = round(count / (population/100000), digits = 2)) %>%
  mutate(state = reorder(state, desc(state)))

vaccines <- tibble(disease = "Measles",
                   x =  1963,
                   xend = x,
                   y = 1,
                   yend = 50)

plot <- ggplot(plot_data, aes(x = year, y = state, fill = rate)) +
  geom_tile(color = "white", size = 0.35) +
  geom_segment(data = vaccines, aes(x = x, xend = xend, y = y, yend = yend), color = "black", size = 0.5, inherit.aes = FALSE) +
  annotate(geom = "text", x = 1963.2, y = 52, label = "Vaccine introduced", size = 5, hjust = 0, family = "Oswald Light") +
  scale_x_continuous(expand = c(0,0), breaks = c(1930, 1950, 1970, 1990, 2010)) +
  labs(x = NULL,
       y = NULL,
       title = "Vaccines Are a Hell of a Drug",
       subtitle = "Since its introduction in 1963, the measles vaccine emphatically reduced the amount of cases\nacross the continental United States nearly eradicating the disease.",
       caption = "**Data**: CDC | **Original**: WSJ  | **Graphic**: @jakekaupp") +
  scale_fill_gradientn("Cases per 1000",
                       colours = c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a", "#e29421", "#e29421", "#f05336", "#ce472e"),
                       values = c(0, 0.01, 0.02, 0.03, 0.09, 0.1, 0.15, 0.25, 0.4, 0.5, 1),
                       breaks = seq(0, 3000, 1000),
                       labels = c("0", "1k", "2k","3k"),
                       limits = c(0, 3000),
                       na.value = "white",
                       guide = "colorbar") +
  guides(fill = guide_colorbar(title.position = "top")) +
  theme_jk(grid = FALSE,
           base_size = 14) +
  theme(axis.text.y = element_blank(),
        legend.position = c(0.1, 0.9),
        legend.direction = "horizontal",
        legend.key.width = unit(6, "mm"),
        legend.key.height = unit(2, "mm"),
        plot.caption = element_markdown()) +
  coord_polar()

ggsave(here("day11", "30dcc_day11.png"), plot, width = 8, height = 8)

altText::alt_text(plot)
