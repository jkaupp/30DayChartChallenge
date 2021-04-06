library(tidyverse)
library(here)
library(jkmisc)
library(janitor)
library(pBrackets)
library(glue)
library(ggtext)
library(altText)

emissions_data <- here("day5", "data", "annual-co-emissions-by-region.csv") %>%
  read_csv() %>%
  clean_names()


entity_data <- filter(emissions_data,
                    entity %in% c("South America",
                                  "Oceania",
                                  "Asia (excl. China & India)",
                                  "China",
                                  "India",
                                  "Africa",
                                  "North America (excl. USA)",
                                  "United States",
                                  "EU-27",
                                  "Europe (excl. EU-27)",
                                  "International transport"
                                  ),
                    year %in% c(1900, 1999))

avg_emissions <- entity_data %>%
  group_by(year) %>%
  summarize(across(annual_co2_emissions, mean),
            entity = "Global Average")

plot_data <- bind_rows(entity_data, avg_emissions) %>%
  group_by(year) %>%
  mutate(color = if_else(annual_co2_emissions < mean(annual_co2_emissions), FALSE, TRUE)) %>%
  group_by(entity) %>%
  mutate(color = if_else(any(color), "#F2AF29", "grey60")) %>%
  mutate(color = if_else(entity == "Global Average","#CA1551" , color))

# https://stackoverflow.com/questions/35633239/add-curly-braces-to-ggplot2-and-then-use-ggsave
# Credit to one of the masters of grid graphics @baptiste
bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l


  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}



b1 <- bracketsGrob(x1 = 0.805, y1 = 0.115, x2 = 0.805, y2 = 0.22, h = -0.025,  lwd = 1, col = "grey60")

bottom_labels <- plot_data %>%
  filter(color == 'grey60', year == 1999) %>%
  arrange(desc(annual_co2_emissions)) %>%
  group_by(year) %>%
  summarize(entity = paste0(entity, collapse = "\n"),
            annual_co2_emissions = 700000000,
            color = "grey60")


plot <- ggplot(plot_data, aes(x = year, y = annual_co2_emissions/1000000000, color = color)) +
  geom_point() +
  geom_path(aes(group = entity)) +
  geom_text(data = filter(plot_data, year == 1999, color != "grey60"), aes(label = entity, x = 2001), hjust = 0, family = "Bebas Neue", size = 5) +
  geom_text(data = bottom_labels, aes(label = entity, x = 2005), hjust = 0, family = "Bebas Neue", size = 5) +
  annotate("text", x = 1900, y = 1:6, label = c(1:5, "6 billion tonnes"), family = "Anton", vjust = 0, hjust = 0, size = 6, color = alpha("#FAFAFF", 0.1)) +
  annotate("text", x = 1900, y = -0.1, label = "1900", family = "Anton", vjust = 1, hjust = 0, size = 10, color = alpha("#FAFAFF", 0.1)) +
  annotate("text", x = 1999, y = -0.1, label = "1999", family = "Anton", vjust = 1, hjust = 1, size = 10, color = alpha("#FAFAFF", 0.1)) +
  annotation_custom(b1) +
  labs(x = NULL,
       y = NULL,
       title = "A Century of Rapid Carbon Dioxide Emissions Growth",
       subtitle = glue("Illustrated below in a slopegraph is the rapid rise in carbon dioxide emissions across regions/countries that are {highlight_text('above', '#F2AF29', 'b')} or {highlight_text('below', 'grey60', 'b')} the {highlight_text('global average', '#CA1551', 'b')} from 1900 to 1999."),
       caption = "**Data**: ourworldindata.org/co2-and-other-greenhouse-gas-emissions | **Graphic**: @jakekaupp") +
  scale_x_continuous(breaks = c(1900, 1999), limits = c(1900, 2020)) +
  scale_y_continuous(breaks = 0:6) +
  scale_color_identity() +
  theme_jk(grid = "XY",
           plot_title_family = "Anton",
           plot_title_size = 25,
           markdown = TRUE) +
  theme(legend.position  = "none",
        plot.title = element_markdown(color = "#FAFAFF"),
        plot.subtitle = element_markdown(color = "#FAFAFF"),
        plot.caption = element_markdown(color = alpha("#FAFAFF", 0.1)),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(color = alpha("#FAFAFF", 0.2), linetype = "dashed"),
        plot.background = element_rect(fill = "#30343F", color = "#30343F"))

ggsave(here('day5', '30dcc_day5.png'), plot, width = 12.5, height = 10, device = ragg::agg_png())

alt_text <- alt_text(plot)
