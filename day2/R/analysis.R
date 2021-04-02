library(tidyverse)
library(here)
library(jkmisc)
library(ragg)
library(ggfx)
library(glue)
library(altText)


ca_covid_data <- here("day2", "data", "vaccination-coverage-map.csv") %>%
  read_csv() %>%
  filter(week_end == last(week_end), prename != "Canada") %>%
  arrange(proptotal_atleast1dose) %>%
  mutate(remainder = 101-proptotal_atleast1dose) %>%
  pivot_longer(cols = c(proptotal_atleast1dose, remainder)) %>%
  mutate(name = factor(name, rev(c("proptotal_atleast1dose", "remainder"))))

order <- ca_covid_data %>%
  distinct(prename) %>%
  pull(prename)

ca_covid_data <- ca_covid_data %>%
  mutate(prename = factor(prename, order))

people <- tibble(prename = order) %>%
  mutate(values = list(1:100),
         parts =  rerun(13, sample(c(letters, LETTERS), 100, replace = TRUE))) %>%
  unnest(c(values, parts)) %>%
  mutate(prename = factor(prename, order))


plot <- ggplot(data = people) +
  as_reference(geom_text(data =  people, aes(y = prename, x = values, label = parts), color = "white", size = 10, family = "WeePeople", hjust = 0.5), id = "people") +
  with_blend(geom_col(data = ca_covid_data, aes(x = value, y = prename, fill = name, alpha = name), position = position_stack()),
                 bg_layer = "people",
                   blend_type = 'in') +
  geom_text(data = filter(ca_covid_data, name == "proptotal_atleast1dose"),  aes(x = 102, y = prename, label = scales::percent(value/100, 0.1)), family = "Bebas Neue", color = "#2274A5", size = 5, hjust = 0) +
  scale_x_continuous(limits = c(0, 108), expand = c(0.01,0), breaks = seq(0, 100, 20), labels = function(x) scale_percent_labels(x/100)) +
  scale_fill_manual(values = c("grey70", "#2274A5")) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(x = NULL,
       y = NULL,
       title = "Percentage of Population in Canadian Provices With At Least One COVID-19 Vaccine Dose",
       subtitle = glue("Pictogram style bar chart showing percentages of the population with at least {highlight_text('one COVID vaccine dose', '#2274A5', 'b')} as of March 27th, 2021."),
       caption = "**Data**: health-infobase.canada.ca/covid-19/vaccination-coverage | **Graphic**: @jakekaupp | **Font**: WeePeople by ProPublica") +
  theme_jk(grid = FALSE,
           markdown = TRUE,
           base_family = "Bebas Neue",
           plot_title_family = "Bebas Neue",
           plot_title_size = 26,
           base_size = 18) +
  theme(legend.position = "none",
        plot.title.position = "plot")

alt_text <- alt_text(plot)

ggsave(here("day2", "30dcc_day2.png"), plot, device = ragg::agg_png(), width = 16, height = 10, dpi = 300)
