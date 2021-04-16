library(tidyverse)
library(here)
library(readxl)
library(ggtern)
library(tricolore)
library(jkmisc)
library(ragg)
library(janitor)
library(albersusa)
library(sf)
library(ggtext)
library(ragg)
library(magick)

education <- here("day15", "data", "Education.xls") %>%
  read_excel(skip = 4) %>%
  clean_names() %>%
  select(fips_code, state, 44:47) %>%
  set_names(c("fips", "state", "lt_hs", "hs", "college_associates", "bachelors_or_higher")) %>%
  mutate(hs_or_less = lt_hs + hs) %>%
  select(-lt_hs, -hs) %>%
  separate(fips, c("state_fips", "county_fips"), sep = 2) %>%
  mutate(across(c(college_associates:hs_or_less), ~.x/100))

tric_education <- Tricolore(education, p1 = 'hs_or_less', p2 = 'college_associates', p3 = 'bachelors_or_higher')

education <- mutate(education, rgb = tric_educ$rgb)

map_data <- counties_sf() %>%
  left_join(education, by = c("state_fips", "county_fips"))

legend <- tric_education$key +
  labs(L = 'HS-',
       T = 'College',
       R = 'Bachelors+',
       caption = "**Data**: USDA Economic Research Services | **Graphic**: @jakekaupp") +
  theme_jk(base_size = 10,
           markdown = TRUE) +
  theme(axis.title = element_markdown(size = 12),
        panel.border = element_rect(),
        plot.background = element_rect(fill = "transparent", color = "transparent"))


plot <- ggplot(map_data) +
  geom_sf(aes(fill = rgb, geometry = geometry), size = 0.1) +
  annotation_custom(ggplotGrob(legend), xmin = -38, xmax = -75, ymin = 20, ymax = 50) +
  scale_x_continuous(limits = c(-125, -40)) +
  scale_fill_identity() +
  labs(x = NULL,
       y = NULL,
       title = "Average Educational Attainment in the US by County 2015-2019",
       subtitle = str_wrap("Illustrated below in a choropleth map of the US is the average educational attainment by county.  Each county in the map is colored according to the proportions between the three levels of attainment: Some or Finished High School, College or an Associates Degree and a Bachelors Degree or Higher as shown on the inset ternary legend.  The legend also shows the distribution of each county as a dot as a position on each of the three scales.", 245)) +
  theme_jk(grid = FALSE,
           plot_title_family = "Oswald Bold",
           plot_title_size = 30,
           subtitle_size = 14,
           subtitle_family = "Barlow Condensed Light") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave(here('day15', "30dcc_day15.png"), plot, device = ragg::agg_png(), width = 16, height = 9)

altText::alt_text(plot)
