library(tidyverse)
library(rnaturalearth)
library(sf)
library(here)
library(readxl)
library(janitor)
library(biscale)
library(jkmisc)
library(ggtext)
library(glue)
library(cowplot)


happiness <- here("day30", "data", "WHR20_DataForTable2.1.xls") %>%
  read_excel() %>%
  clean_names() %>%
  select(year, country_name, life_ladder, sd_mean  = standard_deviation_mean_of_ladder_by_country_year) %>%
  filter(country_name != "Russia")

europe <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf', continent="Europe") %>%
  st_crop(xmin = -13, xmax = 45, ymin = 30, ymax = 73) %>%
  mutate(sovereignt = case_when(str_detect(sovereignt, "Serbia") ~ "Serbia",
                                TRUE ~ sovereignt))

eu_happy <- left_join(europe, happiness, by = c("sovereignt" = "country_name"))

plot_data <- eu_happy %>%
  group_split(year) %>%
  .[-16] %>%
  map_dfr(~bi_class(.x,  x = life_ladder, y = sd_mean, style = "quantile", dim = 3)) %>%
  filter(year %in% 2007:2019)

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Higher Happiness ",
                    ylab = "Higher Uncertainty ",
                    size = 8) +
  theme(plot.background = element_rect(fill = "#F9FBF2", color = NA),
        panel.background = element_rect(fill = "#F9FBF2", color = NA),
        axis.title = element_text(family = "Lato"))


map <- ggplot(plot_data) +
  geom_sf(data = europe, fill = "grey88", alpha = 0.5, color = "white") +
  geom_sf(mapping = aes(fill = bi_class), show.legend = FALSE, color = "grey88", size = 0.1) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(x = NULL,
       y = glue("<span style='font-family:Anton;font-size:30pt;color:#4d4d4d;'>Is Europe a Happy Place?</span><br><br><br>
       <span style='font-family:Lato;font-size:14pt;color:#4d4d4d'>Shown as a series of maps on the right is the life ladder score for European Countries from 2008 to 2019. The survey asks respondents to evaluate their current life as a whole using the image of a ladder, with the best possible life for them as a 10 and worst possible as a 0 and Weighted averages are used to construct population- representative national averages for each year in each country. These averages are presented on bivarate scale against the standard error, essentially providing contrast of uncertainty in the measurement.<br><br>
                <span style='font-family:Lato;font-size:10pt;'>**Data**: World Happiness Report | **Graphic**: @jakekaupp</span>")) +
  facet_wrap(~year, nrow = 2) +
  theme_jk(grid = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_textbox_simple(width = unit("3", "in"), vjust = -0.1),
        strip.text = element_text(family = "Anton", size = 30, color = "#4d4d4d"),
        plot.background = element_rect(fill = "#F9FBF2", color = NA))

plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, x = 0.78,  y = .15, width = 0.3, height = 0.3)

ggsave(here('day30', 'tdcc_day30.png'), plot, device = ragg::agg_png(), width = 16, height = 5)

