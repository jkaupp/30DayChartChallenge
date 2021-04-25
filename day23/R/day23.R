library(here)
library(tidyverse)
library(glue)
library(jkmisc)
library(viridisLite)
library(sf)
library(tilemaps)
library(ggtext)
library(magick)


canada_sf <- here("day23", "data", "lpr_000a16a_e.shp") %>%
  read_sf()

house_prices <- tibble(PRENAME = canada_sf$PRENAME,
                       `2021` = c(282600, 330121, 362998, 253870, 435516, 890035, 338458, 278400, 443000, 945936, 497187, 438580, NA),
                       `2020` = c(264500, 270905, 286397, 192348, 340418, 685430, 301369, 255800, 381160, 787032, 400787, 450249, NA))

canada_tile <- canada_sf %>%
  left_join(house_prices) %>%
  mutate(yoy = (`2021` - `2020`)/`2020`) %>%
  mutate(tile_map = generate_map(geometry, square = FALSE, flat_topped = TRUE),
         buffered = st_buffer(tile_map, 0.5)) %>%
  mutate(text_color = if_else(yoy <= 0.1 & !is.na(yoy), "white", "grey30"))


tile_map <- ggplot(canada_tile) +
  geom_sf(aes(geometry = tile_map), color = "grey30", size = 6) +
  geom_sf(aes(geometry = tile_map, fill = yoy), color = "white", size = 2) +
  geom_sf_text(aes(geometry = tile_map, label = PRENAME,  color = text_color), family = "Anton", size = 4,
               fun.geometry = function(x) st_centroid(x)) +
  scale_fill_viridis_b("Year over Year %", option = "cividis", na.value = "grey99", labels = scales::percent) +
  labs(x = NULL,
       title = NULL,
       subtitle = NULL,
       caption = NULL,
       y = glue("<span style='font-family:Anton;font-size:60pt;'>Houses, So Hot Right Now</span><br><br>
                <span style='font-family:Lato;font-size:18pt;'>Show below in a hexagonal tile map are the year over year pecentage change in housing prices in Canada in 2020-2021. In all but one provinces, you're going to be paying 30% more than last year on average.</span>
                <br><br><br><br>
                <span style='font-family:Lato;font-size:12pt;'>**Data**: The Canadian Real Estate Association | **Graphic**: @jakekaupp</span>")) +
  guides(fill = guide_bins(title.position = "top", direction = "horizontal", keywidth = unit(10, "mm"))) +
  scale_color_identity() +
  theme_jk(grid = FALSE,
           base_family = "Bebas Neue",
           base_size = 14) +
  theme(legend.position = c(0.72, 0.85),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_blank(),
        plot.title = element_blank(),
        legend.title = element_text(family = "Anton"),
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"),
        axis.title.y = element_textbox_simple(width = unit(5, "in")))

ggsave(here("day23", "tdcc_day23.png"), tile_map, width = 16, height = 8, device = ragg::agg_png())


