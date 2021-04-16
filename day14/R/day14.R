library(tidyverse)
library(sf)
library(jsonlite)
library(ggsflabel)
library(nord)
library(colorspace)
library(ggforce)
library(here)
library(glue)
library(magick)

# The majority of the code for this is from the amazing post https://kimnewzealand.github.io/2019/02/21/celestial-maps/



milky_sf <- st_read("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/mw.json")

star_id <- fromJSON("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/stars.6.json")$features %>%
  pull(id)

stars_sf <- st_read("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/stars.6.json") %>%
  mutate(id = as.character(star_id))

star_names <- fromJSON("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/starnames.json")



star_names_full <- tibble(id = names(star_names)) %>%
  bind_cols(bind_rows(star_names))

stars_sf_named <- stars_sf %>%
  left_join(star_names_full)

stars_con_sf <- stars_sf_named %>%
  filter(name != "", c !="")

# Extract the brightest constellation stars with names, with an arbitrary cutoff 0.5 for mag - this extracts the top 10 brightest stars
stars_bright_sf <- stars_con_sf %>%
  filter(mag < 0.5)

# Change the mag to a new scale newmag for the size aesthetic
stars_bright_sf <- stars_bright_sf %>%
  mutate(newmag = -(mag-1.1)/4)


# Transform milky_sf with st_wrap_dateline
milky_sf_trans <- milky_sf %>%
  # These polygons have errrors with the st_wrap_dateline so add a cast to MULTILINESTRING then LINESTRING to break it up smaller
  st_cast("MULTILINESTRING") %>%
  st_cast("LINESTRING") %>%
  group_by(id)%>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
  ungroup()

# Convert the milky way back to MULTIPOLYGON so it will fill with grey in the plots, the outer od1 lines are on the 180 and -180 so we need to treat these separately using to the other lines which we can convert to MULTIPOLYGON
milky_sf_trans[3:202,] <- milky_sf_trans[3:202,] %>%
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON")

# Use concaveman R package https://github.com/joelgombin/concaveman or https://gis.stackexchange.com/questions/290170/convert-a-linestring-into-a-closed-polygon-when-the-points-are-not-in-order to create a polygon of the outer milky way
milky_sf_transclosed <- concaveman::concaveman(milky_sf_trans[1:2,])

# Transform stars_sf to Mollweide CRS
stars_sf<- st_transform(stars_sf, crs = "+proj=moll")

# Transform stars_bright_sf to Mollweide CRS
stars_bright_sf <- st_transform(stars_bright_sf, crs = "+proj=moll")

# Transform milky_sf_trans to Mollweide CRS
milky_sf_trans <- st_transform(milky_sf_trans, crs = "+proj=moll")

# Transform milky_sf_trans to Mollweide CRS
milky_sf_transclosed <- st_transform(milky_sf_transclosed, crs = "+proj=moll")

plot <- ggplot(stars_sf) +
  geom_sf(data = milky_sf_transclosed, alpha = 0.1, fill = "white", color = "white", size = 0.1) +
  geom_sf(data = milky_sf_trans, alpha = 0.6, aes(fill = id, color = id),  size = 0.1, show.legend = FALSE) +
  geom_sf(data = stars_sf, color = nord("snowstorm", 3)[3], size = 0.2, shape = 19) +
  geom_sf(data = stars_bright_sf, aes(size = newmag),  color = "#ffcd3c", show.legend = FALSE) +
  geom_sf_text_repel(data = stars_bright_sf, aes(label = name), nudge_x = -3, nudge_y = -1, color = "white", size = 8, family = "Bebas Neue") +
  scale_fill_nord(palette = "lumina") +
  labs(x = NULL,
       y = NULL,
       title = "Star Light, Star Bright The Ten Brighest Stars I See Tonight",
       subtitle = glue("Shown below is a celestial map of the stars and the milky way.  The {highlight_text('ten brighest stars', '#ffcd3c', 'b')} are labeled below, with their size scaled to their magnitude.  The colors and semi-transparent<br>trails represent the milky way."),
       caption = "**Data**: github.com/ofrohn/d3-celestial | **Graphic**: @jakekaupp with code from @Kim_Fitter") +
  scale_color_manual(values = darken(nord("lumina", 5))) +
  coord_sf(crs = st_crs("+proj=moll")) +
  theme_jk(dark = TRUE,
           grid = FALSE,
           markdown = TRUE,
           plot_title_family = "Bebas Neue",
           plot_title_size = 30,
           subtitle_family = "Oxygen")

ggsave(here("day14", "30dcc_day14.png"), plot, width = 13.5, height = 8.4375)

image_read(here("day14", "30dcc_day14.png")) %>%
  image_trim() %>%
  image_write(here("day14", "30dcc_day14.png"))

altText::alt_text(plot)


