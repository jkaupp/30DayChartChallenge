library(tidyverse)
library(fs)
library(rtweet)
library(lubridate)
library(jkmisc)
library(ggforce)
library(glue)
library(here)
library(textclean)

# Function to get lines of scripts
script_length <- function(file_path){
  file <- readr::read_file(file_path)
  file_lines <- file %>% stringr::str_split("\n")
  first_character_of_lines <- file_lines %>%
    lapply(function(line)stringr::str_replace_all(line," ",""))  %>%
    lapply(function(line)stringr::str_sub(line,1,1)) %>%
    unlist
  sum(first_character_of_lines != "#" & first_character_of_lines != "\r")
}

tidy_tuesday_scripts <- dir_ls("/Users/jake/Projects/R/tidytuesdays", recurse = 4, regexp = "\\.R$")

tidy_tuesday_images <- dir_ls("/Users/jake/Projects/R/tidytuesdays", recurse = 4, regexp = "tw\\d+_plot.png$")

tt_datasets <- read_tsv("https://raw.githubusercontent.com/nsgrantham/tidytuesdayrocks/master/data/datasets.tsv")

tt_tweets <- read_tsv("https://raw.githubusercontent.com/nsgrantham/tidytuesdayrocks/master/data/tweets.tsv")

my_tweets <- tt_tweets %>%
  filter(screen_name == "jakekaupp") %>%
  left_join(tt_datasets) %>%
  filter(dataset_id != "x", year(created_at) == 2019) %>%
  group_by(dataset_id, dataset_name) %>%
  summarize(favorite_count = sum(favorite_count))

tt_2019_images <- tibble(path = tidy_tuesday_images) %>%
  mutate(year = as.numeric(str_extract(path, "[0-9]{4}")),
         week = as.numeric(str_extract(path, "(?<=week)\\d+"))) %>%
  arrange(year, week) %>%
  filter(year == 2019) %>%
  mutate(dataset_id = as.character(seq(39, 90))) %>%
  select(dataset_id, img_path = path)

tt_2019 <- tibble(path = tidy_tuesday_scripts) %>%
  mutate(lines = map_int(path, script_length),
         year = as.numeric(str_extract(path, "[0-9]{4}")),
         week = as.numeric(str_extract(path, "(?<=week)\\d+"))) %>%
  arrange(year, week) %>%
  filter(year == 2019, basename(path) == "analysis.R") %>%
  mutate(dataset_id = as.character(seq(39, 90))) %>%
  inner_join(my_tweets) %>%
  inner_join(tt_2019_images, by = "dataset_id")


r <- cor(tt_2019$lines, tt_2019$favorite_count)

lines <- tt_2019 %>%
  summarize(across(c(lines, favorite_count), mean))


annotations <- slice_max(tt_2019, lines, n = 2) %>%
  bind_rows(slice_max(tt_2019, favorite_count, n = 2)) %>%
  mutate(dataset_name = replace_non_ascii(dataset_name))

plot <- ggplot(tt_2019, aes(x = lines, y = favorite_count)) +
  geom_point() +
  geom_hline(data = lines, aes(yintercept = favorite_count), linetype = "dashed", color = "grey60") +
  geom_vline(data = lines, aes(xintercept = lines), linetype = "dashed", color = "grey60") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", size = 0.5) +
  geom_mark_circle(data = annotations, aes(label = glue("Week {week}: {dataset_name}"), description = glue("{lines} lines of code, {favorite_count} likes"), group = dataset_id), label.family = c('Oswald Bold', "Oswald Light")) +
  labs(x = NULL,
       y = NULL,
       title = "The Relationship Beteen Code and Popularity #TidyTuesday Submisssions",
       subtitle = "Shown below is a scatterplot of Twitter likes against lines of code illustrating the relationship between the lines of code written for my<br>52 #TidyTuesday submissions in 2019.",
       caption = "**Data**: github.com/jkaupp | **Graphic**: @jakekaupp") +
  scale_x_continuous(limits = c(0, 250)) +
  annotate("text", x = 30, y = 30, label = glue("r = {round(r, 3)}"), color = "firebrick", family = "Oswald Bold", size = 4, hjust = 1) +
  theme_jk(grid = "XY",
           markdown = TRUE) +
  theme(plot.title.position = "plot")

ggsave(here("day13", "30dcc_week13.png"), plot, device = ragg::agg_png(), width = 10, height = 10)

