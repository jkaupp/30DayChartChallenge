library(tidyverse)
library(here)
library(jkmisc)
library(readxl)
library(janitor)
library(colorspace)
library(scales)
library(ggtext)
library(glue)

parker <- read_xlsx(here('day29', 'data', 'ParkerElephantData.xlsx')) %>%
  clean_names()

plot_data <- select(parker,
       id = animal_or_elephant_id_number,
       sex,
       age = age_in_years_if_noted_as_juvenile_est_mos_foetus_etc_enter_zero_here_and_use_note_field_below_to_transcribe,
       s_height = s_height_st_shoulder_height_straight_in_inches,
       length = total_body_length_inches,
       total_weight,
       live_weight,
       tusks_weight_right,
       tusks_weight_left) %>%
  mutate(combined_tusk_weight = tusks_weight_right + tusks_weight_right) %>%
  mutate(combined_tusk_weight = combined_tusk_weight/1000,
         s_height = s_height * 0.025) %>%
  filter(sex %in% c("Male", "Female")) %>%
  mutate(stip_text = spaced_title(sex),
    strip_text = highlight_text(stip_text, if_else(sex == "Male", "#331832", "#D81E5B"),'b', 50))

deviations <- plot_data %>%
  select(sex, combined_tusk_weight, s_height) %>%
  group_by(sex) %>%
  summarize(across(everything(), list("sd" = ~sd(.x, na.rm = TRUE), "mean" = ~mean(.x, na.rm = TRUE)))) %>%
  mutate(label = glue("**Mean Shoulder Height** : {round(s_height_mean, 2)} +/- {round(s_height_sd, 2)}m<br>**Mean Combined Tusk Weight** : {round(combined_tusk_weight_mean, 2)} +/- {round(combined_tusk_weight_sd, 2)}kg"))

labels <- plot_data %>%
  distinct(sex, strip_text) %>%
  mutate(x = 25, y = 1.5)

main <- ggplot(plot_data, aes(x = combined_tusk_weight, y = s_height, color = sex, fill = sex)) +
  geom_point(shape = 21, show.legend = FALSE, alpha = 0.5, size = 3) +
  theme_jk(grid = "XY") +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(0, 125, 25), labels = c(seq(0, 100, 25), "125kg"), position = "top") +
  scale_y_continuous(limits = c(1, 3.5), breaks = 1:3, labels = c(1, 2, "3m")) +
  scale_fill_manual(values = c("Male" = "#331832",
                                "Female" = "#D81E5B")) +
  scale_color_manual(values = c("Male" = darken("#331832"),
                                "Female" = darken("#D81E5B"))) +
  theme(plot.background = element_rect(fill = "grey88", color = NA))

facets <- ggplot(plot_data, aes(x = combined_tusk_weight, y = s_height, color = sex, fill = sex)) +
  geom_errorbar(data = deviations, aes(x = combined_tusk_weight_mean, ymin = s_height_mean - s_height_sd, ymax = s_height_mean + s_height_sd, color = sex), inherit.aes = FALSE,  size = 0.8) +
  geom_errorbar(data = deviations, aes(xmin = combined_tusk_weight_mean - combined_tusk_weight_sd, xmax = combined_tusk_weight_mean + combined_tusk_weight_sd, y = s_height_mean, color = sex), inherit.aes = FALSE, size = 0.8) +
  geom_point(shape = 21, show.legend = FALSE, alpha = 0.2, size = 2) +
  geom_richtext(data = labels, aes(x = x, y = y, label = strip_text), family = "Anton", size = 50, show.legend = FALSE,  label.size = unit(0, "mm"), fill = NA, vjust = 0, hjust = 0) +
  geom_richtext(data = deviations, aes(x = 25, y = 1.45, label = label), family = "Lato", show.legend = FALSE,  label.size = unit(0, "mm"), fill = NA, vjust = 0, hjust = 0) +
  theme_jk(grid = FALSE) +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(0, 125, 25), labels = c(seq(0, 100, 25), "125kg"), expand = c(0,0)) +
  scale_y_continuous(limits = c(1, 3.5), breaks = 1:3, labels = c(1, 2, "3m"),  expand = c(0,0)) +
  scale_fill_manual(values = c("Male" = "#331832",
                               "Female" = "#D81E5B")) +
  scale_color_manual(values = c("Male" = darken("#331832"),
                                "Female" = darken("#D81E5B"))) +
  theme(plot.background = element_rect(fill = "transparent", color = NA)) +
  facet_wrap(~sex) +
  theme(strip.text = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

plot <- main +
  annotation_custom(ggplotGrob(facets), xmin = 40, xmax = 140, ymin = 0.915, ymax = 2.5) +
  labs(title = toupper("Gender Differences in Height and Combined Tusk Weight of East African Elephants"),
       subtitle = "Illustrated below in a scatterplot is the relationship between shoulder height and combined tusk weight in East African Elephants. This data is from the Parker Elephant Data Sheets, which are field data compiled during culling (herd thinning)\noperations intended to mitigate elephant overpopulation in 1965-1969 at environmentally stressed sites: Murchison Falls National Park, Budongo Forest, Tsavo, and Mkomasi recorded by Ian Parker.",
       caption = "**Data**: Parker Elephant Data Sheets library.ufl.edu/spec/manuscript/guides/parker.htm | **Graphic**: @jakekaupp") +
  theme(plot.caption = element_markdown(),
        plot.title.position = "plot",
        plot.title = element_text(family = "Anton", size = 40))

ggsave(here("day29", "tdcc_day29.png"), plot, width = 18.4, height = 10, device = ragg::agg_png())

altText::alt_text(plot)
