library(tidyverse)
library(jkmisc)
library(here)
library(ggforce)
library(colorspace)
library(ggdist)
library(distributional)
library(glue)

student_mat <- here("day27", "data", "student-mat.csv") %>%
  read_delim(delim = ";") %>%
  rename(G1_math = G1,
         G2_math = G2,
         G3_math = G3)

student_por <- here("day27", "data", "student-por.csv") %>%
  read_delim(delim = ";") %>%
  rename(G1_por = G1,
         G2_por = G2,
         G3_por = G3)

final <- inner_join(student_mat, student_por, by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

plot_data <- select(final,matches("G3"), Medu, Fedu) %>%
  mutate(idx = row_number()) %>%
  rowwise() %>%
  mutate(edu = max(c(Medu, Fedu))) %>%
  select(-Medu, -Fedu) %>%
  pivot_longer(c(-idx, -edu)) %>%
  filter(!is.na(edu) | !is.na(value)) %>%
  mutate(edu = factor(edu, 0:4, c('No Education', "<=4th Grade", "5th-9th Grade", "Secondary Education", "Higher Education"))) %>%
  separate(name, into = c("item", "subject"), sep = "_") %>%
  mutate(subject = factor(subject, c("math", "por"),  c("Math", "Portuguese")))

errors <- plot_data %>%
  group_by(subject, edu) %>%
  summarize(error = sd(value)/sqrt(mean(value)),
            sd = sd(value),
            moe = sd*1.96,
            mean = mean(value),
            high = mean + error,
            low = mean - error)


plot <- ggplot(plot_data) +
  geom_sina(aes(x = edu , y = value), color = "#2B2D42", size = 3, alpha = 0.5) +
  stat_dist_gradientinterval(data = errors, aes(dist = dist_normal(mean, sd), x = edu, y = mean), interval_color = "#B33F62") +
  geom_point(data = errors, aes(x = edu,y = mean), fill = "#B33F62", size = 4, shape = 21, color = darken("#B33F62")) +
  facet_wrap(~subject) +
  labs(x = NULL,
       y = NULL,
       title = toupper("Does Parental Educational Attainment Affect Student Performance?"),
       subtitle = glue("Shown below are the distribution of final grades by the highest level of parental education and the {highlight_text('mean and confidence interval', '#B33F62', 'b', 21)}  for each group. This data was<br>collected from a Math and Portugeuse Language class in 2005-2006 and students were graded on the standard 20 point scale."),
       caption = "**Data**: P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance.Proceedings of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7. | **Graphic**: @jakekaupp") +
  theme_jk(grid = "XY",
           plot_title_family = "Anton",
           plot_title_size = 30,
           subtitle_size = 16,
           base_family = "Anton",
           base_size = 16,
           strip_text_family = "Anton",
           strip_text_size = 20,
           markdown = TRUE) +
  theme(plot.background = element_rect(fill = "#EDF2F4", color = NA))

ggsave(here('day27', "tdcc_day27.png"), plot, width = 16, height = 8, device = ragg::agg_png())

altText::alt_text(plot)
