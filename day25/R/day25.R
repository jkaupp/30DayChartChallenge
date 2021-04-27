library(tidyverse)
library(here)
library(jkmisc)
library(rvest)
library(janitor)
library(ggtext)
library(glue)


cdn_demo <- read_html("https://en.wikipedia.org/wiki/Demographics_of_Canada") %>%
  html_nodes("table.wikitable:nth-child(1)") %>%
  html_table() %>%
  flatten_df() %>%
  clean_names() %>%
  mutate(across(c(not_a_visible_minority:percent_visible_minority), ~parse_number(.x))) %>%
  mutate(percent_visible_minority = percent_visible_minority/100,
         percent_non_visible_minority = 1 - percent_visible_minority) %>%
  mutate(province_territory = str_remove_all(province_territory, "\\[\\d+\\]"))

order <- cdn_demo %>%
  filter(province_territory != "Canada") %>%
  arrange(desc(percent_non_visible_minority)) %>%
  pull(province_territory)

plot_data <- select(cdn_demo, province_territory, percent_visible_minority, percent_non_visible_minority) %>%
  filter(province_territory != "Canada") %>%
  pivot_longer(c(percent_visible_minority, percent_non_visible_minority)) %>%
  mutate(province_territory = factor(province_territory, order))

canada <- cdn_demo %>%
  filter(province_territory == "Canada")


plot <- ggplot(plot_data, aes(x = value, y = province_territory, fill = name)) +
  geom_col(show.legend = FALSE) +
  geom_vline(data = canada, aes(xintercept = percent_visible_minority), color = "#625F63", linetype = "dashed") +
  geom_text(aes(x = 1, label = paste0(province_territory, " ")), hjust = 1, family = "Lato") +
  labs(x = NULL,
       y = glue("<span style='font-family:Anton;font-size:30pt;color:#4d4d4d;'>Canada: Mosaic or Melting Pot?</span><br><br><br>
       <span style='font-family:Lato;font-size:14pt;color:#4d4d4d'>Canada has a reputation as a cultural mosaic, where diverse groups have maintained their distinctiveness while functioning as part of the whole, while American culture is referred to  melting pot, where peoples of diverse origins have allegedly fused to make a new people. Despite the glowing description, Canada still has a **lot** of work to do in getting to a truly diverse and accepting Nation.<br>
       <br>Shown on the right is a stacked bar chart showing the percentage population of <span style='font-family:Lato;font-size:14pt;color:#FDB833'>**visible minorities**</span> and <span style='font-family:Lato;font-size:14pt;color:#1789FC'>**non-visible minorities**</span> by Province from the 2016 Canadian Census with the dashed line representing the **National average of 22.3%**.  The larger provinces in Western and Central Canada have a much more diverse population than the smaller Atlantic provinces.</span><br><br><br>
                <span style='font-family:Lato;font-size:8pt;'>**Data**: 2016 Canadian Census | **Graphic**: @jakekaupp</span>")) +
  scale_fill_manual(values = c("#1789FC", "#fdb833")) +
  scale_y_discrete(position = "left") +
  scale_x_continuous(labels = scale_percent_labels, expand = c(0,0.01)) +
  theme_jk(base_family = "Bebas Neue",
           base_size = 14) +
  theme(plot.background = element_rect(fill = "#E8EBF7", color = NA),
        axis.text.y = element_blank(),
        axis.title.y = element_textbox_simple(width = unit("3", "in"), vjust = 0.95))

ggsave(here("day25", "tdcc_day25.png"), plot, width = 12, height = 6.2, device = ragg::agg_png())
