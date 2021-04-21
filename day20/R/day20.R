library(tidyverse)
library(tidyquant)
library(lubridate)
library(patchwork)
library(colorspace)
library(ggforce)
library(here)
library(glue)
library(ggtext)

gme <- tq_get("GME",
              from = '2020-12-01',
              to = "2021-04-01",
              get = "stock.prices")

labels <- tibble(date = c("2020-12-08",
                          "2021-01-11",
                          "2021-01-13",
                          "2021-01-22",
                          "2021-01-27",
                          "2021-01-28",
                          "2021-01-29",
                          "2021-01-31",
                          "2021-02-02"
                          ),
                 date_label = c("2020-12-08",
                                "2021-01-01",
                                "2021-01-13",
                                "2021-01-22",
                                "2021-01-27",
                                "2021-01-28",
                                "2021-01-29",
                                "2021-01-31",
                                "2021-02-02"
                 ),
                 label = c("GameStop reports dismal earnings, stock takes a tumble",
                           "GameStop appoints 3 new board directors",
                           "Stock surges more than 50%",
                           "Stock again surges 50%",
                           "Major short sellers close at a significant loss",
                           "Robinhood and other platforms restrict transactions for GME, lawmakers react",
                           "SEC weighs in, trading platforms re-allow most GME transactions",
                           "'Deadline' reports a Hollywood movie deal",
                           "GME falls, all eyes on what comes next")) %>%
  mutate(date = as_date(date)) %>%
  mutate(date_label = as_date(date_label))

annotations <- inner_join(gme, labels)


plot <- ggplot(gme, aes(x = date, y = adjusted)) +
  geom_path(color = "#FD0000", size = 0.5, lineend = "square", linejoin = "mitre") +
  geom_ribbon(aes(x = date, ymin = 0, ymax = adjusted), fill = "#FD0000", alpha = 0.7, size = 0) +
  geom_point(data = annotations, aes(x = date_label), color = "#FD0000", size = 2) +
  geom_mark_circle(data = annotations, aes(label = date, x = date_label, description = label), label.buffer = unit(30, "mm"), label.family = c("Alegreya Sans Bold", "Alegreya Sans Regular"), label.minwidth = unit(60, 'mm'), label.fontsize = c(16, 12)) +
  annotate(GeomRichtext, x = as_date("2020-12-02"), y = 380, label = glue("THE {highlight_text('GAME', '#FD0000', 'b',size = 112)}STOP SAGA"), size = 30, family = "Anton", hjust = 0, vjust = 0, label.size = 0, fill = NA) +
  annotate(GeomTextBox, x = as_date("2020-12-02"), y = 380, label = "The wild ride of Hedge Funds, r/WallStreetBets and RobinHood shown as an annotated timeline of the early rise and fall of the stock price of GameStop (GME)", size = 8, family = "Alegreya Sans Regular", hjust = 0, vjust = 1, fill = NA, box.size = 0, width = unit(9.5, "inch")) +
  scale_y_continuous(limits = c(0, 450), expand = c(0,0), breaks = seq(0, 400, 100), labels = c("", "100", "200", "300", "$400/share")) +
  scale_x_date(date_breaks = "months",
               date_labels = "%Y",
               expand = c(0,0)) +
  labs(x = NULL,
       y = NULL,
       caption = "**Data**: Yahoo Finance | **Graphic**: @jakekaupp | **Headlines**: Reuters & ABC News") +
  theme_jk(caption_family = "Alegreya Sans Light",
           base_family = "Anton",
           markdown = TRUE)

ggsave(here("day20", "tdcc_day20.png"), plot, device = ragg::agg_png(), width = 24, height = 12)

