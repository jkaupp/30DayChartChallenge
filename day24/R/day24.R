library(tidyverse)
library(jkmisc)
library(here)
library(colorspace)
library(claremontrun)
library(ggstream)
library(ggtext)
library(glue)

set.seed(649)

levels <- c("depicted", "speech", "thought", "narrative")

claremont_byrne <- tibble(character = c("Colossus = Peter (Piotr) Rasputin", "Cyclops = Scott Summers",
                                        "Marvel Girl/Phoenix = Jean Grey", "Nightcrawler = Kurt Wagner",
                                        "Storm = Ororo Munroe", "Wolverine = Logan")) %>%
  rowwise() %>%
  mutate(color = if_else(str_detect(character, "Nightcrawler"), "#000000", sample(grey.colors(100, start = 0.3, end = 0.8), 1)))


streams <- character_visualization %>%
  inner_join(claremont_byrne) %>%
  group_by(character, issue, color) %>%
  summarize(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(speech:depicted) %>%
  mutate(name = factor(name, levels))

vlines <- tibble(x = c(97, seq(125, 250, by = 25), 280))

labels <- tibble(issue = 250,
                 value = c(-40, -40, -20, -9),
                 name = factor(levels, levels = levels),
                 label = c("Depicted", "Speech", "Thought", "Narrative"))


out <- ggplot(streams, aes(x = issue, y = value, fill = color, group = character)) +
  geom_stream(method = "new_wiggle", extra_span = 0.1, true_range = "none") +
  geom_stream(aes(color = darken(color)), geom = "contour", method = "new_wiggle", extra_span = 0.1, true_range = "none") +
  geom_vline(data = vlines, aes(xintercept = x), color = "grey20", size = .2, linetype = "dashed", alpha = 0.5) +
  geom_text(data = labels, aes(label = toupper(label), x = issue, y = value), inherit.aes = FALSE, family = "Anton", hjust = 0, size = 10, color = "grey20") +
  labs(x = NULL,
       y = glue("<span style='font-family:Anton;font-size:60pt;color:black;'>Nightcrawler</span><span style='font-family:Anton;font-size:60pt;color:#4d4d4d;'>: Claremont's Ringo</span><br><br><br>
       <span style='font-family:Lato;font-size:30pt;color:#4d4d4d'>During Chris Claremont's run on the X-men, he pushed the series, and the popularity of the comics to new heights.  He introduced many new characters and cemented Wolverine's immense appeal to readers.<br><br>
                Yet, one of the core members of the team (and my personal favourite), <span style='color:black;'><b>Nightcrawler</b></span>, was sometimes treated as an afterthought and a mere spectator across storylines and never really got the attention he deserved on the core X-men team.<br><br>
                He even got the Soap Opera Kiss of Death of being put in a Coma during the Mutant Massacre storyline, with other upcoming major arcs focusing on other X-team members from #225 onwards.<br><br>
                After Claremont's departure, Nightcrawler endured and rose again in popularity as part of Excalibur and other storylines, but received the Ringo treatment once more in a solo 2014 series by Claremont and was cancelled after 12 books.<br><br>
                I'm **not at all** bitter.<br><br>See for yourself in the streamgraphs of appearance counts for the core team by issue.</span><br><br><br>
                <span style='font-family:Lato;font-size:16pt;'>**Data**: The Claremont Run | **Graphic**: @jakekaupp</span>")) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(limits = c(74, NA), breaks = c(94, seq(125, 250, by = 25), 280), labels = glue::glue("Issue\n#{c(97, seq(125, 250, by = 25), 280)}"), position = "top") +
  scale_y_continuous(expand = c(.1, .1)) +
  facet_grid(name ~ ., scales = "free_y", space = "free") +
  coord_cartesian(clip ="off") +
  theme_jk(grid = FALSE,
           base_size = 20) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_textbox_simple(width = unit(6, "in"), vjust = 1),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        plot.background = element_rect(lighten("grey88"), color = NA))

ggsave(here("day24", "tdcc_day24.png"), out, width = 20, height = 20, limitsize = FALSE)

