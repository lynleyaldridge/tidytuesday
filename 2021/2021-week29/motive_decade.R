# Faceted bar charts, presented by rank within facet, code for which draws heavily on the following sources:

# @Kierisi's 13 July 2021 twitch stream
# 💻 Code with me! || 🐕 Scooby Doo data || 💖 Beginner-friendly graphs with {ggplot} || 🧹 Tidy Tuesday Unfiltered

# https://juliasilge.com/blog/reorder-within/
# https://thomasadventure.blog/posts/enhance-ggplot2-with-ggtext/
# https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/
# https://www.color-hex.com/color-palette/49179

# load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(ggtext) 
library(mdthemes)
library(lubridate)

# load data ---------------------------------------------------------------

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv', na = "NULL")

# plot motives by decade (as percent) -----------------------------------------------

colors <- c("#128A84", "#79af30", "#BB5C37", "#4B0055")

scoobydoo %>%
  mutate(air_year = lubridate::year(date_aired)) %>% 
  mutate(decade = ifelse(air_year <2000, paste0("19", substr(air_year, 3, 3), "0s"),
                         paste0("20", substr(air_year, 3, 3), "0s"))) %>%
  drop_na(motive) %>%
  filter(format %in% c("TV Series", "TV Series (segmented)")) %>%
  filter(decade %in% c("1970s", "1980s", "2000s", "2010s")) %>%
  group_by(decade, motive) %>%
  summarise(count = n()) %>%
  mutate(percent = 100*count/sum(count)) %>%
  mutate(label = paste0(sprintf("%4.0f", percent), "%")) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(motive = reorder_within(motive, percent, decade)) %>%
  ggplot(aes(motive, percent, fill = decade)) +
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = label),
             hjust = 1,
             nudge_y = -.5,
             size = 4,
             fill = "white", label.size = 0) +
  facet_wrap(~decade, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Have the motives of antagonists in Scooby Doo episodes changed over time?",
       subtitle = "Theft was most common in the <span style = 'color:#128A84'>**1970s**</span> and <span style = 'color:#79af30'>**1980s**</span>, with competition dominant in the <span style = 'color:#BB5C37'>**2000s**</span> and <span style = 'color:#4B0055'>**2010s**</span>", 
       y = NULL,
       x = NULL,
       caption = "Top 5 motives shown as percent of all motives within decade | Source: kaggle, with thanks to plummye | Tidy Tuesday 2021, Week 29") +
  theme(plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown(),
        axis.text.y = element_text, hjust = 1) +
  scale_fill_manual(values = colors, guide = "none") +
  md_theme_minimal()


# save a copy of plot (remove commenting to use this part of code)

# library(here)
# ggsave(here::here("2021", "2021-week29", "motive_decade.png"))