# install packages ----------------------------
# install.packages("tidytuesdayR")
# install.packages("tidytext")
# install.packages("gganimate")
# install.packages("ggtext")
# install.packages("mdthemes")
# install.packages("transformr")

# load libraries ----------------------------
library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(ggtext) 
library(mdthemes)
library(lubridate)
library(transformr)
library(gganimate)


# notes and credits -------------------------------------------------------

# @Kierisi's 13 July 2021 twitch stream
# 💻 Code with me! || 🐕 Scooby Doo data || 💖 Beginner-friendly graphs with {ggplot} || 🧹 Tidy Tuesday Unfiltered

# https://juliasilge.com/blog/reorder-within/
# https://thomasadventure.blog/posts/enhance-ggplot2-with-ggtext/
# https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/
# https://www.color-hex.com/color-palette/49179
# https://www.r-bloggers.com/2012/10/palettes-in-r/
# https://www.stat.ubc.ca/~jenny/STAT545A/block17_colorsGgplot2Qualitative.html

# read in data -------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
scoobydoo <- tuesdata$scoobydoo

# alternative that changes NULL to NA during import
scoobydoo_v2 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv', na = "NULL")

# wrangle monster types for first dataset --------------------------------------------------------------------
# ctl-shift-r makes these section labels

glimpse(scoobydoo)

tidyscooby <- scoobydoo %>%
  mutate(across(where(is.character), factor)) %>%
  separate_rows(monster_type, sep = ",", convert = TRUE) %>%
  mutate(monster_type = str_trim(monster_type),
         monster_type = recode(monster_type,
                               Disguise = "Disguised",
                               Disugised = "Disguised",
                               Insect = "Animal",
                               Skeleton = "Undead",
                               "Possessed Object" = "Possessed")) %>%
  filter(monster_type != "") %>%
  filter(monster_type != "NULL") %>%
  filter(motive != "NULL")
  
glimpse(tidyscooby)

# initial monster types exploring -------------------------------------------------------

scoobydoo %>%
  group_by(motive) %>%
  summarise(count = n()) %>%
  ggplot(aes(reorder(motive, count), count)) +
  geom_col() +
  coord_flip() 

scoobydoo %>%
  filter(motive == "Competition") %>%
  group_by(monster_type) %>%
  summarise

scoobydoo %>%
  count(monster_type)

scoobydoo %>%
  distinct(monster_type)

scoobydoo %>%
  distinct(monster_amount)

scoobydoo %>%
  filter(monster_type == "NULL") %>%
  head()

scoobydoo %>%
  filter(monster_type == "Possessed Object") %>%
  select(monster_type, monster_subtype) %>%
  view()

# exploring after tidying -------------------------------------------------------

tidyscooby %>%
  filter(motive == "Competition") %>%
  group_by(monster_type) %>%
  summarise(count = n()) %>%
  ggplot(aes(reorder(monster_type, count), count)) +
  geom_col() +
  coord_flip()

# monster types and motives ------------------------------------------------------------

tidyscooby %>%
  group_by(monster_type, motive) %>%
  summarise(count = n()) %>%
  ggplot(aes(motive, count)) +
  geom_col() +
  coord_flip() +
  facet_wrap(vars(monster_type))


# monster types and motives (for a select group) ------------------------------------------------------------

# https://juliasilge.com/blog/reorder-within/
# https://www.color-hex.com/color-palette/49179
# https://www.r-bloggers.com/2012/10/palettes-in-r/
# https://www.stat.ubc.ca/~jenny/STAT545A/block17_colorsGgplot2Qualitative.html
# https://thomasadventure.blog/posts/enhance-ggplot2-with-ggtext/
# https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/

colors <- c("#128A84","#4B0055",  "#BB5C37")


tidyscooby %>%
  filter(monster_type %in% c("Animal", "Ghost", "Undead")) %>%
  mutate(monster_type = factor(monster_type, levels = c("Ghost", "Animal", "Undead"))) %>%
  group_by(monster_type, motive) %>%
  summarise(count = n()) %>%
  top_n(8) %>%
  ungroup() %>%
  mutate(motive = reorder_within(motive, count, monster_type)) %>%
  ggplot(aes(motive, count, fill = monster_type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~monster_type, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Do motives differ by monster type in Scooby Doo episodes?",
      subtitle = "Competition is most common overall, but <span style = 'color:#4B0055'>**ghosts**</span> prioritise theft and conquest more than <span style = 'color:#128A84'>**animals**</span> and the <span style = 'color:#BB5C37'>**undead**</span>", 
      y = NULL,
      x = NULL,
      caption = "Source: kaggle, with thanks to plummye | Tidy Tuesday 2021, Week 29") +
  theme(plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown()) +
  scale_fill_manual(values = colors) +
  md_theme_minimal()

# Error in bl_make_text_box(token, drawing_context$gp, drawing_context$yoff_pt) : 
# function 'Rcpp_precious_remove' not provided by package 'Rcpp'
# I had to reinstall Rcpp to fix this error (i.e., re-run install.packages("Rcpp"))
# https://stackoverflow.com/questions/21657575/what-does-this-mean-in-lme4-function-dataptr-not-provided-by-package-rcpp

# https://www.kaggle.com/williamschooleman


# monster types and motives (for a select group) ------------------------------------------------------------
# as percentage: https://stackoverflow.com/questions/30233590/how-to-draw-a-ggplot2-with-facet-wrap-showing-percentages-from-each-group-not
# colors: https://www.color-hex.com/color-palette/49179

tidyscooby %>%
  filter(monster_type %in% c("Animal", "Undead", "Ghost")) %>%
  filter(motive != "NULL") %>%
  group_by(monster_type, motive) %>%
  summarise(count = n()) %>%
  mutate(percent = 100*count/sum(count)) %>%
  top_n(8) %>%
  ungroup() %>%
  mutate(motive = reorder_within(motive, count, monster_type)) %>%
  ggplot(aes(motive, percent, fill = monster_type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~monster_type, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0))+
  labs(title = "Do motives differ by monster type in Scooby Doo?",
       subtitle = "Competition is the most common motive for animal, ghost and undead alike, but theft and conquest may be higher priorities for ghosts",
       x = NULL,
       y = "Percentage by group (only top 8 values shown)") +
  theme_minimal()

# wrangle motives and decade ----------------------------------------------

# initial motives exploring -----------------------------------------------

#603 obs - 67 don't have motive
scoobydoo_v2 %>%
  count(is.na(motive))

# 67 obs broken down by monster type
scoobydoo_v2 %>%
  group_by(monster_type) %>%
  filter(is.na(motive)) %>%
  count 

#603 obs - 87 don't have monster type
scoobydoo_v2 %>%
  count(is.na(monster_type))

# 131 rows - counting monster types where not null and we have motive
scoobydoo_v2 %>%
  drop_na(monster_type) %>%
  drop_na(motive) %>%
  group_by(monster_type) %>%
  summarise

# 536 rows, 27 motives
scoobydoo_v2 %>%
  drop_na(motive) %>%
  group_by(motive) %>%
  count  

# motives by decade (wrangling practice) ---------------------------------------------------------
# https://www.r-bloggers.com/2016/10/difference-between-paste-and-paste0/

scoobydoo_v2 %>%
  mutate(air_year = lubridate::year(date_aired)) %>% 
  mutate(decade = paste0(substr(air_year, 3, 3), "0s")) %>%
  group_by(air_year) %>%
  count () %>%
  view()

scoobydoo_v2 %>%
  mutate(air_year = lubridate::year(date_aired)) %>% 
  mutate(decade = ifelse(air_year <2000, paste0("19", substr(air_year, 3, 3), "0s"),
         paste0("20", substr(air_year, 3, 3), "0s"))) %>%
  group_by(decade) %>%
  count () %>%
  view()

scoobydoo_v2 %>%
  mutate(air_year = lubridate::year(date_aired)) %>% 
  mutate(decade = ifelse(air_year <2000, paste0("19", substr(air_year, 3, 3), "0s"),
                         paste0("20", substr(air_year, 3, 3), "0s"))) %>%
  group_by(format, decade) %>%
  count () %>%
  view()

# motives by decade (chart) -----------------------------------------------

scoobydoo_v2 %>%
  mutate(air_year = lubridate::year(date_aired)) %>% 
  mutate(decade = ifelse(air_year <2000, paste0("19", substr(air_year, 3, 3), "0s"),
                         paste0("20", substr(air_year, 3, 3), "0s"))) %>%
  drop_na(motive) %>%
  filter(format %in% c("TV Series", "TV Series (segmented)")) %>%
  filter(decade %in% c("1970s", "1980s", "2000s", "2010s")) %>%
  group_by(decade, motive) %>%
  summarise(count = n()) %>%
  top_n(3) %>%
  ungroup() %>%
  mutate(motive = reorder_within(motive, count, decade)) %>%
  ggplot(aes(motive, count, fill = decade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~decade, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Top 3 motives of antagonists in Scooby Doo episodes by decade",
       y = NULL,
       x = NULL,
       caption = "Source: kaggle, with thanks to plummye | Tidy Tuesday 2021, Week 29") +
  theme(plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown()) +
  md_theme_minimal()

# motives by decade (as percent) -----------------------------------------------

colors <- c("#128A84", "#79af30", "#BB5C37", "#4B0055")

scoobydoo_v2 %>%
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

# monster names ------------------------------------------------------------

scoobydoo %>%
  separate_rows(monster_type, sep = ",", convert = TRUE) %>%
  mutate(monster_type = str_trim(monster_type)) %>%
  filter(monster_type != "NULL") %>%
  group_by(monster_name) %>%
  summarise(count = n()) %>%
  view()

# catchphrases ------------------------------------------------------------

scoobydoo %>%
  group_by(if_it_wasnt_for) %>%
  summarise(count = n()) %>%
  arrange(-(count))

# my first animated plot ------------------------------------------------------------
# https://goodekat.github.io/presentations/2019-isugg-gganimate-spooky/slides.html#1
  
scoobydoo_v2 %>%
    select(date_aired, jinkies) %>%
    mutate(air_year = lubridate::year(date_aired)) %>% 
    group_by(air_year) %>%
    summarize(jpy=sum(jinkies, na.rm = TRUE)) %>% 
    ggplot(aes(air_year, jpy)) + 
    geom_line() +
    transition_reveal(air_year)

# when i mutated to make jinkies numeric, I needed:
  # mutate(jinkies = as.numeric(jinkies)) %>%
  # ggplot(aes(air_year, jpy, group = 1)) + 

# https://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation

scoobydoo_v2 %>%
    select(date_aired, jeepers) %>%
    mutate(air_year = lubridate::year(date_aired)) %>% 
    group_by(air_year) %>%
    summarize(jeepy = sum(jeepers, na.rm = TRUE)) %>% 
    ggplot(aes(air_year, jeepy)) + 
    geom_line() +
    transition_reveal(air_year)