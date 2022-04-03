# table showing top ten games for exactly 2 players, showing distributions of ratings for each game

# Code draws heavily on:
# https://github.com/rivasiker/TidyTuesday/blob/main/2022/2022-01-25/analysis_2022-01-25.Rmd

# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(gt)
library(here)
library(webshot)  # may need for exporting table as image

# load and manipulate data ------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-01-25')
ratings <- tuesdata$ratings
details <- tuesdata$details

bgg_reviews <- read.csv('https://raw.githubusercontent.com/lynleyaldridge/tidytuesday/main/2022/2022-week04/data/bgg_reviews.csv')

two_player_exactly <- 
  full_join(ratings, details, by = 'id') %>%
  filter(minplayers == 2 & maxplayers ==2) %>%
  arrange(-average) %>%
  top_n(10) %>%
  rowwise() %>%
  mutate(
    playtime_R = ifelse(minplaytime==maxplaytime,
                        as.character(minplaytime),
                        paste0(c(minplaytime, maxplaytime), collapse = "-"))) %>%
  mutate(
    cat = str_remove_all(boardgamecategory, "\\'"),
    cat = str_remove_all(cat, "\\["),
    cat = str_remove_all(cat, "\\]"),
    cat = str_remove_all(cat, "\"")) %>%
  select(id, name, thumbnail, year,  cat, playtime_R, average, users_rated) 

# ratings of top ten 2 player games only and box plot of this data

plots_dat <- bgg_reviews %>%
  arrange(desc(rating)) %>%
  group_by(name) %>%
  dplyr::select(rating) %>%
  nest() %>%
  rowwise() %>%
  mutate(
    plots = list(
      ggplot(data, aes(x=1, y=rating)) +
        geom_jitter(width =0.4, alpha= 0.5, size = 5) +
        geom_boxplot(size = 5, outlier.size = 4,
                     outlier.shape = NA, color = 'red', alpha = 0.5) +
        theme_minimal() +
        coord_flip(ylim = c(4.75, 10)) +
        theme(
          text=element_text(size = 150),
          axis.title = element_blank(),
          axis.text.y = element_blank()
        ))
  ) %>%
  ungroup()


# top ten 2 player games  ------------------------------------------------

two_player_exactly %>%
  select(-id) %>%
  mutate(ggplot = NA) %>%
  gt() %>%
  text_transform(
    locations=cells_body(ggplot),
    fn = function(x) {map(plots_dat$plots, ggplot_image, height = px(40), aspect_ratio = 5)}
  ) %>%
  cols_label(
    name = "Game",
    thumbnail = "",
    year = "Year published",
    cat = "Category",
    playtime_R = "Playing time (minutes)",
    average = "Average rating",
    users_rated = "Number of ratings",
    ggplot = "Distribution of ratings") %>%
  cols_align(
    align = c("center"),
    columns = c(everything())
  ) %>%
  text_transform(
    locations = cells_body(columns = c(thumbnail)),
    fn = function(x) {
      web_image(url = x, height = 80)
    } ) %>%
  fmt_number(columns = c(average), decimals = 1) %>%
  data_color(columns = average,
             colors = scales::col_numeric(
               palette = "Blues",
               domain = c(7, 8.3))) %>%
  tab_header(
    title = "Top ten board games for exactly 2 players") %>%
  tab_source_note(source_note = "Source: BoardGameGeek (via Kaggle) | TidyTuesday 2022, Week 4") 

# %>%

# save a copy of table (remove commenting to use this part of code)

# library(here)
# gtsave(filename = here::here("2022", "2022-week04", "top10exactly2players.png"))


# to prepare reviews data for use in this project
# first, download bgg-19m-reviews.csv from kaggle (https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews?select=bgg-19m-reviews.csv) to location of your own choice
# then, uncomment code below (adding path details) to create a dataset with ratings for each game in our table (from bgg_reviews file available from kaggle) 


# library(janitor)
# bgg_19m_reviews <- read.csv("path to your own downloaded version/bgg-19m-reviews.csv", colClasses = c(NA, NA, NA, "NULL", NA, NA)) %>%
#  clean_names()


# bgg_reviews <- 
#   left_join(two_player_exactly, bgg_19m_reviews) %>%
#   select(name, user, rating)

  
# write_csv(bgg_reviews, here::here("2022", "2022-week04", "data", "bgg_reviews.csv"))
