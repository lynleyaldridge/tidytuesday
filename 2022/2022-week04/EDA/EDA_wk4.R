
# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(gt)
library(ggdist)
library(janitor)

# load and manipulate data ------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-01-25')
ratings <- tuesdata$ratings
details <- tuesdata$details

bgg.19m.reviews <- read.csv("C:/Users/lynle/Documents/Data/tidytuesday_large/bgg-19m-reviews.csv", colClasses = c(NA, NA, NA, "NULL", NA, NA)) %>%
  clean_names()

werewolf_fans <- bgg.19m.reviews %>%
  filter(name == "Werewolf", rating == 10) %>%
  select(user, name, rating)

glimpse(ratings)
glimpse(details)

boardgames <- 
  full_join(ratings, details, by = 'id')
glimpse(boardgames)

# top ten 2 player games

two_player_only <- boardgames %>%
  filter(minplayers == 2 & maxplayers ==2) %>%
  arrange(-average) %>%
  top_n(10) %>%
  rowwise() %>%
  mutate(
    playtime_R = ifelse(minplaytime==maxplaytime,
                        as.character(minplaytime),
                        paste0(c(minplaytime, maxplaytime), collapse = "-"))) %>%
  ungroup() %>%
  mutate(
    cat = str_remove_all(boardgamecategory, "\\'"),
    cat = str_remove_all(cat, "\\["),
    cat = str_remove_all(cat, "\\]"),
    cat = str_remove_all(cat, "\"")) %>%
  select(id, name, thumbnail, year,  cat, playtime_R, average, users_rated) 

# ratings of top ten 2 player games only and box plot of this data

bgg.reviews <- 
  left_join(two_player_only, bgg.19m.reviews)
glimpse(bgg.reviews)


plots_dat <- bgg.reviews %>%
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
# drawing on https://github.com/rivasiker/TidyTuesday/blob/main/2022/2022-01-25/analysis_2022-01-25.Rmd
# can I highlight ratings from our friends, those who rated game x a ten??

two_player_only %>%
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

    
## reviews for top twenty 2 player games --------------------------------

bgg.reviews.playable <- boardgames %>%
  filter(minplayers == 1 | minplayers == 2) %>%
  arrange(-average) %>%
  top_n(20) %>%
  left_join(bgg.19m.reviews)

bgg.reviews.playable <- left_join(bgg.reviews.playable, werewolf_fans, by = "user")


plots_dat <- bgg.reviews.playable %>%
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

# top twenty 2 player games  ------------------------------------------------

boardgames %>%
  filter(minplayers == 1 | minplayers == 2) %>%
  arrange(-average) %>%
  top_n(20) %>%
  rowwise() %>%
  mutate(
    playtime_R = ifelse(minplaytime==maxplaytime,
                        as.character(minplaytime),
                        paste0(c(minplaytime, maxplaytime), collapse = "-")),
    players = ifelse(minplayers==maxplayers,
                        as.character(minplayers),
                        paste0(c(minplayers, maxplayers), collapse = "-"))) %>%
  ungroup() %>%
  mutate(
    cat = str_remove_all(boardgamecategory, "\\'"),
    cat = str_remove_all(cat, "\\["),
    cat = str_remove_all(cat, "\\]"),
    cat = str_remove_all(cat, "\""), 
    ggplot = NA) %>%
  select(name, thumbnail, year, cat, players, playtime_R, average, users_rated, ggplot) %>%
  gt() %>%
  cols_label(
    name = "Game",
    thumbnail = "",
    year = "Year published",
    cat = "Category",
    players = "Number of players",
    playtime_R = "Playing time (minutes)",
    average = "Average rating",
    users_rated = "Number of ratings",
    ggplot = "Distribution of ratings") %>%
  text_transform(
    locations=cells_body(ggplot),
    fn = function(x) {map(plots_dat$plots, ggplot_image, height = px(40), aspect_ratio = 5)}
  ) %>%
  cols_align(
    align = c("center"),
    columns = c(everything())) %>%
  text_transform(
    locations = cells_body(columns = c(thumbnail)),
    fn = function(x) {
      web_image(url = x, height = 80)
    } ) %>%
  fmt_number(columns = c(average), decimals = 1) %>%
  data_color(columns = average,
             colors = scales::col_numeric(
               palette = "Blues",
               domain = c(7.6, 8.8))) %>%
  tab_header(
    title = "Top twenty board games playable by 2 players") %>%
  tab_source_note(source_note = "Source: BoardGameGeek (via Kaggle) | TidyTuesday 2022, Week 4")

## visualizing uncertainty with ggdist - individual game ratings --------------
# https://mjskay.github.io/ggdist/articles/slabinterval.html
# these show how ratings are concentrated on whole and half numbers, let's try next with averages by category

bgg.reviews %>%
  mutate(name = fct_reorder(name, average)) %>%
    ggplot(aes(y = name, x = rating)) +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf)))) +
  scale_fill_brewer(direction = -1) 

bgg.reviews %>%
  mutate(name = fct_reorder(name, average)) %>%
    ggplot(aes(y = name, x = rating)) +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, 
                                          .width = c(.5, .8, .95)))),
               position = "dodge") +
  stat_dotsinterval(side = "bottom", scale = 0.6, position = "dodge") +
  scale_fill_brewer(direction = -1, na.translate = FALSE) +
  labs(fill = "Interval") + 
  scale_x_continuous(breaks = seq(0, 10, by = .5))

bgg.reviews %>%
  mutate(name = fct_reorder(name, average)) %>%
  ggplot(aes(y = name, x = rating)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(0, 10, by = .5))

summary <- bgg.reviews %>%
  group_by(name) %>%
  summarize(n = n(),
    median_rating = median(rating),
    average = mean(rating),
    sd = sd(rating)) %>%
  arrange(desc(average)) 

view(summary)

## plotting individual game ratings and highlighting selected observations --------------
# need to troubleshoot ordering of games 

bgg.reviews.playable %>%
  group_by(name.x) %>%
  mutate(name.x = fct_reorder(name.x, average)) %>%
  ggplot() +
  geom_boxplot(aes(y = name.x, x = rating.x)) +
  geom_point(data = bgg.reviews.playable %>% 
                filter(user %in% c("ljtrigirl", "doppio")) %>%
                mutate(color = case_when(user == "ljtrigirl" ~ "blue",
                                        user == "doppio" ~ "green", 
                                        TRUE ~ "black"),
                        name.x = fct_reorder(name.x, average)),
  aes(y = name.x, x = rating.x, color = color)) +
  scale_color_identity()

bgg.reviews.playable %>%
  mutate(color = ifelse(is.na(rating.y), "black", "red")) %>%
  group_by(name.x) %>%
  ggplot() +
  geom_jitter(aes(y = name.x, x = rating.x, color = color)) +
  scale_color_identity()

bgg.reviews.playable %>%
  filter(rating.y == 10) %>%
  group_by(name.x) %>%
  ggplot() +
  geom_jitter(aes(y = name.x, x = rating.x)) +
  scale_color_identity()



## visualizing uncertainty with ggdist - ratings by category -----------------

### sorting by category
# drawing on https://github.com/rivasiker/TidyTuesday/blob/main/2022/2022-01-25/analysis_2022-01-25.Rmd

category <- boardgames %>% 
  dplyr::select(id, name, boardgamecategory, bayes_average, thumbnail, 
                yearpublished, minplayers, maxplayers, playingtime) %>% 
  group_by(id, name, bayes_average, thumbnail, yearpublished, 
           minplayers, maxplayers, playingtime) %>% 
  # Remove unwanted characters
  mutate(
    cat = str_remove_all(boardgamecategory, "\\'"),
    cat = str_remove_all(cat, "\\["),
    cat = str_remove_all(cat, "\\]"),
    cat = str_remove_all(cat, "\""),
    cat = str_replace_all(cat, "Childrens", "Children\\'s")
  ) %>% 
  # Extract categories
  summarise(
    cat = str_split(cat, ', ')
  ) %>% 
  unnest(cat) %>% 
  rowwise() %>% 
  ungroup()

# there are 21831 different ids, but only 21432 different names
# details has 21631 obs, and ratings 21831 obs?

category %>%
  summarise(n = n_distinct(name))

boardgames %>%
  summarise(n = n_distinct(name))

## summarizing distributions for top 17 categories

category %>%
  group_by(cat) %>%
  summarize(n = n()) %>%
  # Get the top 17 categories with more games
  top_n(17, n) %>% 
  # Join info with whole dataset
  # This is for removing all other categories
  left_join(category, by = "cat") %>% 
  group_by(cat) %>%
  select(-c(thumbnail, yearpublished, minplayers, maxplayers)) %>%
  summarise(
    group_average = median(bayes_average), 
    group_sd = sd(bayes_average),
    n = n(),
    max = max(bayes_average)) %>% 
  arrange(desc(max)) %>%
  view()
  
## graphing distributions for top 17 categories 


category %>%
  group_by(cat) %>%
  summarize(n = n(),     
            group_average = median(bayes_average), 
            max = max(bayes_average)) %>%
  top_n(17, n) %>% 
  left_join(category, by = "cat") %>% 
  mutate(cat = fct_reorder(cat, max)) %>%
  ggplot(aes(y = cat, x = bayes_average)) +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, 
                                          .width = c(.5, .8, .95)))),
               position = "dodge") +
  stat_dotsinterval(side = "bottom", scale = 0.6, position = "dodge") +
  stat_summary(fun = "max", colour = "blue", size = 1, geom = "point") +
  scale_fill_brewer(direction = -1, na.translate = FALSE) +
  labs(fill = "Interval") + 
  scale_x_continuous(limits = c(4.75, 8.511))
  

category %>%
  group_by(cat) %>%
  summarize(n = n(),     
            group_average = median(bayes_average), 
            max = max(bayes_average)) %>%
  top_n(17, n) %>% 
  left_join(category, by = "cat") %>% 
  mutate(cat = fct_reorder(cat, max)) %>%
  ggplot(aes(y = cat, x = bayes_average)) +
  geom_boxplot() +
  stat_summary(fun = "max", colour = "blue", size = 1, geom = "point") +
  scale_x_continuous(limits = c(4.75, 8.511))
