
#github.com/hrbrmstr/waffle/blob/master/README.md

# load libraries ----------------------------
library(tidytuesdayR)
library(tidyverse)
library(summarytools)
library(scales)
library(here)
library(ggbeeswarm)
library(waffle)
library(ggthemes)
library(ggtext)    # for formatting text in graph titles as markdown, use of element_textbox_simple
library(gt)


# install.packages("waffle", repos = "https://cinc.rud.is")


# read in data -------------------------------

# read in data with tidytuesdayR
# tuesdata <- tidytuesdayR::tt_load(2021, week = 11)

# bechdel <- tuesdata$raw_bechdel
# movies <- tuesdata$movies

# alternative for reading in data manually
# raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
# movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

# save to csv
# write_csv(bechdel, here("2021", "2021-week11", "data", "bechdel.csv"))
# write_csv(movies, here("2021", "2021-week11", "data", "movies.csv"))
# write_csv(joined, here("2021", "2021-week11", "data", "joined.csv"))

# read in from saved data while internet is broken

bechdel <- read_csv(here("2021", "2021-week11", "data", "bechdel.csv"))
movies <- read_csv(here("2021", "2021-week11", "data", "movies.csv"))

freq(movies$test)
freq(bechdel$rating)

bechdel$rating <- as.factor(bechdel$rating)

# silly basic questions, how do I get a count of movies in this dataset?

count(bechdel$imdb_id)

# simple graph, of 8839 movies, how many achieved each rating?

bechdel %>%
  ggplot(aes(rating)) +
  geom_bar()

# simple graph, show distribution of 8839 movies by year

bechdel %>%
  ggplot(aes(year)) +
  geom_bar() 

# now color that by rating 

bechdel %>%
  ggplot(aes(x = year)) +
  geom_bar()

# can we join the two datasets?
  
joined <- full_join(bechdel, movies, by = "imdb_id")

# various checks on ratings and joins

freq(joined$test)
freq(joined$rating)

count(movies, test)
count(joined, test)  

count(bechdel, rating)
count(joined, rating)

table(joined$rating, joined$test)

table(joined$test, joined$clean_test)

table(joined$clean_test, joined$rating)

# One for the Money (2012) missing rating - NA

# various recoding

joined$runtime <- as.numeric(str_remove(joined$runtime, " min"))


# simple graph, show runtime by decade

joined %>%
  ggplot(aes(decade_code, runtime), alpha = .5) +
  geom_point() +
  geom_jitter()

# show runtime by Bechdel rating - not a lot of difference

joined %>%
  ggplot(aes(runtime)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~ rating)

# scatter graph, x by y, color by rating 

joined %>%
  ggplot(aes(decade_code, budget_2013), alpha = .5) +
  geom_point(aes(color = rating)) 


# beeswarm graph, decade by budget, color ratings ---------------- 

options(scipen=999) 

joined %>%
  ggplot(aes(decade_code, budget_2013), alpha = .5) +
  geom_beeswarm(aes(color = rating)) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous()


# looking at top 10 grossing films by decade ---------------- 

## make a frequency table for graphing: this seems wrong, let's keep and try again below

ratings_bechdel_df <- joined %>%
  filter(year.x > 1969) %>%  #& year.x < 2010
  mutate(decade = paste0(str_sub(year.x, 1, 3), c("0s"))) %>%
  group_by(decade) %>%
  top_n(100, intgross) %>%
  ungroup() %>%
  filter(rating%in% c("0", "1", "2", "3")) %>%
  count(decade, rating)

## retry and test creating a frequency table for graphing

ratings_decade_df <- joined %>%
  filter(year.x > 1969) %>%  #& year.x < 2010
  mutate(decade = paste0(str_sub(year.x, 1, 3), c("0s"))) %>%
  group_by(decade) %>%
  count(decade, rating) 

gt(ratings_decade_df)


## try creating top 100 for one decade

top100_1970s <- joined %>%
  mutate(intgross = as.numeric(intgross)) %>%
  mutate(decade = paste0(str_sub(year.x, 1, 3), c("0s"))) %>%
  select(decade, rating, title.x, intgross, intgross_2013) %>%
  group_by(decade) %>%
  top_n(5, intgross) %>%
  arrange(desc(decade, intgross))

class(joined$intgross)

%>%
  top_n(100, intgross) %>%
  ungroup() %>%
  filter(rating%in% c("0", "1", "2", "3")) %>%
  count(decade, rating)

## waffle plot

ratings_bechdel_df$rating <- factor(ratings_bechdel_df$rating,
        labels = c("At least two named women", "Who talk to each other", "About something besides a man", "Pass"))

ggplot(ratings_bechdel_df, aes(fill = rating, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~decade, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, expand = c(0, 0)) +
  ggthemes::scale_fill_tableau(name = NULL) +
  coord_equal() +
  labs(
    title = "Were the 1990s as good as it gets for female inclusion in film?", 
    subtitle = "<b style = 'color:#F8766D'>text</b> Bechdel test results for the top 100 grossing films by decade. Films either passed, or failed because
    they did not feature at least two named women, who talk to each other, about something besides a man. 
    
    As good as it gets (1997) was one of the xx films from the 1990s meeting these criteria."
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

# can I use these?  doesn't seem to work with element_text and function
# "<b style = 'color:#F8766D'>text</b> 
  #      plot.title = element_markdown,
  #      plot.subtitle = element_markdown

# what colors are used in a plot -----

# useful expression for seeing what colors are use to make a plot (plus other details)
# source: https://stackoverflow.com/questions/25211078/what-are-the-default-plotting-colors-in-r-or-ggplot2
ggplot_build(p)$data

# colors: #4E79A7, #F28E2B, #76B7B2, #4E79A7

# waffle plot, rating by decade from bechdel data ---------------- 

## make a frequency table for graphing

ratings_bechdel_df <- joined %>%
  filter(year.x > 1949) %>%
  mutate(decade = paste0(str_sub(year.x, 1, 3), c("0s"))) %>%
  filter(rating%in% c("0", "1", "2", "3")) %>%
  count(decade, rating)

## waffle plot

ggplot(ratings_bechdel_df, aes(fill = rating, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~decade, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, expand = c(0, 0)) +
  ggthemes::scale_fill_tableau(name = NULL) +
  coord_equal() +
  labs(
    title = "Test"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))



# waffle plot, rating by decade from movies database ---------------- 

## make a frequency table for graphing

ratings_df <- joined %>%
  filter(rating%in% c("0", "1", "2", "3")) %>%
    filter(decade_code %in% c("1", "2", "3")) %>%
  count(decade_code, rating)

## waffle plot

ggplot(ratings_df, aes(fill = rating, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~decade_code, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, expand = c(0, 0)) +
  ggthemes::scale_fill_tableau(name = NULL) +
  coord_equal() +
  labs(
    title = "Test"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))
  

# example waffle plot for comparison -----------

storms %>%
  filter(year >= 2010) %>%
  count(year, status) -> storms_df

ggplot(storms_df, aes(fill = status, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, expand = c(0, 0)) +
  ggthemes::scale_fill_tableau(name = NULL) +
  coord_equal() +
  labs(
    title = "Test"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

