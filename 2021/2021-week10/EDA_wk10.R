# set up and loading data

# load libraries ----------------------------
library(tidytuesdayR)
library(tidyverse)
library(here)

# read in data -------------------------------
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

write_csv(youtube, here("2021", "2021-week10", "data", "youtube.csv"))


# explore data -------------------------------

