library(tidyverse)
library(devtools)
library(janitor)

# devtools::install_github("ekothe/nzbabynames")
library(nzbabynames)

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

rank_order <- nzbabynames %>%
  clean_names() %>%
  filter(sex == "Female") %>%
  group_by(name) %>%
  summarise(total = sum(count)) %>%
  arrange(desc(total)) %>%
  mutate(rank = rank(desc(total), ties.method = "min"))


nz <- nzbabynames %>%
  clean_names() %>%
  filter(sex == "Female",
         name == "Lynley") %>%
  select(year, count) %>%
  mutate(country = "NZ")

nz %>%
  ggplot(aes(x = year, y = count)) +
  geom_col()

us <- babynames %>%
  clean_names() %>%
  filter(sex == "F",
         name == "Lynley") %>%
  select(year, n) %>%
  mutate(country = "US") %>%
  rename(count = n)

us %>%
  ggplot(aes(x = year, y = count)) +
  geom_col()


# https://www.peretaberner.eu/merging-and-appending-datasets-with-dplyr/

merged <- us %>%
  bind_rows(nz)

merged %>%
  ggplot(aes(x = year, y = count)) +
  geom_col() +
  facet_wrap(~ country, ncol = 1)
