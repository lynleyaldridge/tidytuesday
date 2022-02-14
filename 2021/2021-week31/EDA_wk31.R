

# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(ggridges)

# load and manipulate data ------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
olympics <- tuesdata$olympics
regions <- tuesdata$regions

glimpse(regions)
glimpse(olympics)

olympics %>%
  filter(sport == "Judo", medal == "Gold") %>%
  group_by(noc) %>%
  count() %>%
  arrange(-(n))


olympics %>%
  filter(sport == "Ice Hockey", medal == "Gold") %>%
  group_by(noc) %>%
  count() %>%
  arrange(-(n))

olympics %>%
  filter(sport == "Ice Hockey", medal == "Gold") %>%
  group_by(event, year, noc) %>%
  count() %>%
  view()

olympics %>%
  filter(sport == "Ice Hockey", medal == "Gold") %>%
  group_by(event, year, noc) %>%
  select(event, year, noc) %>%
  distinct() %>%
  arrange(year, event) %>%
  view()

olympics %>%
  filter(sport == "Ice Hockey", medal == "Gold") %>%
  select(event, year, noc) %>%
  distinct() %>%
  group_by(noc) %>%
  count() %>%
  arrange(-(n))

# gold medals by country 

olympics %>%
  filter(noc == "NZL", medal == "Gold") %>%
  select(event, year) %>%
  distinct(year, event) %>%
  arrange(year)

# most medals 

olympics %>%
  drop_na() %>%
  group_by(name) %>%
  count() %>%
  arrange(-(n))


# oldest medalists by sport 

olympics %>%
  filter(season == "Summer") %>%
  mutate(
    medalist = case_when(
     medal %in% c("Gold", "Silver", "Bronze") ~ "1",
     is.na(medal) ~ "0"
     )
    ) %>%
  group_by(sport) %>%
  summarize(oldest = max(age))


olympics %>%
  filter(sport == "Archery") %>%
  select(name, age) %>%
  arrange(-(age))

# ridgelines of age distributions by sport

olympics %>%
  ggplot(aes(x = age, y = sport, height = height, group = sport)) +
  geom_density_ridges(stat = "identity", scale = 1)


olympics %>%
  drop_na() %>%
  group_by(sport) %>%
  summarize(age) %>%
  ggplot(aes(x = factor(age), fill = factor(sport))) +
  geom_bar(position = "stack")


  ggplot(aes(x = age, y = sport, height = height, group = sport)) +
  geom_density_ridges(stat = "identity", scale = 1)


