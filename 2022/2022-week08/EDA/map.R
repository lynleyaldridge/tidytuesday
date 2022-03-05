
# load libraries ----------------------------------------------------------

# devtools::install_github('rensa/ggflags')
# reference
# https://github.com/gshs-ornl/wbstats

library(tidyverse)
library(janitor)
library(countrycode)
library(here)
library(rvest) # for extracting table from html for map 2
library(sf)
library(rnaturalearth)
library(ggflags)
library(patchwork)

# making simple map ------------------

## tidy tuesday dataset (list of countries and regions, at 2020)
# https://freedomhouse.org/reports/freedom-world/freedom-world-research-methodology
# https://twitter.com/quite_grey/status/1496253113493245959?s=20&t=-ZHdHOaVP7JMJZLrqzi1Ug

freedom_2020 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') %>%
  clean_names() %>%
  filter(year == 2020) %>%
  mutate(country = ifelse(
    country == "CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire", 
    "Ivory Coast", 
    country))

# https://www.youtube.com/watch?v=AgWgPSZ7Gp0

mapdata <- map_data("world")

mapdata$region[mapdata$region == "USA"] = "United States of America"
mapdata$region[mapdata$region == "Russia"] = "Russian Federation"
mapdata$region[mapdata$region == "Bolivia"] = "Bolivia (Plurinational State of)"
mapdata$region[mapdata$region == "Tanzania"] = "United Republic of Tanzania"
mapdata$region[mapdata$region == "Republic of Congo"] = "Congo"
mapdata$region[mapdata$region == "Iran"] = "Iran (Islamic Republic of)"
mapdata$region[mapdata$region == "Syria"] = "Syrian Arab Republic"
mapdata$region[mapdata$region == "Czech Republic"] = "Czechia"
mapdata$region[mapdata$region == "Moldova"] = "Republic of Moldova"
mapdata$region[mapdata$region == "UK"] = "United Kingdom of Great Britain and Northern Ireland"
mapdata$region[mapdata$region == "Venezuela"] = "Venezuela (Bolivarian Republic of)"

mapdata1 <- left_join(mapdata, freedom_2020, by = c("region" = "country")) %>%
  filter(!is.na(status)) 

map1 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = status), color = "black") +
  scale_fill_manual(values = c("#3BAC87", "#B9A350", "#9868a0"),
                    labels = c("Free", "Partly Free", "Not Free"),
                    name = "Status",
                    breaks = c("F", "PF", "NF")) +
  labs(title = "Global freedom status, 2020",
       subtitle = "Based on Freedom in the World's annual global assessment of political rights and civil liberties",
       caption = "Source: Freedom House (via Arthur Cheib) | TidyTuesday 2022, Week 8") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

map1

# ggsave(filename = here::here("2022", "2022-week08", "globalfreedomstatus_2020.png"))

## global freedom scores ---------------------------------
# https://dcl-wrangle.stanford.edu/rvest.html
# https://cran.r-project.org/web/packages/countrycode/countrycode.pdf
# https://www.kaggle.com/sezginildes/reshaping-data-with-tidyr
# https://community.rstudio.com/t/tidyr-separate-at-first-whitespace/26269

freedom_url <- "https://freedomhouse.org/countries/freedom-world/scores"
css_selector <- "#block-countryscorestable > table"

scores <- freedom_url %>% 
  read_html() %>%
  html_element(css = css_selector) %>%
  html_table() %>%
  separate("Total Score and Status", into = c("Score", "Status"), sep = " ", extra = "merge") %>%
  rename(country = 1) %>%
  filter(!str_detect(country, "\\*")) %>%
  clean_names() %>%
  mutate(country = ifelse(
    country == "Côte d'Ivoire", 
    "Ivory Coast", 
    country))

scores$code <- countrycode(scores$country, "country.name", "iso2c")
scores$region_wb <- countrycode(scores$country, "country.name", "region")
scores$region_un <- countrycode(scores$country, "country.name", "un.region.name")

# save data

# write_csv(scores, here::here("2022", "2022-week08", "data", "scores.csv"))


# map2 --------------------------------------------------------------------

mapdata <- map_data("world")

mapdata$region[mapdata$region == "USA"] = "United States"
mapdata$region[mapdata$region == "Republic of Congo"] = "Republic of the Congo"
mapdata$region[mapdata$region == "UK"] = "United Kingdom"

mapdata2 <- left_join(mapdata, scores, by = c("region" = "country")) %>%
  filter(!is.na(mapdata2$score)) %>%
  mutate(score = as.numeric(score))

map2 <- ggplot(mapdata2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = score), color = "black") +
  scale_fill_gradient2(name = "Global freedom score", midpoint = 50, low = "#9868a0", mid = "#B9A350", high = "#3BAC87")+
  labs(title = "Global freedom score, 2022",
       subtitle = "Based on Freedom in the World's annual global assessment of political rights and civil liberties",
       caption = "Source: Freedom House | TidyTuesday 2022, Week 8") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

map2

# ggsave(filename = here::here("2022", "2022-week08", "globalfreedomscore_2022.png"))


# americas map ------------------------------------------------------------
# cropping maps: https://stackoverflow.com/questions/68679777/error-when-trying-to-crop-spatial-extent-using-package-sf


map3 <- ne_countries(returnclass = "sf") %>%
  left_join(scores, by = c("iso_a2" = "code")) %>%
  filter(!is.na(score)) %>%
  mutate(score = as.numeric(score)) %>%
  ggplot() +
  geom_sf(aes(fill = score)) +
  scale_fill_gradient2(midpoint = 50, low = "#9868a0", mid = "#B9A350", high = "#3BAC87")+
  coord_sf(xlim = c(-165, -40),
           ylim = c(-57, 75)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank(),
        legend.position = "none")

map3

# global freedom scores - americas ----------------------------------------

chart <- scores %>%
  filter(region_un == "Americas") %>%
  filter(!is.na(country)) %>%
  mutate(code = tolower(code)) %>% 
  mutate(score = as.numeric(score)) %>%
  mutate(country = fct_reorder(country, score)) %>%
  ggplot(aes(x = country, y = score, fill = status)) +
  geom_col(alpha = 0.8, width = 0.75, position = position_dodge(width = 0.75)) +
  coord_flip() +
  scale_fill_manual(values = c("#3BAC87", "#B9A350", "#9868a0"),
                    breaks = c("Free", "Partly Free", "Not Free"),
                    name = "Status") +
  geom_flag(y = 0, aes(country = code), size = 4) +
  labs(x = NULL, y = "Global freedom score",
       title = "Global freedom score of countries in the Americas, 2022",
       subtitle = "Based on Freedom in the World's annual global assessment of political rights and civil liberties",
       caption = "Source: Freedom House | TidyTuesday 2022, Week 8") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

chart

ggsave(filename = here::here("2022", "2022-week08", "globalfreedomscores_Americas_2022.png"))

map3 + chart + plot_annotation(title = "Global freedom score of countries in the Americas, 2022",
                           subtitle = "Based on Freedom in the World's annual global assessment of political rights and civil liberties",
                           caption = "Source: Freedom House | TidyTuesday 2022, Week 8")

# global freedom scores - africa ----------------------------------------

scores %>%
  filter(region_un == "Africa") %>%
  filter(!is.na(country)) %>%
  mutate(code = tolower(code)) %>% 
  mutate(score = as.numeric(score)) %>%
  mutate(country = fct_reorder(country, score)) %>%
  ggplot(aes(x = country, y = score, fill = status)) +
  geom_col(alpha = 0.8, width = 0.75, position = position_dodge(width = 0.75)) +
  coord_flip() +
  scale_fill_manual(values = c("#3BAC87", "#B9A350", "#9868a0"),
                    breaks = c("Free", "Partly Free", "Not Free")) +
  geom_flag(y = 0, aes(country = code), size = 6) +
  labs(y = "Global freedom score", x = NULL, 
       title = "Global freedom scores of countries in Africa (2022)", 
       caption = "Source: Freedom House | TidyTuesday 2022, Week 8") +
  theme_minimal() +
  theme(
    plot.margin = margin(.5, .5, .5, .5, unit = "cm"),
    strip.placement = "outside",
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

# ggsave(filename = here::here("2022", "2022-week08", "globalfreedomscores_Africa_2022.png"))


