
# load libraries ----------------------------------------------------------

# devtools::install_github('rensa/ggflags')

library(tidyverse)
library(janitor)
library(rvest)
library(tidyr)
library(ggflags)
library(countrycode)
library(wbstats)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# reference
# https://github.com/gshs-ornl/wbstats

# load and manipulate data ------------------------------------------------

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
  clean_names()

scores$code <- countrycode(scores$country, "country.name", "iso2c")
scores$region_wb <- countrycode(scores$country, "country.name", "region")
scores$region_un <- countrycode(scores$country, "country.name", "un.region.name")

## population size (most recent value, from wbstats)
# https://github.com/gshs-ornl/wbstats

pop <- wb_data("SP.POP.TOTL", mrv = 1) %>%
  select(iso2c, SP.POP.TOTL, date) %>%
  rename(code = iso2c)

data <- scores %>%
  left_join(pop, by = "code") %>%
  select(-date)

# Oceania - global freedom scores plot: Oceania region (faceting remains) ----------------------------------------------
# https://mattherman.info/blog/fix-facet-width/

data %>%
  filter(region_un == "Oceania") %>%
  filter(!is.na(country)) %>%
  mutate(code = tolower(code)) %>% 
  mutate(score = as.numeric(score)) %>%
  mutate(country = fct_reorder(country, score)) %>%
  ggplot(aes(x = country, y = score, fill = region_un)) +
  geom_col(alpha = 0.8, width = 0.75, position = position_dodge(width = 0.75)) +
  coord_flip() +
  facet_grid(rows = vars(region_un), scales = "free_y", switch = "y", space = "free_y") +
  geom_flag(y = 0, aes(country = code), size = 7) +
  labs(y = "Freedom score", x = NULL, title = "xx", 
       caption = "xx") +
  theme_minimal() +
  theme(
    plot.margin = margin(.5, .5, .5, .5, unit = "cm"),
    legend.position = "none",
    strip.placement = "outside",
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )


# East Asia and Pacific - global freedom scores plot ----------------------------------------------

data %>%
  filter(region_wb == "East Asia & Pacific") %>%
  filter(!is.na(country)) %>%
  mutate(code = tolower(code)) %>% 
  mutate(score = as.numeric(score)) %>%
  mutate(country = fct_reorder(country, score)) %>%
  ggplot(aes(x = country, y = score, fill = status)) +
  geom_col(alpha = 0.8, width = 0.75, position = position_dodge(width = 0.75)) +
  coord_flip() +
  scale_fill_manual(values = c("#3BAC87", "#B9A350", "#9868a0"),
                    breaks = c("Free", "Partly Free", "Not Free")) +
  geom_flag(y = 0, aes(country = code), size = 5.5) +
  labs(y = "Global freedom score", x = NULL, 
       title = "Global freedom scores of countries in the East Asia and Pacific Region (2022)", 
       caption = "Source: Freedom House | TidyTuesday 2022, Week 8") +
  theme_minimal() +
  theme(
    plot.margin = margin(.5, .5, .5, .5, unit = "cm"),
    strip.placement = "outside",
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

# scatterplot with population sizes ---------------------------------------

data %>%
  filter(!is.na(region_un)) %>%
  ggplot(aes(x = political_rights, y = civil_liberties, size = SP.POP.TOTL, color = status)) +
  geom_point() +
  facet_grid(~region_un)


# faceted scatterplot with flags: asia ------------------------------------------
# https://github.com/jimjam-slam/ggflags

data %>%
  mutate(code = tolower(code)) %>% 
  filter(region_un == "Asia") %>%
  filter(!is.na(SP.POP.TOTL)) %>%
  ggplot(aes(x = political_rights, y = civil_liberties, country = code, size = SP.POP.TOTL)) +
  geom_flag() +
  scale_country() +
  scale_size(range = c(0, 15)) +
  theme(
    legend.position = "none"
  )

# faceted scatterplot with flags ------------------------------------------

data %>%
  mutate(code = tolower(code)) %>% 
  filter(!is.na(SP.POP.TOTL)) %>%
  ggplot(aes(x = political_rights, y = civil_liberties, country = code, size = SP.POP.TOTL)) +
  geom_flag() +
  scale_country() +
  scale_size(range = c(0, 15)) +
  facet_grid(~region_un) +
  theme(
    legend.position = "none"
  )


# americas - faceted scatterplot ------------------------------------------

data %>%
  mutate(code = tolower(code)) %>% 
  filter(!is.na(SP.POP.TOTL), region_un == "Americas") %>%
  ggplot(aes(x = political_rights, y = civil_liberties, country = code, size = SP.POP.TOTL)) +
  geom_flag() +
  scale_country() +
  scale_size(range = c(5, 20)) +
  theme(
    legend.position = "none"
  )

# map - using ggplot ----------------------------------------
# https://www.youtube.com/watch?v=AgWgPSZ7Gp0
# https://www.rgbtohex.net/
# https://www.statology.org/ggplot-legend-order/
# https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
# https://www.jessesadler.com/post/gis-with-r-intro/

mapdata <- map_data("world")

mapdata$region[mapdata$region == "USA"] = "United States"

mapdata <- left_join(mapdata, data, by = c("region" = "country"))

# guess_field(mapdata$region)


mapdata1 <- mapdata %>%
  filter(!is.na(mapdata$score)) %>%
  mutate(score = as.numeric(score))

map1 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = score), color = "black")

map1

map2 <- map1 + scale_fill_gradient(name = "Global freedom score", low = "purple", high = "green", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

map2

map3 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = status), color = "black") +
  scale_fill_manual(values = c("#3BAC87", "#B9A350", "#9868a0"),
                      breaks = c("Free", "Partly Free", "Not Free")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

map3


# yellow, purple and green gradient map -----------------------------------

mapdata <- map_data("world")

mapdata$region[mapdata$region == "USA"] = "United States"
mapdata$region[mapdata$region == "Republic of Congo"] = "Republic of the Congo"
mapdata$region[mapdata$region == "UK"] = "United Kingdom"

mapdata2 <- left_join(mapdata, scores, by = c("region" = "country")) %>%
  filter(!is.na(score)) %>%
  mutate(score = as.numeric(score))

map2 <- ggplot(mapdata2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = score), color = "black") +
  scale_fill_gradientn(colours = c("purple", "yellow", "dark green"), 
                       name = "Global freedom score")+  
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

#   scale_fill_gradient(name = "Global freedom score", low = "yellow", high = "red")+  


# attempt to use colorspacer lighten within categories ------------------------------------
# https://colorspace.r-forge.r-project.org/articles/manipulation_utilities.html

# palettes

status_colors <- tibble(
  status = c("Free", "Partly Free", "Not Free"),
  status_color = c("#3BAC87", "#B9A350", "#9868a0"))

scores_modified <- scores %>%
  filter(!is.na(score)) %>%
  mutate(score = as.numeric(score))%>%
  left_join(status_colors) %>%
  mutate(fill = lighten(status_color, score/105))


mapdata <- map_data("world")

mapdata$region[mapdata$region == "USA"] = "United States"
mapdata$region[mapdata$region == "Republic of Congo"] = "Congo"

mapdata2 <- left_join(mapdata, scores_modified, by = c("region" = "country")) 

map2 <- ggplot(mapdata2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = fill), color = "black") +
  scale_fill_identity()+
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

      
# freedom world map (using geom_sf and rnaturalearth) -------------------------------------------------------

# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://datavizpyr.com/how-to-make-world-map-with-ggplot2-in-r/
# https://slcladal.github.io/maps.html

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf()

# combine data frame with map




# save a copy of plot (remove commenting to use this part of code)

# library(here)
ggsave(filename = here::here("2022", "2022-week08", "freedombycountry.png"), height = 30, width = 20)



# global freedom scores plot: East Asia and Pacific (faceting remains) ----------------------------------------------

data %>%
  filter(region_wb == "East Asia & Pacific") %>%
  filter(!is.na(country)) %>%
  mutate(code = tolower(code)) %>% 
  mutate(score = as.numeric(score)) %>%
  mutate(country = fct_reorder(country, score)) %>%
  ggplot(aes(x = country, y = score, fill = status)) +
  geom_col(alpha = 0.8, width = 0.75, position = position_dodge(width = 0.75)) +
  coord_flip() +
  facet_grid(rows = vars(region_wb), scales = "free_y", switch = "y", space = "free_y") +
  geom_flag(y = 0, aes(country = code), size = 5) +
  labs(y = "Freedom score", x = NULL, title = "xx", 
       caption = "xx") +
  theme_minimal() +
  theme(
    plot.margin = margin(.5, .5, .5, .5, unit = "cm"),
    legend.position = "none",
    strip.placement = "outside",
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )



# superceded version beginning with tidy tuesday dataset ------------------

## tidy tuesday dataset (list of countries and regions, at 2020)
# https://freedomhouse.org/reports/freedom-world/freedom-world-research-methodology

freedom_2020 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') %>%
  clean_names() %>%
  filter(year == 2020) %>%
  select(country, region_name, is_ldc)

freedom_2020$code <- countrycode(freedom_2020$country, "country.name", "iso2c")

## population size (most recent value, from wbstats)
# https://github.com/gshs-ornl/wbstats

pop <- wb_data("SP.POP.TOTL", mrv = 1) %>%
  select(iso2c, SP.POP.TOTL, date) %>%
  rename(code = iso2c)

data <- freedom_2020 %>%
  left_join(pop, by = "code") %>%
  select(-date)

data <- data %>%
  select(-c(country)) %>%
  left_join(scores) %>%
  relocate(country)
