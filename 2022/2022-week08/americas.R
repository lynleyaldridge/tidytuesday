
# load libraries ----------------------------------------------------------

# install.packages('devtools')
# devtools::install_github('rensa/ggflags')

library(tidyverse)
library(here)
library(janitor) # for cleaning column names
library(countrycode) # for converting country names to country codes
library(rvest) # for extracting data from html tables
library(sf) # for mapping
library(rnaturalearth) # for mapping
library(ggflags) # for flag images used in chart
library(patchwork) # for combining plots

# harvesting global freedom scores for 2022 from Freedom House website ----
# https://dcl-wrangle.stanford.edu/rvest.html
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
scores$region_un <- countrycode(scores$country, "country.name", "un.region.name")


# uncomment to save data
# write_csv(scores, here::here("2022", "2022-week08", "data", "scores.csv"))

# uncomment to load a copy of this data from my github, instead of rvest scraping above
# read_csv("https://raw.githubusercontent.com/lynleyaldridge/tidytuesday/main/2022/2022-week08/data/scores.csv")

# americas map ------------------------------------------------------------
# cropping maps: https://stackoverflow.com/questions/68679777/error-when-trying-to-crop-spatial-extent-using-package-sf

map <- ne_countries(returnclass = "sf") %>%
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

# americas chart ----------------------------------------------------------

chart <- scores %>%
  filter(region_un == "Americas") %>%
  filter(!is.na(country)) %>%
  mutate(code = tolower(code)) %>% 
  mutate(score = as.numeric(score)) %>%
  mutate(country = fct_reorder(country, score)) %>%
  ggplot(aes(x = country, y = score, fill = status)) +
  geom_col(width = 0.75, position = position_dodge(width = 0.75)) +
  coord_flip() +
  scale_fill_manual(values = c("#3BAC87", "#B9A350", "#9868a0"),
                    breaks = c("Free", "Partly Free", "Not Free"),
                    name = "Status") +
  geom_flag(y = 0, aes(country = code), size = 3.5) +
  labs(y = NULL, x = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

map + chart + plot_annotation(title = "Global freedom score of countries in the Americas, 2022",
                               subtitle = "Based on Freedom in the World's annual global assessment of political rights and civil liberties",
                               caption = "Source: Freedom House | TidyTuesday 2022, Week 8")

# uncomment to save
# ggsave(filename = here::here("2022", "2022-week08", "globalfreedom_Americas_2022.png"))

