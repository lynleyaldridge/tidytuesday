# **My first map**, showing drought level by highest percentage area for each US county, code for which draws heavily on the following sources:

# https://kpress.dev/blog/2021-07-20-tidy-tuesday-drought-conditions/

# https://urban-institute.medium.com/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
# https://urbaninstitute.github.io/urbnmapr/articles/introducing-urbnmapr.html
# https://urbaninstitute.github.io/r-at-urban/mapping.html

# install packages --------------------------------------------------------

# uncomment to install devtools and urbnmapr if needed
# install.packages("devtools")
# library(devtools)
# devtools::install_github("UrbanInstitute/urbnmapr")

# load libraries ----------------------------------------------------------

library(tidyverse)
library(urbnmapr)
library(ggtext) 
library(RColorBrewer)

# load and manipulate data ---------------------------------------------------------------

# county data downloaded from US Drought Monitor  
county <- readr::read_csv('https://raw.githubusercontent.com/lynleyaldridge/tidytuesday/main/2021/2021-week30/data/county_categorical.csv')

dr_county <- county %>%
  pivot_longer(cols = None:D4, names_to = "drought_lvl", values_to = "area_pct") %>%
  mutate(drought_lvl = factor(drought_lvl, levels = c("D4", "D3", "D2", "D1", "D0", "None"))) %>%
  group_by(FIPS) %>%
  arrange(FIPS, desc(area_pct), drought_lvl) %>%
  slice(1) %>%
  rename(county_fips = FIPS) %>%
  view()


# view color palettes of interest -----------------------------------------

# brewer.pal(n=8, name = "YlOrRd")
# display.brewer.pal(n=8, name = "YlOrRd")


# plot map ---------------------------------------------------------------------

countydata <- dr_county %>%
  left_join(counties, by = "county_fips")

state <- states

colors <- c("#B10026", "#FD8D3C", "#FED976", "#FFEDA0", "#FFFFCC", "white")


ggplot(data = countydata, mapping = aes(long, lat, group = group, fill = drought_lvl)) +
  geom_polygon(color = "#ffffff", size = 0.0001) +
  geom_polygon(data = state,
               mapping = aes(x = long, y = lat, group = group),
               color = "light grey",
               fill = NA,
               size = 0.2) +
  scale_fill_manual(name = "Drought level",
                    values = colors,
                    labels = c("Exceptional drought", "Extreme drought", "Severe drought", "Moderate drought", "Abnormally dry", "None")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Current drought intensity in the US by country (data from 13 July 2021)",
       subtitle = "Drought levels based on highest percentage area for each county", 
       y = NULL,
       x = NULL,
       caption = "Source: US Drought Monitor | Tidy Tuesday 2021, Week 30"
  )

# save a copy of plot (remove commenting to use this part of code)

# library(here)
# ggsave(here::here("2021", "2021-week30", "drought_county_map.png"))