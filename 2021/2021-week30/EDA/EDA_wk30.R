
# install packages --------------------------------------------------------

# install.packages("ggridges")
# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnmapr")
# install.packages("viridis")
# install.packages("RColorBrewer")
# install.packages("geofacet")

# load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(ggridges)
library(devtools)
library(urbnmapr)
library(ggtext) 
library(here)
library(viridis)
library(RColorBrewer)
library(geofacet)

# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 30)
drought <- tuesdata$drought

# change path once uploaded
county_cumulative <- read_csv(here("2021", "2021-week30", "data", "county.csv"))
county_categorical <- read_csv(here("2021", "2021-week30", "data", "county_categorical.csv"))


# notes and credits -------------------------------------------------------

# AKLongmuir's (@alyssatweeting) 20 July 2021 twitch stream
# The Tidiest of Tuesdays

# https://kpress.dev/blog/2021-07-20-tidy-tuesday-drought-conditions/

# https://urban-institute.medium.com/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
# https://urbaninstitute.github.io/urbnmapr/articles/introducing-urbnmapr.html
# https://urbaninstitute.github.io/r-at-urban/mapping.html

# https://stackoverflow.com/questions/23161897/how-to-change-labels-legends-in-ggplot

# https://meghan.rbind.io/blog/color-legend/

# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/

# blank map ---------------------------------------------------------------

ggplot() +
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)


# state data --------------------------------------------------------------

statedata <- drought %>%
  filter(map_date == "20210713", drought_lvl == "D0") %>%
  select(map_date, state_abb, drought_lvl, area_pct) %>%
  rename(state_abbv = state_abb) %>%
  view()

statedata %>%
  left_join(states, by = "state_abbv") %>%
  ggplot(mapping = aes(long, lat, group = group, fill = area_pct)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Percentage in drought")


# county data - cumulative -------------------------------------------------------------

glimpse(county)

county <- county %>%
  rename(county_fips = FIPS)

countydata <- county %>%
  left_join(counties, by = "county_fips")

countydata %>%
  ggplot(aes(long, lat, group = group, fill = D0)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)


# county data - categorical -----------------------------------------------

glimpse(county_categorical)

dr_county <- county_categorical %>%
  pivot_longer(cols = None:D4, names_to = "drought_lvl", values_to = "area_pct") %>%
  mutate(drought_lvl = factor(drought_lvl, levels = c("D4", "D3", "D2", "D1", "D0", "None"))) %>%
  group_by(FIPS) %>%
  arrange(FIPS, desc(area_pct), drought_lvl) %>%
  slice(1) %>%
  rename(county_fips = FIPS) %>%
  view()

countydata <- dr_county %>%
  left_join(counties, by = "county_fips")

state <- states

countydata %>%
  ggplot(aes(long, lat, group = group, fill = drought_lvl)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

ggplot(data = countydata, mapping = aes(long, lat, group = group, fill = drought_lvl)) +
  geom_polygon(color = "#ffffff", size = 0.0001) +
  geom_polygon(data = state,
               mapping = aes(x = long, y = lat, group = group),
               color = "light grey",
               fill = NA,
               size = 0.2) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)


colors <- c("#B10026", "#FD8D3C", "#FED976", "#FFEDA0", "#FFFFCC", "white")

brewer.pal(n=8, name = "YlOrRd")
display.brewer.pal(n=8, name = "YlOrRd")

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
  labs(title = "US drought levels by county",
       subtitle = "Drought levels classified based on classification applying to the largest area of the county (data from 13 July 2021)", 
       y = NULL,
       x = NULL,
       caption = "Source: US Drought Monitor | Tidy Tuesday 2021, Week 30"
       )


# The U.S. Drought Monitor is jointly produced by the National Drought Mitigation Center \n at the University of Nebraska-Lincoln, the United States Department of Agriculture, and the National Oceanic \n and Atmospheric Administration. Map courtesy of NDMC (all errors my own)

# percent (area) of state in severe, extreme and exceptional drought categories by state (using geofacet) ----------------------------------------------------------

# Geofaceted graph made up of bar plots showing the percentage of each state in severe, extreme and exceptional drought, arranged into a grid resembling the geography of the United States. Drought intensity is worst in the west and high plans regions, with more than 60% of Arizona and Utah in exceptional drought conditions. Nevada, California, Oregon, and North Dakota are also experiencing state-wide droughts, with 90% or more of these states experiencing severe, extreme or exceptional drought. 
# https://edition.cnn.com/2021/06/17/weather/west-california-drought-maps/index.html

drought <- drought %>%
  mutate(drought_lvl = factor(drought_lvl, levels = c("D4", "D3", "D2", "D1", "D0", "None"))) %>%
  filter(map_date == "20210713") %>%
  select(map_date, state_abb, drought_lvl, area_pct) %>%
  filter(drought_lvl %in% c("D2", "D3", "D4")) 

colors <- c("#B10026", "#FD8D3C", "#FED976")

urban_grid <- tibble(
  row = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
          4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 
          7, 7, 8, 8, 8),
  col = c(1, 11, 6, 10, 11, 1, 2, 3, 4, 5, 6, 7, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 3, 4, 5, 6, 7, 8, 9, 4, 5, 6, 7, 8, 1, 4, 9),
  code = c("AK", "ME", "WI", "VT", "NH", "WA", "ID", "MT", "ND", "MN", "IL", "MI", "NY", "MA", "OR", "NV", "WY", "SD", "IA", "IN", "OH", "PA", "NJ", "CT", "RI", "CA", "UT", "CO", "NE", "MO", "KY", "WV", "VA", "MD", "DE", "AZ", "NM", "KS", "AR", "TN", "NC", "SC", "DC", "OK", "LA", "MS", "AL", "GA", "HI", "TX", "FL"),
  name = c("Alaska", "Maine", "Wisconsin", "Vermont", "New Hampshire", "Washington", "Idaho", "Montana", "North Dakota", "Minnesota", "Illinois", "Michigan", "New York", "Massachusetts", "Oregon", "Nevada", "Wyoming", "South Dakota", "Iowa", "Indiana", "Ohio", "Pennsylvania", "New Jersey", "Connecticut", "Rhode Island", "California", "Utah", "Colorado", "Nebraska", "Missouri", "Kentucky", "West Virginia", "Virginia", "Maryland", "Delaware", "Arizona", "New Mexico", "Kansas", "Arkansas", "Tennessee", " North Carolina", "South Carolina", " District of Columbia", "Oklahoma", "Louisiana", "Mississippi", "Alabama", "Georgia", "Hawaii", "Texas", "Florida")
)

ggplot(data = drought, aes(x = drought_lvl, y = area_pct, fill = drought_lvl)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_geo(facets = ~state_abb, grid = urban_grid) +
  scale_fill_manual(values = colors) +
  labs(title = "Current drought intensity in the US (data from 13 July 2021)",
      subtitle = "Percent of state in <span style = 'color:#FED976'>**severe**</span>, <span style = 'color:#FD8D3C'>**extreme**</span>, and <span style = 'color:#B10026'>**exceptional**</span> drought",
       x = NULL,
       y = NULL, 
       caption = "Source: US Drought Monitor | Tidy Tuesday 2021, Week 30") +
  theme(plot.subtitle = ggtext::element_markdown(),
        plot.background = element_rect(colour = "white"),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(margin = margin(t = 2)),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8L),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "black", 
                                    fill = NA, 
                                    size = 0.5),
        strip.background = element_rect(fill = "grey85", 
                                        colour = "black", 
                                        size = 0.5),
        axis.ticks.length = unit(1L, "pt"),
        strip.text.x = element_text(margin = margin(t = 1, b = 1), size = 11))

# alternative with four categories

drought <- drought %>%
  mutate(drought_lvl = factor(drought_lvl, levels = c("D4", "D3", "D2", "D1", "D0", "None"))) %>%
  filter(map_date == "20210713") %>%
  select(map_date, state_abb, drought_lvl, area_pct) %>%
  filter(drought_lvl %in% c("D1", "D2", "D3", "D4")) 

# looking at percentages of states - which are in total drought
drought %>%
  filter(state_abb %in% c("OR", "CA", "ID", "NV", "UT", "ND")) %>%
  group_by(state_abb) %>%
  summarize(total = sum(area_pct)) %>%
  view()

drought %>%
  group_by(state_abb) %>%
  summarize(total = sum(area_pct)) %>%
  filter(total > 80) %>%
  view()

colors <- c("#B10026", "#FD8D3C", "#FED976", "#FFFFCC")

urban_grid <- tibble(
  row = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
          4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 
          7, 7, 8, 8, 8),
  col = c(1, 11, 6, 10, 11, 1, 2, 3, 4, 5, 6, 7, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 3, 4, 5, 6, 7, 8, 9, 4, 5, 6, 7, 8, 1, 4, 9),
  code = c("AK", "ME", "WI", "VT", "NH", "WA", "ID", "MT", "ND", "MN", "IL", "MI", "NY", "MA", "OR", "NV", "WY", "SD", "IA", "IN", "OH", "PA", "NJ", "CT", "RI", "CA", "UT", "CO", "NE", "MO", "KY", "WV", "VA", "MD", "DE", "AZ", "NM", "KS", "AR", "TN", "NC", "SC", "DC", "OK", "LA", "MS", "AL", "GA", "HI", "TX", "FL"),
  name = c("Alaska", "Maine", "Wisconsin", "Vermont", "New Hampshire", "Washington", "Idaho", "Montana", "North Dakota", "Minnesota", "Illinois", "Michigan", "New York", "Massachusetts", "Oregon", "Nevada", "Wyoming", "South Dakota", "Iowa", "Indiana", "Ohio", "Pennsylvania", "New Jersey", "Connecticut", "Rhode Island", "California", "Utah", "Colorado", "Nebraska", "Missouri", "Kentucky", "West Virginia", "Virginia", "Maryland", "Delaware", "Arizona", "New Mexico", "Kansas", "Arkansas", "Tennessee", " North Carolina", "South Carolina", " District of Columbia", "Oklahoma", "Louisiana", "Mississippi", "Alabama", "Georgia", "Hawaii", "Texas", "Florida")
)

ggplot(data = drought, aes(x = drought_lvl, y = area_pct, fill = drought_lvl)) +
  geom_col() +
  coord_flip() +
  facet_geo(facets = ~state_abb, grid = urban_grid) +
  scale_fill_manual(name = "Drought level",
                    values = colors,
                    labels = c("Exceptional drought", "Extreme drought", "Severe drought", "Moderate drought")) +
  labs(title = "Percent of state (by area) in moderate, severe, extreme, and exceptional drought (13 July 2021)",
       x = NULL,
       y = NULL, 
       caption = "Source: US Drought Monitor | Tidy Tuesday 2021, Week 30") +
  theme(plot.background = element_rect(colour = "white"),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(margin = margin(t = 2)),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8L),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "black", 
                                    fill = NA, 
                                    size = 0.5),
        strip.background = element_rect(fill = "grey85", 
                                        colour = "black", 
                                        size = 0.5),
        axis.ticks.length = unit(1L, "pt"),
        strip.text.x = element_text(margin = margin(t = 1, b = 1), size = 11))


# explore data ------------------------------------------------------------

glimpse(drought)

# this code verifies percentages are cumulative
# now sum to 100% because statistic type has been changed to categorical

drought %>%
  filter(map_date == "20210713") %>%
  select(map_date, state_abb, drought_lvl, area_pct) %>%
  group_by(state_abb) %>%
  summarize(total_pct = sum(area_pct)) %>%
  filter(total_pct > 100)

# trying to see if this is the case for all map_dates, maybe just look at one state?

drought %>%
  select(map_date, state_abb, drought_lvl, area_pct) %>%
  group_by(map_date, state_abb) %>%
  filter(state_abb == "CO") %>%
  summarize(total_pct = sum(area_pct)) %>%
  filter(total_pct > 100)

# code along -  AKLongmuir's (@alyssatweeting) final code (20 July 2021)  --------------------------------------------------------------

drought_level <- drought %>%
  group_by(valid_start, state_abb) %>%
  mutate(max_level = max(area_pct)) %>%
  filter(max_level == area_pct)  %>%
  view()

drought_level <- drought_level %>%
  mutate(level = case_when(
    str_detect(drought_lvl, "None") ~ "0",
    str_detect(drought_lvl, "D0") ~ "1",
    str_detect(drought_lvl, "D1") ~ "2",
    str_detect(drought_lvl, "D2") ~ "3",
    str_detect(drought_lvl, "D3") ~ "4")) 

drought_level <- drought_level %>%
  ungroup() %>%
  group_by(state_abb) %>%
  mutate(av_drought = mean(as.numeric(level)))

drought_level$drought_lvl <- factor(drought_level$drought_lvl, levels = c("None", "D0", "D1", "D2", "D3", "D4"))

drought_level_long <- uncount(drought_level, as.numeric(level))


# code along -  AKLongmuir's (@alyssatweeting) twitch stream (20 July 2021)  --------------------------------------------------------------

drought_level <- drought %>%
  group_by(valid_start, state_abb) %>%
  mutate(max_level = max(area_pct)) %>%
  filter(max_level == area_pct) %>%
  view()

ggplot(drought_level, aes(x = valid_start, y = drought_lvl))+
  geom_point()

ggplot(drought_level, aes(x = valid_start, y = state_abb, colour = drought_lvl))+
  geom_point()
  
ggplot(drought_level, aes(x = valid_start, y = drought_lvl))+
  geom_density_ridges()

ggplot(drought_level, aes(x = valid_start, y = drought_lvl))+
  geom_line()+
  facet_wrap(~state_abb)

drought_level$drought_lvl <- factor(drought_level$drought_lvl, levels = c("None", "D0", "D1", "D2", "D3", "D4"))

col <- drought_level %>%
  filter(state_abb == "CO")

ggplot(col, aes(x = valid_start, y = drought_lvl, group = state_abb))+
  geom_line()

ggplot(col, aes(x = valid_start, y = drought_lvl, group = state_abb))+
  geom_smooth()

ggplot(col, aes(x = valid_start, y = drought_lvl, group = state_abb))+
  geom_path()

ggplot(drought_level, aes(x = valid_start, y = drought_lvl, group = state_abb, colour = state_abb))+
  geom_smooth()

drought_level <- drought_level %>%
  mutate(level = case_when(
    str_detect(drought_lvl, "None") ~ "0",
    str_detect(drought_lvl, "D0") ~ "1",
    str_detect(drought_lvl, "D1") ~ "2",
    str_detect(drought_lvl, "D2") ~ "3",
    str_detect(drought_lvl, "D3") ~ "4")) %>%
    ungroup() %>%
    mutate(av_drought = mean(as.numeric(level)))

view(drought_level)

drought_level <- drought_level %>%
  slice(rep(1:n(), each = level))


ggplot(drought_level, aes(x = valid_start, y = state_abb, colour = av_drought)) +
  geom_density_ridges()


# https://twitter.com/MeghanMHall/status/1417249625648648197
# FYI for those using this week's #TidyTuesday data set: the data dictionary says area_pct is "Percent of state currently in that drought category" but (for some records) it's a cumulative variable, counting up from the D4 level. I was getting funky totals so had to investigate Right-pointing magnifying glass

# https://twitter.com/zerogetsamgow/status/1417373923511914501

# Drought levels are cumulative D1 = sum(D1,D2,D3,D4). Need to subtract D4 from D3 and D3 from D2 and D2 from D1. 
# group_by(valid_start) %>% arrange(desc(drought_lvl) %>% mutate(area_pct.thislevel=area_pct-lag(area_pct),
                                                                                                                                                                               area_pct.thislevel=coalece(area_pct.thislevel,area_pct)