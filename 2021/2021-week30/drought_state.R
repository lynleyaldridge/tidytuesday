# Geofaceted bar charts, showing proportion of US states in specified categories of drought, code for which draws heavily on the following sources:

# https://urbaninstitute.github.io/r-at-urban/mapping.html

# load libraries ----------------------------------------------------------

library(tidyverse)
library(geofacet)
library(ggtext) 

# load and manipulate data ---------------------------------------------------------------

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

drought <- drought %>%
  filter(drought_lvl %in% c("D2", "D3", "D4"),
         map_date == "20210713") %>%
  mutate(drought_lvl = factor(drought_lvl, levels = c("D4", "D3", "D2"))) %>%
  select(state_abb, drought_lvl, area_pct) 


# plot chart --------------------------------------------------------------

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

# save a copy of plot (remove commenting to use this part of code)

# library(here)
# ggsave(here::here("2021", "2021-week30", "drought_state.png"))
