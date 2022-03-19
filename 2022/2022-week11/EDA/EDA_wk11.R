# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(paletteer)
library(ggtext)

# load and manipulate data ------------------------------------------------

# cran packages

cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

# color palette data sourced from: EmilHvitfeldt
# https://github.com/EmilHvitfeldt/paletteer


# list of color packages

colors <- readxl::read_xlsx(here("2022", "2022-week11", "data", "colorpalettes.xlsx"),
                            sheet = "paletteer",
                            range = "A1:C68") %>%
  clean_names()

# join datasets and format dates
# https://stackoverflow.com/questions/37704212/extract-month-and-year-from-date-in-r

data <- colors %>%
  left_join(cran, by = c("name" = "package")) %>%
  mutate(
    date_r = parse_date_time(date, c("ymd HMS", "md HMS y")),
    date_r = as_date(date_r, tz = NULL)
#month_yr = format_ISO8601(date_r, precision = "ym")
  )

class(data$date_r)

# plot --------------------------------------------------------------------
# https://stackoverflow.com/questions/7450600/count-number-of-rows-per-group-and-add-result-to-original-data-frame
# https://betterfigures.org/2015/06/23/picking-a-colour-scale-for-scientific-graphics/


data %>%
  na.omit() %>%
  group_by(name) %>%
  mutate(count = n(),
         start = min(date_r)) %>%
  ungroup() %>%
  mutate(name = fct_reorder(name, start)) %>%
  ggplot(aes(x = date_r, y = name, color = count)) +
           geom_point(alpha = 0.5) +
  scale_x_date(date_labels = "%Y", breaks = "2 years", 
               limits = as.Date(c("2002-02-01", "2022-01-03")),
               labels = as.Date(c("2004-01-01", "2022-01-03"))) +
  scale_color_paletteer_c(`"scico::bilbao"`, name = "Updates") +
  geom_text(aes(x = start, y = name, label = name, hjust = 1.1, vjust = .25)) +
  labs(x = NULL, y = NULL,
       title = "Color palette packages available via CRAN 2004-2022",
       subtitle = "Packages are ordered by first upload, and colored by frequency of updates",
       caption = "Data R. M. Flight | Inspriration for plot BjnNowak | Color packages EmilHvitfeldt | Tidy Tuesday 2022, Week 11") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, color = "white"),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 14, color = "white"),
    plot.subtitle = element_text(size = 10, color = "white"),
    plot.caption = element_text(size = 10, color = "white"),
    plot.background = element_rect(fill = "#767676")
  )

# view(palettes_c_names)
