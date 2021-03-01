library(tidyverse)
library(scales)
library(ggflags)

sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')

# recode countries
sales <- sales %>%
  mutate(country = ifelse(country == "AUS", "AU", country)) %>%
  mutate(country = ifelse(country == "CAN", "CA", country)) %>%
  mutate(country = ifelse(country == "FRA", "FR", country)) %>%
  mutate(country = ifelse(country == "JPN", "JP", country)) %>%
  mutate(country = ifelse(country == "UK", "GB", country)) %>%
  mutate(country = ifelse(country == "World", "WW", country))

sales %>%
  filter((title %in% c("Lover") & country != "WW")) %>%
  mutate(code = tolower(country)) %>% 
  mutate(country = fct_reorder(country, sales)) %>%
  ggplot(aes(sales, country)) +
  geom_col() +
  geom_flag(x = 0, aes(country = code), size = 10) +
  labs(y = "Country", x = "Copies sold", title = "Sales of Taylor Swift's 'Lover' (2019) by country", 
       caption = "Data source: Billboard via Wikipedia, October 2020") +
  scale_x_continuous(labels = comma) +
  theme_minimal()

# save a copy of plot (remove commenting to use this part of the code)

#  library(here)
#  ggsave(here::here("2020", "2020-week40", "plots", "by-country.png"), height = 4, width = 7)