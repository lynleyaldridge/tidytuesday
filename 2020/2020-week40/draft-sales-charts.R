library(tidyverse)

sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')



sales %>%
  filter(country %in% c("WW", "World")) %>%
  mutate(title = fct_reorder(title, sales)) %>%
  ggplot(aes(sales, title, fill = artist)) +
  geom_col() +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "Sales (Worldwide)", y = "Album") +
  theme_minimal()
  