# examples from: https://themockup.blog/posts/2020-09-04-10-table-rules-in-r/
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-09-01

library(tidyverse)
library(janitor)
library(gt)


key_crop_yields <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv') 


yield_data <- key_crop_yields %>%
  janitor::clean_names() %>%
  filter(entity %in% c("China", "India", "Indonesia", "Mexico", "Pakistan", "United States")) %>%
  filter(between(year, 2013, 2017)) %>%
  rename_with(~str_remove(., "_tonnes_per_hectare")) %>%
  select(entity, year, maize, potatoes) %>%
  pivot_longer(cols = maize:potatoes, 
               names_to = "crop",
               values_to = "yield") %>%
  rename(country = entity)
  


yield_data_wide <- yield_data %>%
  pivot_wider(names_from = "year",
              values_from = "yield")
                               
plot_spark <- function(data){
  data %>%
    mutate(
      start = if_else(year == 2013, yield, NA_real_),
      end = if_else(year == 2017, yield, NA_real_)
    ) %>%
    tidyr::fill(start, end, .direction = "downup") %>%
    mutate(color = if_else(end - start < 0, "red", "blue")) %>%
    ggplot(aes(x = year, y = yield, color = color)) +
    geom_line(size = 15) +
    theme_void() +
    scale_color_identity() +
    theme(legend.position = "none")
}


yield_plots <- yield_data %>%
  filter(crop == "potatoes") %>%
  nest(yields = c(year, yield)) %>%
  mutate(plot = map(yields, plot_spark))


yield_data_wide %>%
  filter(crop == "potatoes") %>%
  mutate(ggplot = NA,
         pct_change = (round((`2017`/ `2013` - 1)*100, 1))) %>%
  select(-crop) %>%
  gt() %>%
  cols_label(
    ggplot = "2013-2017",
    pct_change = md("Percent Change")
  ) %>%
  fmt_number(2:6) %>%
  tab_style(
    style = list(
      cell_text(color = "red")
    ),
    locations = cells_body(
      columns = c("pct_change"),
      rows = pct_change <= 0
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "blue")
    ),
    locations = cells_body(
      columns = c("pct_change"),
      rows = pct_change > 0
    )
  ) %>%
  text_transform(
    locations = cells_body(columns = ggplot),
    fn = function(x) {
      map(yield_plots$plot, ggplot_image, height = px(15), aspect_ratio = 4)
    }
     ) %>%
  cols_width(cols = ggplot ~ px(100))
   