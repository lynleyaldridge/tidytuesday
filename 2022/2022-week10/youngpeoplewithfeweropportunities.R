# code draws on references on gt tables from Thomas Mock
# https://themockup.blog/posts/2020-10-31-embedding-custom-features-in-gt-tables/
# https://themockup.blog/posts/2020-09-04-10-table-rules-in-r/#rule-10-add-visualizations-when-appropriate

# flag images taken from https://www.flaticon.com/packs/countrys-flags, following@tashapiro:
# https://github.com/tashapiro/TidyTuesday/tree/master/2021/W49


# load libraries ----------------------------------------------------------

library(tidyverse)
library(gt)
library(countrycode)
library(glue)
library(fontawesome)
library(here)

# load and manipulate data ------------------------------------------------

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

top10sending <- erasmus %>%
  filter(activity_mob == "Transnational youth meetings") %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(sending_country_code) %>%
  summarise(total = sum(participants)) %>%
  arrange(desc(total)) %>%
  top_n(10) %>%
  select(-total)

# summary table and plots - sending countries all years -----------------------------------------------

summary <- top10sending %>% 
  left_join(erasmus) %>%
  filter(activity_mob == "Transnational youth meetings") %>%
  filter(sending_country_code != receiving_country_code) %>%
  mutate(year = case_when(academic_year == "2014-2015" ~ 2014,
                          academic_year == "2015-2016" ~ 2015,
                          academic_year == "2016-2017" ~ 2016,                       
                          academic_year == "2017-2018" ~ 2017,
                          academic_year == "2018-2019" ~ 2018,
                          academic_year == "2019-2020" ~ 2019,
                          TRUE ~ NA_real_)) %>%
  group_by(sending_country_code, year) %>%
  summarise(value = sum(participants[fewer_opportunities == "Yes"])) %>%
  ungroup()

summary_wide <- summary %>%
  pivot_wider(names_from = "year",
              values_from = "value") %>%
  mutate(country = countrycode(sourcevar = sending_country_code, origin = "eurostat",
                               destination = "country.name"),
         flag_URL = case_when(
           country == "Germany" ~ 'https://cdn-icons-png.flaticon.com/512/197/197571.png',
           country == "Belgium"~ 'https://cdn-icons-png.flaticon.com/512/197/197583.png',
           country == "Austria"~ 'https://cdn-icons-png.flaticon.com/512/197/197447.png',
           country == "France"~ 'https://cdn-icons-png.flaticon.com/512/197/197560.png',
           country == "United Kingdom"~ 'https://cdn-icons-png.flaticon.com/512/197/197374.png',
           country == "Romania"~ 'https://cdn-icons-png.flaticon.com/512/197/197587.png',
           country == "Spain"~ 'https://cdn-icons-png.flaticon.com/512/197/197593.png',
           country == "Poland"~ 'https://cdn-icons-png.flaticon.com/512/197/197529.png',
           country == "Netherlands"~ 'https://cdn-icons-png.flaticon.com/512/197/197441.png',
           country == "Italy"~ 'https://cdn-icons-png.flaticon.com/512/197/197626.png'))%>%
  select(flag_URL, country, `2014`:`2019`)


# https://themockup.blog/posts/2020-09-04-10-table-rules-in-r/
# https://www.liamdbailey.com/post/making-beautiful-tables-with-gt/

plot_spark <- function(data){
  data %>%
    mutate(
      start = if_else(year == "2014", value, NA_real_),
      end = if_else(year == "2019", value, NA_real_)
    ) %>%
    tidyr::fill(start, end, .direction = "downup") %>%
    mutate(color = if_else(end-start <0, "red", "blue")) %>%
    ggplot(aes(x = year, y = value, color = color)) +
    geom_line(size = 15) +
    theme_void() +
    scale_color_identity() +
    theme(legend.position = "none")
}

# SPARKLINE
# keep order consistent between two data sources

fewer_opportunities_plots <- summary %>%
  arrange(factor(sending_country_code, levels = c("DE", "BE", "AT", "FR", "UK", 
                                                  "RO", "ES", "PL", "NL", "IT"))) %>%
  nest(numbers = c(year, value)) %>%
  mutate(plot = map(numbers, plot_spark))



summary_wide %>%
  mutate(ggplot = NA) %>%
  mutate(change = (`2019` - `2018`)) %>%
  arrange(desc(change)) %>%
  select(flag_URL, country, ggplot, `2014`:`2019`, change) %>%
  gt() %>%
  tab_spanner(
    label = "Academic year beginning",
    columns = c(`2014`:`2019`)
  ) %>%
  cols_label(
    flag_URL = "",
    country = "",
    ggplot = "",
    change = md("Annual change 2018-2019")) %>%
  text_transform(
    locations = cells_body(columns = ggplot),
    fn = function(x){
      map(fewer_opportunities_plots$plot, ggplot_image, height = px(15), aspect_ratio = 4)
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = flag_URL),
    fn = function(x) {
      web_image(url = x,
                height = 12
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = change),
    fn = function(x) {
      
      change <- as.integer(x)
      
      choose_image <- function(x){
        if (x > 0){
          gt::html(glue("<span style='color:#1134A6;font-face:bold;font-size:10px;'>{x}</span>"), fontawesome::fa("arrow-up", fill = "#1134A6"))
        } else if (x <= 0){
          gt::html(glue("<span style='color:#DA2A2A;font-face:bold;font-size:10px;'>{x}</span>"), fontawesome::fa("arrow-down", fill = "#DA2A2A"))
        }
      }
      map(change, choose_image)
      
    }
  ) %>%
  cols_width(cols = country ~ px(130),
              cols = ggplot ~ px(120),
              cols = flag_URL ~ px(30),
              cols = change ~ px(120)) %>%
  tab_style(
    style = list(
      cell_text(color = "red")
    ),
    locations = cells_body(
      columns = change,
      rows = change <= 0
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "blue")
    ),
    locations = cells_body(
      columns = change,
      rows = change > 0
    )
  ) %>%
  cols_align(align = "right",
             columns = 8) %>%
  tab_header(
    title = "Increased participation of young people with fewer opportunities in Erasmus Transnational Youth Meetings",
    subtitle = md("Number of young people with fewer opportunities sent overseas by top 10 sending countries for academic years 2014-2015 to 2019-2020. <br> Ordered by annual change between 2018 and 2019.")) %>%
  tab_source_note(
    source_note = "Source: Data from data.europa, flag icons from flaticon.com  | TidyTuesday 2022, Week 10")

# %>%


# save a copy of table (remove commenting to use this part of code)

# gtsave(filename = here::here("2022", "2022-week10", "youngpeoplewithfeweropportunities.png"))
