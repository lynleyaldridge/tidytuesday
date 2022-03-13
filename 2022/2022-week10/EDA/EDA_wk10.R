# load libraries ----------------------------------------------------------

library(tidyverse)
library(gt)
library(countrycode)
library(glue)
library(fontawesome)
library(sjPlot)

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
  
# summary table and plots - sending all years (before I modify) -----------------------------------------------

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

#add flag images - images taken from https://www.flaticon.com/packs/countrys-flags following@tashapiro 2021/W49
# https://themockup.blog/posts/2020-10-31-embedding-custom-features-in-gt-tables/
# https://themockup.blog/posts/2020-09-04-10-table-rules-in-r/#rule-10-add-visualizations-when-appropriate

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
  cols_width(cols = ggplot ~ px(100),
             cols = flag_URL ~ px(30)) %>%
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
    subtitle = "Number of young people with fewer opportunities sent overseas by top 10 sending countries for academic years 2014-2015 to 2019-2020. Ordered by annual change between 2018 and 2019") %>%
  tab_source_note(
    source_note = "Data from data.Europa | #TidyTuesday wk10-2022")



# percentages -------------------------------------------------------------


summary_percent <- top10sending %>% 
  left_join(erasmus) %>%
  filter(activity_mob == "Transnational youth meetings") %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(sending_country_code, academic_year) %>%
  summarise(totalsent = sum(participants), 
            feweropportunitiessent = sum(participants[fewer_opportunities == "Yes"]),
            percentfeweropportunities = round(feweropportunitiessent/totalsent *100, 1)) %>%
  ungroup() %>%
  mutate(year = case_when(academic_year == "2014-2015" ~ 2014,
                          academic_year == "2015-2016" ~ 2015,
                          academic_year == "2016-2017" ~ 2016,                       
                          academic_year == "2017-2018" ~ 2017,
                          academic_year == "2018-2019" ~ 2018,
                          academic_year == "2019-2020" ~ 2019,
                          TRUE ~ NA_real_)) %>%
  mutate(country = countrycode(sourcevar = sending_country_code, origin = "eurostat",
                               destination = "country.name")) %>%
  select(country, year, percentfeweropportunities) %>%
  pivot_wider(names_from = "year",
              values_from = "percentfeweropportunities")


# other graphs ------------------------------------------------------------


ggplot(summary, mapping = aes(x = year, y = value, group = sending_country_code)) +
  geom_line() +
  facet_wrap(~sending_country_code, ncol = 2)


ggplot(summary, mapping = aes(x = year, y = value, group = sending_country_code)) +
  geom_line()


summary_long <- top10sending %>% 
  left_join(erasmus) %>%
  filter(activity_mob == "Transnational youth meetings") %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(academic_year, sending_country_code) %>%
  summarise(totalsent = sum(participants), 
            feweropportunitiessent = sum(participants[fewer_opportunities == "Yes"]),
            percentfeweropportunities = feweropportunitiessent/totalsent) %>%
  pivot_longer(!(c("academic_year", "sending_country_code")), 
               names_to = "variable",
               values_to = "value")


ggplot(summary_long %>%
         filter(variable != "percentfeweropportunities"), mapping = aes(x = academic_year, y = value, group = variable)) +
  geom_col(position = "dodge") +
  facet_wrap(~ sending_country_code)


ggplot(summary_long %>%
         filter(variable == "percentfeweropportunities"), mapping = aes(x = academic_year, y = value, group = variable)) +
  geom_line() +
  facet_wrap(~ sending_country_code)


# summary table and plots - receiving all years -----------------------------------------------


summary <- top10receiving %>% 
  left_join(erasmus) %>%
  filter(activity_mob == "Transnational youth meetings") %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(academic_year, receiving_country_code) %>%
  summarise(totalreceived = sum(participants), 
            feweropportunitiesreceived = sum(participants[fewer_opportunities == "Yes"]),
            percentfeweropportunities = feweropportunitiesreceived/totalreceived) 


ggplot(summary, mapping = aes(x = academic_year, y = percentfeweropportunities, group = receiving_country_code)) +
  geom_line()


summary_long <- top10receiving %>% 
  left_join(erasmus) %>%
  filter(activity_mob == "Transnational youth meetings") %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(academic_year, receiving_country_code) %>%
  summarise(totalreceived = sum(participants), 
            feweropportunitiesreceived = sum(participants[fewer_opportunities == "Yes"]),
            percentfeweropportunities = feweropportunitiesreceived/totalreceived) %>%
  pivot_longer(!(c("academic_year", "receiving_country_code")), 
               names_to = "variable",
               values_to = "value")


ggplot(summary_long %>%
         filter(variable != "percentfeweropportunities"), mapping = aes(x = academic_year, y = value, group = variable)) +
  geom_col(position = "dodge") +
  facet_wrap(~ receiving_country_code)




# raw code for percent special needs summary by academic year ------------------------------

crossborder_percentspecialneeds1415 <- erasmus %>%
  filter(activity_mob == "Transnational youth meetings") %>%
  filter(academic_year == "2014-2015") %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(sending_country_code) %>%
  summarise(totalsent = sum(participants), 
            specialneedssent = sum(participants[special_needs == "Yes"])) 
  
  




# looping across academic years (not ready to do this yet) --------------------------------------------

# vector of years of interest

years <- c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")


summarise.percentspecialneeds <- function(x) {
  
  years <- x
  
    output <- tibble::tibble(), length(years))
  
    for (i in seq_along(years)) {
    
      paste0("percentsending_",
           years[[i]]) <- raw_erasmus %>%
      filter(activity_mob == "Transnational youth meetings") %>%
      filter(academic_year == years[i]) %>%
      filter(sending_country_code != receiving_country_code) %>%
      group_by(sending_country_code) %>%
      summarise(totalsent = sum(participants), 
                specialneedssent = sum(participants[special_needs == "Yes"]),
                percentspecialneeds = specialneedssent/totalsent) 
  }
}  
  
summarise.percentspecialneeds(years)



# other summary -----------------------------------------------------------

str(erasmus)

summary(erasmus)
view_df(erasmus, show.string.values = TRUE)


curious <- erasmus %>%
  mutate(internal = case_when(sending_country_code == receiving_country_code ~ 1,
                              TRUE ~ 0)) %>%
  filter(internal == 1)

domestic <- erasmus %>%
  filter(sending_country_code == receiving_country_code)

crossborder <- erasmus %>%
  filter(sending_country_code != receiving_country_code)

crossborder %>%
  group_by(activity_mob) %>%
  summarise(total = n()) %>%
  arrange(-(total))

erasmus %>%
  group_by(activity_mob) %>%
  summarise(total = n()) %>%
  arrange(-(total))

erasmus %>%
  group_by(participant_nationality) %>%
  summarise(total = n()) %>%
  arrange(-(total))

top10receiving <- erasmus %>%
  group_by(receiving_country_code) %>%
  summarise(total = sum(participants)) %>%
  arrange(desc(total)) %>%
  top_n(10)


# plot year-on-year differences -------------------------------------------

# summary table and plots - sending all years -----------------------------------------------
# https://stackoverflow.com/questions/30606360/subtract-value-from-previous-row-by-group

summary <- top10sending %>% 
  left_join(raw_erasmus) %>%
  filter(activity_mob == "Transnational youth meetings") %>%
  filter(sending_country_code != receiving_country_code) %>%
  mutate(period = case_when(academic_year == "2014-2015" ~ 1,
                            academic_year == "2015-2016" ~ 2,
                            academic_year == "2016-2017" ~ 3,
                            academic_year == "2017-2018" ~ 4,
                            academic_year == "2018-2019" ~ 5,
                            academic_year == "2019-2020" ~ 6,
                            TRUE ~ NA_real_)) %>%
  group_by(sending_country_code, academic_year, period) %>%
  summarise(totalsent = sum(participants), 
            feweropportunitiessent = sum(participants[fewer_opportunities == "Yes"]),
            percentfeweropportunities = feweropportunitiessent/totalsent) %>%
  group_by(sending_country_code) %>%
  arrange(period, .by_group = TRUE) %>%
  mutate(diff = feweropportunitiessent - lag(feweropportunitiessent, default = first(feweropportunitiessent)))



ggplot(summary, mapping = aes(x = academic_year, y = diff, group = sending_country_code)) +
  geom_point() +
  facet_wrap(~sending_country_code)



# pivoting example --------------------------------------------------------


summary_wide <- summary %>%
  pivot_longer(cols = !(c("academic_year", "sending_country_code", "country")), 
               names_to = "variable",
               values_to = "value",
               values_transform = list(value = as.character)) %>%
  pivot_wider(names_from = "academic_year",
              values_from = "value")



# summary table with summary pasted as text (convert to character variable in pivot_longer) --------------------------------


summary <- top10sending %>% 
  left_join(erasmus) %>%
  filter(activity_mob == "Transnational youth meetings") %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(sending_country_code, academic_year) %>%
  summarise(totalsent = sum(participants), 
            feweropportunitiessent = sum(participants[fewer_opportunities == "Yes"]),
            percentfeweropportunities = round(feweropportunitiessent/totalsent *100, 1)) %>%
  mutate(new = paste0(round(feweropportunitiessent, 0),
                      " of ",
                      round(totalsent, 0),
                      " (",
                      round(percentfeweropportunities, 1),
                      "%)")) %>%
  ungroup()

summary$country <- countrycode(summary$sending_country_code, "eurostat", "country.name")


summary_wide <- summary %>%
  pivot_longer(cols = !(c("academic_year", "sending_country_code", "country")), 
               names_to = "variable",
               values_to = "value",
               values_transform = list(value = as.character)) %>%
  pivot_wider(names_from = "academic_year",
              values_from = "value") 


summary_wide %>%
  select(country, variable, "2014-2015":"2019-2020") %>%
  group_by(country) %>%
  gt()


# before amendments -------------------------------------------------------


# summary table and plots - sending all years -----------------------------------------------

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
  summarise(feweropportunitiessent = sum(participants[fewer_opportunities == "Yes"]),
            totalsent = sum(participants)) %>%
  ungroup()


summary$country <- countrycode(summary$sending_country_code, "eurostat", "country.name")

summary_long <- summary %>%
  pivot_longer(cols = !(c("academic_year", "sending_country_code", "country")), 
               names_to = "variable",
               values_to = "value") 


summary_wide <- summary %>%
  pivot_wider(names_from = "academic_year",
              values_from = "value") %>%
  rowwise() %>%
  mutate(total = sum(c_across(where(is.numeric)))) %>%
  arrange(desc(total))

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

fewer_opportunities_plots <- summary_long %>%
  filter(variable == "feweropportunitiessent") %>%
  select(country, year, variable, value) %>%
  nest(numbers = c(year, value)) %>%
  mutate(plot = map(numbers, plot_spark)) %>%
  arrange(factor(country, levels = c("Germany", "Belgium", "United Kingdom", "France", "Austria", 
                                     "Romania", "Spain", "Poland", "Netherlands", "Italy")))


summary_wide %>%
  filter(variable == "feweropportunitiessent") %>%
  mutate(ggplot = NA,
         iso2 = countrycode(sourcevar = sending_country_code, origin = "eurostat",
                            destination = "fips"),
         flag_URL = glue('https://www.cia.gov/library/publications/the-world-factbook/attachments/flags/{iso2}-flag.jpg')) %>%
  select(-c("sending_country_code", "variable", "total")) %>%
  mutate(change = (`2019-2020` - `2018-2019`)) %>%
  arrange(desc(change)) %>%
  gt(rowname_col = "country") %>%
  text_transform(
    locations = cells_body(columns = ggplot),
    fn = function(x){
      map(fewer_opportunities_plots$plot, ggplot_image, height = px(15), aspect_ratio = 4)
    }
  ) %>%
  cols_width(cols = ggplot ~ px(100)) %>%
  cols_label(
    ggplot = "Trendline: 2014-2019",
    change = md("Annual change")
  ) %>%
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
    title = "Participation of young people with fewer opportunities in Erasmus Transnational Youth Meetings",
    subtitle = "Participants in overseas exchanges by top 10 sending countries for academic years 2014-2015 to 2019-2020. Ordered by annual change between 2018-2019 and 2019-2020.") %>%
  tab_source_note(
    source_note = "Data from data.Europa | #TidyTuesday wk10-2022")



# old flags from cia - superceded -----------------------------------------


summary_wide <- summary %>%
  pivot_wider(names_from = "year",
              values_from = "value") %>%
  mutate(country = countrycode(sourcevar = sending_country_code, origin = "eurostat",
                               destination = "country.name"),
         iso2 = countrycode(sourcevar = sending_country_code, origin = "eurostat",
                            destination = "fips"),
         flag_URL = glue('https://www.cia.gov/library/publications/the-world-factbook/attachments/flags/{iso2}-flag.jpg')) %>%
  select(flag_URL, country, `2014`:`2019`)

# summary table and plots - sending all years (before I modify) -----------------------------------------------

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

#add flag images - images taken from https://www.flaticon.com/packs/countrys-flags following@tashapiro 2021/W49
# https://themockup.blog/posts/2020-10-31-embedding-custom-features-in-gt-tables/
# https://themockup.blog/posts/2020-09-04-10-table-rules-in-r/#rule-10-add-visualizations-when-appropriate

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
  cols_width(cols = ggplot ~ px(100),
             cols = flag_URL ~ px(30)) %>%
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
  subtitle = "Number of young people with fewer opportunities sent overseas by top 10 sending countries for academic years 2014-2015 to 2019-2020. Ordered by annual change between 2018 and 2019") %>%
  tab_source_note(
    source_note = "Data from data.Europa | #TidyTuesday wk10-2022")



