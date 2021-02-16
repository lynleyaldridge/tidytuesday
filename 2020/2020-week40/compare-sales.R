
#load packages -------------

library(tidyverse)
library(gt) # for summary tables
library(webshot) # for saving final table as .png image; requires 'webshot::install_phantomjs()' to install PhantomJS 

#load data --------------
albums <- read.csv('https://raw.githubusercontent.com/lynleyaldridge/tidytuesday/main/2020/2020-week40/data/albums.csv')

# create summary data frame
# adapted from josep maria porrà: https://stackoverflow.com/questions/63041655/how-can-you-add-group-percentages-to-tables-using-the-gt-package 

albums_grouped <- albums %>%
  drop_na() %>%
  group_by(artist) %>%
  summarise_at(vars(US_sales, WW_sales), sum) %>%
  rowwise() %>%
  mutate(pct_US = US_sales/WW_sales)

# create dataframe and summary table

albums %>%
  
  select(title, artist, year, US_chart, UK_chart, US_sales, WW_sales, US_percent) %>%
  
  drop_na() %>%
  
  gt(rowname_col = "title", groupname_col = "artist") %>%
  
  cols_label(
    year = "Released",
    US_chart = "US",
    UK_chart = "UK",
    US_sales = "US",
    WW_sales = "Total",
    US_percent = "US sales (%)") %>%
  
  tab_spanner(label = "Chart position", columns = vars(US_chart, 
                                                       UK_chart)) %>%
  tab_spanner(label = "Sales ($ million)", columns = vars(US_sales,
                                                          WW_sales)) %>%
  
 # create summary rows for each group 
  summary_rows(groups = TRUE, 
               columns = vars(US_sales, WW_sales),
               fns = list(TOTAL = "sum"),
               formatter = fmt_number, decimals = 1) %>%
  summary_rows(groups = "Taylor Swift",
               columns = vars(US_percent), 
               fns = list(TOTAL = ~ 
                            albums_grouped %>%
                            filter(artist == "Taylor Swift") %>%
                            select(pct_US) %>%
                            pull()),
               formatter = fmt_percent, decimals = 1) %>%
  summary_rows(groups = "Beyoncé",
               columns = vars(US_percent), 
               fns = list(TOTAL = ~ 
                            albums_grouped %>%
                            filter(artist == "Beyoncé") %>%
                            select(pct_US) %>%
                            pull()),
               formatter = fmt_percent, decimals = 1) %>%
  
  # style summary rows and row group titles 
  tab_style(style = cell_text(color = "#795548", weight = "bold"),
            locations = list(cells_summary(groups = TRUE),
                             cells_row_groups(groups = TRUE))) %>%

  cols_align(align = "right", columns = c("US_chart", "UK_chart",
                                          "US_sales", "WW_sales",
                                          "US_percent")) %>%
  
  tab_style(style = cell_text(color = "#795548"),
            locations = list(
              cells_column_labels(everything()),
              cells_stub(rows = TRUE),
              cells_body()))%>%
  
  tab_style(style = cell_text(
    color = "#795548",
    weight = "bold"),
    locations = cells_column_spanners(spanners = vars("Chart position", 
                                                      "Sales ($ million)")))%>%
  
  tab_header(
    title = md(
      "**Taylor Swift has higher sales than Beyoncé, but owes a greater proportion of her success to US sales than Beyoncé**"),
    subtitle = md(
      "*Peak chart position, sales, and US sales as a percentage of total sales by album*")) %>%
  
  tab_source_note(
    source_note = md("<span style = 'color:#795548'>Source: Billboard via Wikipedia, October 2020</span>")) %>%
  
  tab_footnote(
    footnote = md("<span style = 'color:#795548'>Excludes 3 albums for which worldwide sales data was not available - Taylor Swift, Folklore, and 4</span>"),
    locations = cells_title(groups = "subtitle")) %>%
  
  tab_style(style = cell_text(color = "#795548", size = "large"),
            locations = cells_title(groups = "title")) %>%
  
  tab_style(style = cell_text(color = "#795548", size = "medium"),
            locations = cells_title(groups = "subtitle")) %>%
  
  tab_options(
    table.border.top.color = '#795548',
    table.border.bottom.color = '#795548',
    heading.border.bottom.color = '#795548',
    stub.border.color = '#795548',
    column_labels.border.bottom.color = '#795548',
    row_group.border.top.color = '#795548',
    row_group.border.bottom.color = '#795548',
    table_body.hlines.color = '#795548',
    table_body.border.bottom.color = '#795548',
    summary_row.border.color = '#795548') 

# %>%

# save image (remove commenting and load here library to use this part of the code)

# gtsave(here::here("2020", "2020-week40", "plots", "compare-sales.png"))
