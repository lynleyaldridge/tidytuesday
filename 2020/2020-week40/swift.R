
#load packages
library(tidyverse)
library(here)
library(gt) # for summary tables

#load data
albums <- readRDS(here("static", "data", "albums.RDS"))

#albums <- readRDS("../../static/data/albums.RDS")

# https://blogdown-demo.rbind.io/2018/02/27/r-file-paths/
# look at what I did for resources and modify

gt(albums_with_urls) %>%
  
  # create headings spanning multiple columns
  tab_spanner(label = "Chart position", columns = vars(US_chart, UK_chart)) %>%
  tab_spanner(label = "Sales ($ million)", columns = vars(WW_sales, US_sales)) %>%
  tab_spanner(label = "US sales", columns = vars(US_percent, percent_plot)) %>%
  
  # set column labels 
  cols_label(
    img = "",
    title_released = html(
      "<div style = 'text-align:left;'>
        <span style='font-weight:bold'>Album</span><br> 
        <span style='font-weight:normal'>Released</span>
        </div>"),
    US_chart = "US",
    UK_chart = "UK",
    WW_sales = "World",
    US_sales = "US",
    US_percent = "%",
    percent_plot = "Plot") %>%
  
  # format title_released column as markdown text
  fmt_markdown(columns = c("title_released")) %>%
  
  # transform text in img column to display and appropriately size images
  text_transform(locations = cells_body(vars(img)),
                 fn = function(x){web_image(url = x, height = 80)}) %>%
  
  # color column labels and cells in table body 
  tab_style(style = cell_text(color = "#795548"),
            locations = list(
              cells_column_labels(everything()),
              cells_body()))%>%
  
  # color column spanners and make bold
  tab_style(style = cell_text(
    color = "#795548",
    weight = "bold"),
    locations = cells_column_spanners(spanners = vars("Chart position", 
                                                      "Sales ($ million)", 
                                                      "US sales")))%>%
  
  # color title and subtitle and set size
  tab_style(style = cell_text(color = "#795548", size = "large"),
            locations = cells_title(groups = "title")) %>%
  
  tab_style(style = cell_text(color = "#795548", size = "medium"),
            locations = cells_title(groups = "subtitle")) %>%
  
  # horizontal alignment of cells 
  tab_style(style = cell_text(align = 'center'),
            locations = cells_body(columns = 3:4)) %>%
  
  tab_style(style = cell_text(align = 'right'),
            locations = cells_body(columns = c("WW_sales", 
                                               "US_sales", 
                                               "US_percent"))) %>%
  
  tab_style(style = cell_text(align = 'left'),
            locations = cells_body(columns = c("percent_plot"))) %>% 
  
  # vertical alignment of cells
  tab_style(style = cell_text(v_align = "middle"), 
            locations = cells_body()) %>%
  
  # set width of columns
  cols_width(
    vars("title_released", "percent_plot") ~ px(150)) %>%
  
  # remove label for plot column
  cols_label(percent_plot = "") %>%
  
  # create title for table, formatting as markdown
  tab_header(
    title = md("**Taylor Swift's Speak Now sold primarily to US audiences, but her US percentage share of sales fell with each subsequent album**"),
    subtitle = md("*Peak chart position and sales by album and location*")) %>%
  
  # create source note for table, formatting as md and applying color
  tab_source_note(source_note = md("<span style = 'color:#795548'>Source: Billboard via Wikipedia, October 2020; excludes albums for which worldwide sales were unavailable <br>Table: Modified from Georgios Karamanis</span>")) 