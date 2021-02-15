
#load packages ------

library(tidyverse)
library(gt) # for summary tables
library(webshot) # for saving final table as .png image; requires 'webshot::install_phantomjs()' to install PhantomJS 

#load data --------

albums <- read.csv('https://raw.githubusercontent.com/lynleyaldridge/tidytuesday/main/2020/2020-week40/data/albums.csv')

# albums <- readRDS(here("2020", "2020-week40", "data", "albums.RDS"))
# albums <- read.csv(here("2020", "2020-week40", "data", "albums.csv"))

# write.csv(albums, here("2020", "2020-week40", "data", "albums.csv"))
# albums <- read.csv(here("2020", "2020-week40", "data", "albums.csv"))


# githubURL <- 'https://github.com/lynleyaldridge/tidytuesday/blob/main/2020/2020-week40/data/albums.RDS'
# load(url(githubURL))


# define functions ------

bar_chart <- function(value, color = "#795548"){
  glue::glue("<span style=\"display: inline-block; direction: ltr; border-radius: 4px; padding-right: 2px; background-color: {color}; color: {color}; width: {value}%\"> &nbsp; </span>") %>% 
    as.character() %>% 
    gt::html()
}

# format data frame -------

albums_with_urls  <- albums %>%
  filter(artist == "Taylor Swift") %>%
  drop_na() %>%
  mutate(percent_plot = map(other_percent, ~bar_chart(value = .x))) %>% 
  
  # create new column combining values from title and released columns
  mutate(title_released = paste0("**", title, "**", 
                                 "<br>", year)) %>% 
  
  # create new column containing urls to album art
  mutate(img = paste0("https://raw.githubusercontent.com/lynleyaldridge/tidytuesday/main/2020/2020-week40/img/", title, ".jpg")) %>%
  
  # select columns for inclusion in table in desired order  
  select(img, title_released, US_chart, UK_chart, WW_sales, US_sales, other_sales, other_percent, percent_plot)

# create table --------

gt(albums_with_urls) %>%
  
  # create headings spanning multiple columns
  tab_spanner(label = "Chart position", columns = vars(US_chart, UK_chart)) %>%
  tab_spanner(label = "Sales ($ million)", columns = vars(WW_sales, US_sales, other_sales)) %>%
  tab_spanner(label = "International sales", columns = vars(other_percent, percent_plot)) %>%
  
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
    WW_sales = "Total",
    US_sales = "US",
    other_sales = "Intl",
    other_percent = "%",
    percent_plot = "") %>%
  
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
                                                      "International sales")))%>%
  
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
                                               "other_sales",
                                               "other_percent"))) %>%
  
  tab_style(style = cell_text(align = 'left'),
            locations = cells_body(columns = c("percent_plot"))) %>% 
  
  # vertical alignment of cells
  tab_style(style = cell_text(v_align = "middle"), 
            locations = cells_body()) %>%
  
  # set width of columns
  cols_width(
    vars("percent_plot") ~ px(150)) %>%
  
  # set border color and width
  tab_options(
    container.width = px(675),
    table.border.top.color = '#795548',
    table.border.bottom.color = '#795548',
    heading.border.bottom.color = '#795548',
    column_labels.border.bottom.color = '#795548',
    table_body.hlines.color = '#795548',
    table_body.border.bottom.color = '#795548') %>%
  
  # create title for table, formatting as markdown
  tab_header(
    title = md("**Taylor Swift's Speak Now sold primarily to US audiences, but international sales comprised an increasing proportion of her sales for each subsequent album**"),
    subtitle = md("*Peak chart position and sales by album and location*")) %>%
  
  # create footnote for subtitle
  tab_footnote(
    footnote = md("<span style = 'color:#795548'>Excludes albums for which worldwide sales were unavailable - Taylor Swift and Folklore</span>"),
    locations = cells_title(groups = "subtitle")) %>%
  
  # create footnote for international sales column
  tab_footnote(
    footnote = md("<span style = 'color:#795548'>International sales represent total worldwide sales excluding US sales</span>"),
    locations = cells_column_labels(columns = "other_sales")) %>%
  
  # create source note for table, formatting as md and applying color
  tab_source_note(source_note = md("<span style = 'color:#795548'>Source: Billboard via Wikipedia, October 2020; Table: Modified from Georgios Karamanis</span>")) 

# %>%

# save a copy of plot (remove commenting to use this part of the code)
# gtsave(here::here("2020", "2020-week40", "plots", "swift-albums.png"))
