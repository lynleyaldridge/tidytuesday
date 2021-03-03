# set up and loading data

# install packages - run once
# install.packages("tidytuesdayR") # not needed 

# load libraries ----------------------------
library(tidytuesdayR)
library(tidyverse)
library(summarytools)
library(gt)
library(here)
library(ggtext) # for formatting text in graph titles as markdown, use of element_textbox_simple

# read in data -------------------------------

# read in data with tidytuesdayR
# tuesdata <- tidytuesdayR::tt_load(2021, week = 9)
# employed <- tuesdata$employed
# earn <- tuesdata$earn

# alternative for reading in data manually
employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

# save to csv

write_csv(employed, here("2021", "2021-week9", "data", "employed.csv"))

# explore data -------------------------------

# view employed
glimpse(employed)

summarytools::freq(employed$race_gender)

# create summary data frames for cross-checking ----------------

## employed by race ----------

emp_by_race <- employed %>%
  filter(year == 2020) %>%
  select(race_gender, employ_n) %>%
  drop_na() %>%
  group_by(race_gender) %>%
  summarize_at(vars(employ_n), sum) %>%
  mutate(thousands = employ_n/1000)

## employed and employment share by occupation and race ----------

summary_data <- employed %>%
  filter(year == 2020) %>%
  select(minor_occupation, race_gender, employ_n) %>%
  drop_na() %>%
  filter(race_gender %in% c("Black or African American", "TOTAL")) %>%
  group_by(race_gender, minor_occupation) %>%
  summarize_at(vars(employ_n), sum) %>%
  mutate(percent = employ_n/sum(employ_n)*100) %>%
  mutate(thousands = employ_n/1000) %>%
  pivot_wider(names_from = race_gender, values_from = c(employ_n, thousands, percent))

# gt table (full table for checking): employment share by occupation  --------------------

summary_data %>%
  select(minor_occupation, `thousands_Black or African American`, thousands_TOTAL, 
         `percent_Black or African American`, percent_TOTAL) %>%
  arrange(desc(`percent_Black or African American`)) %>%
  gt() %>%
  cols_label(
    minor_occupation = "Occupational group",
    `thousands_Black or African American` = "Black or African American",
    thousands_TOTAL = "Total",
    `percent_Black or African American` = "Black or African American",
    percent_TOTAL = "Total workers") %>%
  tab_spanner(label = "Workers (thousands)",
              columns = vars(`thousands_Black or African American`,
                               thousands_TOTAL)) %>%
  tab_spanner(label = "Share (%)",
              columns = vars(`percent_Black or African American`,
                             percent_TOTAL)) %>%
  fmt_number(columns = vars(`percent_Black or African American`,
                            percent_TOTAL),
             decimals = 1) %>%
  summary_rows(columns = 2:5,
               fns = list(Total = "sum"),
               formatter = fmt_number, decimals = 0) %>%
  tab_header(
    title = md(
      "**Occupations with the largest concentration of Black or African American workers**"),
    subtitle = md(
      "*Annual average 2020*")) %>%
  tab_source_note(
    source_note = md("Source: Tidy Tuesday Week 9, 2021 | U.S Bureau of Labor Statistics, Current Population Survey"))
  

## gt table (final summary table): employment share by occupation 

summary_data %>%
  select(minor_occupation, `percent_Black or African American`, percent_TOTAL) %>%
  arrange(desc(`percent_Black or African American`)) %>%
  gt() %>%
  cols_label(
    minor_occupation = "Occupational group",
    `percent_Black or African American` = "Share of Blacks or African Americans (%)",
    percent_TOTAL = "Share of total workers (%)") %>%
  fmt_number(columns = vars(`percent_Black or African American`,
                            percent_TOTAL),
             decimals = 1) %>%
  tab_header(
    title = md(
      "**Occupations with the largest concentration of Black or African American workers**"),
    subtitle = md(
      "*Annual average 2020*")) %>%
  tab_source_note(
    source_note = md("Source: Tidy Tuesday Week 9, 2021 | U.S Bureau of Labor Statistics, Current Population Survey"))

# Cleveland dot plot -----------
# https://uc-r.github.io/cleveland-dot-plots

# Also drew on:
# https://twitter.com/DanIrwinBrown/status/1365703400793509889
# https://github.com/djpib/tidy.tuesday/blob/main/employed_status.R

# create dataframe for plotting

occupation_race <- employed %>%
  filter(year == 2020) %>%
  select(minor_occupation, race_gender, employ_n) %>%
  drop_na() %>%
  filter(race_gender %in% c("Black or African American", "TOTAL")) %>%
  group_by(race_gender, minor_occupation) %>%
  summarize_at(vars(employ_n), sum) %>%
  mutate(percent = employ_n/sum(employ_n)*100) %>%
  select(-employ_n) %>%
  ungroup() %>%
  mutate(minor_occupation = fct_rev(fct_reorder2(minor_occupation, race_gender=="TOTAL", percent)))

# create dataframe identifying large differences for highlighting

big_diff <- occupation_race %>%
  pivot_wider(names_from = race_gender, values_from = percent) %>%
  mutate(diff = abs(`Black or African American` - `TOTAL`)) %>%
  arrange(desc(diff)) %>%
  filter(diff> 1.8)

# create label data frames for formatting of labels

right_label <- occupation_race %>%
  group_by(minor_occupation) %>%
  arrange(desc(percent)) %>%
  top_n(1)

left_label <- occupation_race %>%
  group_by(minor_occupation) %>%
  arrange(desc(percent)) %>%
  slice(2)

# filter label dataframes to include only those with difference > 1.8 percentage points

right_label <- filter(right_label, minor_occupation %in% big_diff$minor_occupation)
left_label <- filter(left_label, minor_occupation %in% big_diff$minor_occupation)

# filter main data frame to include only occupations with difference > 1.8 percentage points

highlight <- filter(occupation_race, minor_occupation %in% big_diff$minor_occupation)

# code for graph

ggplot(occupation_race, aes(percent, minor_occupation)) +
  geom_line(aes(group = minor_occupation), alpha = .5) +
  geom_point(aes(color = race_gender), size = 1.5, alpha = .5) +
  geom_line(data = highlight, aes(group = minor_occupation)) +
  geom_point(data = highlight, aes(color = race_gender), size = 2) +
  geom_text(data = right_label, aes(color = race_gender, label = round(percent, 1)),
                                    size = 3, hjust = - 0.5) +
  geom_text(data = left_label, aes(color = race_gender, label = round(percent, 1)),
            size = 3, hjust = 1.5) +
  scale_x_continuous(limits = c(-2.5, 27.5), breaks = c(0, 5, 10, 15, 20, 25)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 15),
        plot.subtitle = element_textbox_simple(size = 12),
        plot.title.position = "plot")+
  labs(y = "",
       x = "Share of workers (%)",
       title = "Employment by occupational group: Annual average (2020)",
       subtitle = "<b style = 'color:#F8766D'>Black or African American workers</b> are underrepresented in professional and \n management occupations, 
       and overrepresented in service and transportation, relative to <b style = 'color:#00BFC4'>all workers</b>",
       caption = "Data: U.S Bureau of Labor Statistics, Current Population Survey | Tidy Tuesday 2021, Week 9")


  
# useful expression for seeing what colors are use to make a plot (plus other details)
# source: https://stackoverflow.com/questions/25211078/what-are-the-default-plotting-colors-in-r-or-ggplot2
# ggplot_build(p)$data



# What about a Cleveland dot plot based on this example:
# https://uc-r.github.io/cleveland-dot-plots

# Also drew on:
# https://twitter.com/DanIrwinBrown/status/1365703400793509889

# https://edav.info/cleveland.html
# https://mbounthavong.com/blog/tag/Cleveland+dot+plots (includes other references)


# examples
# https://twitter.com/DanIrwinBrown/status/1365703400793509889/photo/1
# https://github.com/djpib/tidy.tuesday/blob/main/employed_status.R

# https://twitter.com/gkckoc/status/1365797055428497408/photo/1
# https://github.com/gkckoc/TidyTuesday/blob/main/2021/week_9

# https://twitter.com/Andy_A_Baker/status/1366000328609562624/photo/1
# https://github.com/AndyABaker/TidyTuesday/blob/main/2021_week09_employmentandearnings.R

# https://twitter.com/sianbladon/status/1364994159002738691/photo/1
# https://github.com/sianbladon/Data-Viz/tree/master/Tidy-Tuesday-2021-Week-9

# Employment by industry and race
# https://twitter.com/francisc0o_o0/status/1364642626318688260/photo/1
# https://github.com/fgarza55/tidytuesday/blob/master/Code/2021_02_23_BLS.Rmd

# Change in employment by industry
# https://twitter.com/caren_koli/status/1364546463808823300/photo/1
# https://github.com/Caren-Koli/tidytuesday_challenge/tree/master/week9

# Number of employees by occupation and industry
# https://twitter.com/SeanPJackson/status/1365520208014708736





# what about 2016

summary_2016 <- employed %>%
  filter(year == 2016) %>%
  select(minor_occupation, race_gender, employ_n) %>%
  drop_na() %>%
  filter(race_gender %in% c("Black or African American", "TOTAL")) %>%
  group_by(race_gender, minor_occupation) %>%
  summarize_at(vars(employ_n), sum) %>%
  mutate(percent = employ_n/sum(employ_n)*100) %>%
  pivot_wider(names_from = race_gender, values_from = c(employ_n, percent))


summary_2016 %>%
  arrange(desc(`percent_Black or African American`)) %>%
  gt(groupname_col = "race_gender")


# let's look by gender

summary_gender <- employed %>%
  filter(year == 2020) %>%
  select(minor_occupation, race_gender, employ_n) %>%
  drop_na() %>%
  filter(race_gender %in% c("Men", "Women")) %>%
  group_by(race_gender, minor_occupation) %>%
  summarize_at(vars(employ_n), sum) %>%
  mutate(percent = employ_n/sum(employ_n)*100) %>%
  pivot_wider(names_from = race_gender, values_from = c(employ_n, percent))

summary_gender %>%
  arrange(desc(percent_Women)) %>%
  gt(groupname_col = "race_gender")
  
