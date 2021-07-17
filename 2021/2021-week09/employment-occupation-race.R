# Cleveland box chart, code for which draws heavily on the following sources:
# https://uc-r.github.io/cleveland-dot-plots
# https://github.com/djpib/tidy.tuesday/blob/main/employed_status.R


# load libraries --------------

library(tidyverse)
library(ggtext)    # for formatting text in graph titles as markdown, use of element_textbox_simple

# read in data ----------------
employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

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
        text = element_text(family = "mono"),
        plot.title = element_markdown(size = 15),
        plot.subtitle = element_textbox_simple(size = 12),
        plot.title.position = "plot")+
  labs(y = "",
       x = "Share of workers (%)",
       title = "Employment by occupational group: Annual average (2020)",
       subtitle = "<b style = 'color:#F8766D'>Black or African American workers</b> are underrepresented in professional and \n management occupations, 
       and overrepresented in service and transportation, relative to <b style = 'color:#00BFC4'>all workers</b>",
       caption = "Data: U.S Bureau of Labor Statistics, Current Population Survey | Tidy Tuesday 2021, Week 9")

# save a copy of plot (remove commenting to use this part of code)

# library(here)
# ggsave(here::here("2021", "2021-week9", "plots", "employment-occupation-race.png"))
