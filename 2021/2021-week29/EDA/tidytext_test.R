# install.packages("Rcpp")

# https://thomasadventure.blog/posts/enhance-ggplot2-with-ggtext/

library(tidyverse)
library(ggtext)
library(Rcpp)


tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
scoobydoo <- tuesdata$scoobydoo

tidyscooby <- scoobydoo %>%
  mutate(across(where(is.character), factor)) %>%
  separate_rows(monster_type, sep = ",", convert = TRUE) %>%
  mutate(monster_type = str_trim(monster_type),
         monster_type = recode(monster_type,
                               Disguise = "Disguised",
                               Disugised = "Disguised",
                               Insect = "Animal",
                               Skeleton = "Undead",
                               "Possessed Object" = "Possessed")) %>%
  filter(monster_type != "") %>%
  filter(monster_type != "NULL") %>%
  filter(motive != "NULL")

tidyscooby %>%
  filter(monster_type %in% c("Animal")) %>%
  ggplot(aes(motive, fill = monster_type)) +
  geom_bar() +
  coord_flip() +
  ggtitle(
    paste0(
      "<span style = 'color:#93C1DE'>**Roche**</span>",
      " *overtook* <span style = 'color:darkorange'>**Novartis**</span>",
      " in 2016"
    )
  ) +
  scale_color_manual(
    values = c("Animal" = "#93C1DE"),
    guide = "none"
  ) +
  theme(plot.title = element_markdown())


