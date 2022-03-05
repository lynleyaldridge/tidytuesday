library(tidyverse)
library(here)

pisa <- readxl::read_xlsx(here("projects", "plotting", "data", "EDU-2020-485-EN-T006.XLSX"),
                              sheet = "Table V.B1.4.2",
                              skip = 11) %>%
  select(1, 14, 15, 17, 18) %>%
  filter(!row_number() %in% c(1:3, 42:89)) %>%
  setNames(c("country", "lackstaff_percent", "lackstaff_se", 
              "unqualstaff_percent", "unqualstaff_se")) %>%
  pivot_longer(!country, 
               names_to = c("variable", ".value"),
               names_sep = "_")

write_csv(pisa, here("projects", "plotting", "data", "pisa.csv"))