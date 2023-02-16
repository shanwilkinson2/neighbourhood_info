library(dplyr)
library(readxl)
library(data.table)

new_lookup <- read_xlsx("6 neighbourhoods final option.xlsx") %>%
  janitor::clean_names()

msoa_areas <- new_lookup %>%
  group_by(msoa_name, msoa_code, msoa_hoc_name, x6_areas_number, x6_areas_name) %>%
  summarise(num_lsoas = n()) %>%
  rename(hoc_msoa_name = msoa_hoc_name, 
         neighbourhood = x6_areas_name)

fwrite(msoa_areas, "msoas_neighbourhood_multiple2.csv")
