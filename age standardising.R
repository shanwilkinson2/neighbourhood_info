# age standardising
 
library(data.table)
library(dplyr)


england_age <- fread("census2021-england age.csv") %>% 
  janitor::clean_names() %>%
  filter(countries == "England") %>%
  mutate(england_total = sum(observation),
    england_age_proportion = observation/england_total *100)

age_disab <- fread("census2021-lsoa age disability.csv") %>%
  janitor::clean_names()
  filter()
  
age_genhealth <- fread("census2021-lsoa age disability.csv") %>% 
  janitor::clean_names() %>%
  rename(lsoa_code = lower_layer_super_output_areas_code,
         lsoa_name = lower_layer_super_output_areas) 

age_genhealth2 <- age_genhealth %>%
  filter(stringr::str_detect(lsoa_name, "^Bolton.+$")) %>%
  group_by(lsoa_code) %>%
    mutate(area_total = sum(observation)) %>%
  ungroup() %>%
  group_by(lsoa_code, age_6_categories) %>%
    mutate(age_area_total = sum(observation)) %>%
  ungroup() %>%
    mutate(age_pct_of_area = age_area_total/area_total * 100) %>%
  # add in england age breakdown
  left_join(england_age %>%
              select(age_6_categories, england_age_proportion), 
            by = "age_6_categories") %>%
  # calculate adjusted area % if exactly as england age structure
  mutate(
    age_area_pct = observation/ area_total *100,
    #age_pct_of_area * england_age_proportion,
    pct_adjusted = observation/ age_area_total/ 
           age_pct_of_area * england_age_proportion *10000
    ) 
  group_by(lsoa_code, disability_3_categories) %>%
    mutate(area_cat_pct = sum(pct),
           area_cat_pct_adjusted = sum(pct_adjusted)
    )

