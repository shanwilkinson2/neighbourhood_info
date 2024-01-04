# age standardising
 
library(data.table)
library(dplyr)


england_age <- fread("census2021-england age.csv") %>% 
  janitor::clean_names() %>%
  filter(countries == "England") %>%
  mutate(england_total = sum(observation),
    england_age_proportion = observation/england_total *100)

age_disab <- fread("census2021-lsoa age disability.csv") %>%
  janitor::clean_names() %>%
  select(-c(disability_3_categories_code)) %>%
  rename(lsoa_code = lower_layer_super_output_areas_code,
         lsoa_name = lower_layer_super_output_areas,
          IndicatorName = disability_3_categories) %>%
  mutate(DomainName = "Disability")

age_genhealth <- fread("census2021-lsoa age gen health.csv") %>% 
  janitor::clean_names() %>%
  select(-c(general_health_6_categories_code)) %>%
  rename(lsoa_code = lower_layer_super_output_areas_code,
         lsoa_name = lower_layer_super_output_areas,
         IndicatorName = general_health_6_categories) %>%
  mutate(DomainName = "General health") 

age_genhealth_disab <- bind_rows(age_disab, age_genhealth) %>% 
  filter(IndicatorName != "Does not apply") %>% 
  # filter(stringr::str_detect(lsoa_name, "^Bolton.+$")) %>%
  group_by(lsoa_code) %>%
    mutate(area_total = sum(observation)) %>%
  ungroup() %>%
  group_by(lsoa_code, IndicatorName) %>%
    mutate(age_area_total = sum(observation)) %>%
  ungroup() %>%
    mutate(lsoa_age_proportion = age_area_total/area_total * 100) %>%
  # add in england age breakdown
  left_join(england_age %>%
              select(age_6_categories, england_age_proportion), 
            by = "age_6_categories") %>%
  # calculate adjusted area % if exactly as england age structure
  mutate(
    area_pct = observation/ area_total *100,
    area_pct_adj = area_pct/lsoa_age_proportion * england_age_proportion
    ) %>%
  group_by(lsoa_code, IndicatorName) %>%
    mutate(area_cat_pct = sum(area_pct),
           area_cat_pct_adj = sum(area_pct_adj)
    ) %>%
  ungroup()

# get just one row per area, as don't need age groups any more
age_genhealth_disab2 <- age_genhealth_disab %>%
    select(-c(age_6_categories_code, age_6_categories,
              observation:england_age_proportion, area_pct, area_pct_adj)) %>%
  group_by(lsoa_code, IndicatorName) %>%
    slice(1) %>%
  ungroup()
