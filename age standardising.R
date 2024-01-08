# age standardising
 
library(data.table)
library(dplyr)
library(tidyr)

## read in age files ################################################

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

## add england age profile to lsoa ##############################

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

############################################


################# gen health & disability #################################

gen_health <- read.csv("census2021-ts037-lsoa.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(cols = -c(1:4),
               values_to = "Value",
               names_to = "IndicatorName") %>%
  rename(num = Value,
         area_total = 4) %>%
  mutate(
    Value = num/area_total *100,
    DomainName = "Census 2021 - general health (standardised)",
    IndicatorName = case_when(
      IndicatorName == "general_health_very_good_health" ~ "Very good health",
      IndicatorName == "general_health_good_health" ~"Good health",
      IndicatorName == "general_health_fair_health" ~"Fair health",
      IndicatorName == "general_health_bad_health" ~"Bad health",
      IndicatorName == "general_health_very_bad_health"  ~"Very bad health",
      TRUE ~IndicatorName
    )
  ) %>%
  relocate(DomainName, .after = IndicatorName)

disab <- read.csv("census2021-ts038-lsoa.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(cols = -c(1:4),
               values_to = "Value",
               names_to = "IndicatorName") %>%
  rename(num = Value,
         area_total = 4) %>%
  mutate(
    Value = num/area_total *100,
    DomainName = "Census 2021 - disability (standardised)",
    IndicatorName = case_when(
      IndicatorName == "disability_disabled_under_the_equality_act" ~"Disabled under the equality act", 
      IndicatorName == "disability_disabled_under_the_equality_act_day_to_day_activities_limited_a_lot" ~"Disabled - activities limited a lot",
      IndicatorName == "disability_disabled_under_the_equality_act_day_to_day_activities_limited_a_little"  ~"Disabled - activities limited a little",
      IndicatorName == "disability_not_disabled_under_the_equality_act" ~"Not disabled under the equality act",
      IndicatorName == "disability_not_disabled_under_the_equality_act_has_long_term_physical_or_mental_health_condition_but_day_to_day_activities_are_not_limited" ~"Not disabled - long term condition no limitation",
      IndicatorName == "disability_not_disabled_under_the_equality_act_no_long_term_physical_or_mental_health_conditions" ~"Not disabled - no long term condition",
      TRUE ~IndicatorName
    )
  ) %>%
  relocate(DomainName, .after = IndicatorName)

together <- bind_rows(gen_health, disab)

# add in neighbourhood
  
  lsoa_neighbourhood <- readRDS("lsoa_neighbourhood_lookup.RDS") %>%
    rename(neighbourhood_name = neighbourhood_analytical_name,
           neighbourhood_num = neighbourhood_analytical_num) %>%
    select(-c(neighbourhood_operational_num, neighbourhood_operational_name, 
              operational_arrangements_apply_lsoa))
  
  lsoa_standardised <- together %>%
    full_join(lsoa_neighbourhood,
              by = c("geography_code" = "lsoa_name")) %>%
    group_by(IndicatorName) %>%
    mutate(
      lsoa_z = (Value - mean(Value, na.rm = TRUE))/ sd(Value, na.rm = TRUE)
    ) %>%
    filter(stringr::str_detect(geography, "^Bolton"))  %>%
    ungroup() %>%
    rename(neighbourhood = neighbourhood_name)


neighbourhood_pop <- lsoa_standardised %>%
  select(geography, neighbourhood, area_total) %>%
  group_by(geography, neighbourhood) %>%
  # get only 1 age total row per lsoa
  slice(1) %>%
  ungroup() %>%
  group_by(neighbourhood) %>%
  summarise(nbourhood_denominator = sum(area_total))

nbourhood_indicators <- lsoa_standardised %>%
  group_by(IndicatorName, neighbourhood) %>%
  mutate(nbourhood_count = NA, 
         nbourhood_denominator = NA,
         nbourhood_pct = NA,
         nbourhood_median = median(Value, na.rm = TRUE),
         nbourhood_max = max(Value, na.rm = TRUE),
         nbourhood_min = min(Value, na.rm = TRUE),
         nbourhood_q1 = quantile(Value, 0.25, na.rm = TRUE),
         nbourhood_q3 = quantile(Value, 0.75, na.rm = TRUE),
         z_nbourhood_median = median(lsoa_z, na.rm = TRUE),
         z_nbourhood_max = max(lsoa_z, na.rm = TRUE),
         z_nbourhood_min = min(lsoa_z, na.rm = TRUE),
         z_nbourhood_q1 = quantile(lsoa_z, 0.25, na.rm = TRUE),
         z_nbourhood_q3 = quantile(lsoa_z, 0.75, na.rm = TRUE),
         z_nbourhoood_median_abs = abs(z_nbourhood_median),
         z_nbourhood_iqr_abs = abs(z_nbourhood_q3 - z_nbourhood_q1),
         z_nbourhood_range_abs = abs(z_nbourhood_max - z_nbourhood_min)
  ) %>%
  ungroup() %>%
  # get direction of absolute values
  mutate(
    z_nbourhood_median_abs_direction = case_when(
      z_nbourhood_median >1.96 ~ "much higher", # 95% of a normal distribution lie between +1.96 & -1.96
      z_nbourhood_median >0 ~ "higher",
      z_nbourhood_median <0 ~ "lower",
      z_nbourhood_median <1.96 ~ "much lower",
      z_nbourhood_median ==0 ~ "average")
  ) %>%
  # bolton min & max
  group_by(IndicatorName) %>%
  mutate(bolton_min = min(Value, na.rm = TRUE),
         bolton_max = max(Value, na.rm = TRUE),
         bolton_q1 = quantile(Value, 0.25, na.rm = TRUE),
         bolton_median = median(Value, na.rm = TRUE),
         bolton_q3 = quantile(Value, 0.75, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(IndicatorName, neighbourhood) %>%
  select(-nbourhood_denominator) %>%
  left_join(neighbourhood_pop, by = ("neighbourhood")) %>%
  relocate(nbourhood_denominator, .after = nbourhood_count) %>%
  mutate(nbourhood_count = sum(num),
         nbourhood_pct = nbourhood_count/nbourhood_denominator*100
  )  %>%
  ungroup() 

health_disab <- nbourhood_indicators %>%
  select(-c(date, area_total, lsoa_code))
  
saveRDS(health_disab, "lsoa_health_disab_standardised.RDS")

rm(disab)
rm(gen_health)
rm(lsoa_standardised)
rm(nbourhood_indicators)  
rm(neighbourhood_pop)
rm(together)
