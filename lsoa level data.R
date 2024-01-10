# get lsoa data - minimum needed
 # imd, life expectancy, population

# creates age, deprivation, health_disab

library(dplyr)
library(openxlsx)
library(tidyr)

lsoa_neighbourhood <- readRDS("lsoa_neighbourhood_lookup.RDS") %>%
  select(-c(neighbourhood_operational_num: operational_arrangements_apply_lsoa)) %>%
  rename(neighbourhood_num = neighbourhood_analytical_num, 
         neighbourhood_name = neighbourhood_analytical_name)

#######################

# english indices of deprivation 2019
  # https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019

  # imd overall plus domains
  imd_overall2 <- read.xlsx("https://assets.publishing.service.gov.uk/media/5d8b3ade40f0b60999a23330/File_2_-_IoD2019_Domains_of_Deprivation.xlsx",
                           sheet = 2) %>%
    janitor::clean_names() %>%
    # rename(imd_rank = index_of_multiple_deprivation_imd_rank,
    #        imd_decile = index_of_multiple_deprivation_imd_decile) %>%
    pivot_longer(cols = -c(lsoa_code_2011:local_authority_district_name_2019),
                 values_to = "Value", names_to = "IndicatorName") %>%
    mutate(
      IndicatorName = case_when(
      IndicatorName == "index_of_multiple_deprivation_imd_rank_where_1_is_most_deprived" ~"* IMD (rank)",
      IndicatorName == "index_of_multiple_deprivation_imd_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~ "* IMD (decile)",
      IndicatorName == "income_rank_where_1_is_most_deprived" ~"Income (rank)",
      IndicatorName == "income_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Income (decile)",
      IndicatorName == "employment_rank_where_1_is_most_deprived" ~"Employment (rank)",
      IndicatorName == "employment_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Employment (decile)",
      IndicatorName == "education_skills_and_training_rank_where_1_is_most_deprived" ~"Education skills & training (rank)",
      IndicatorName == "education_skills_and_training_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Education skills & training (decile)",
      IndicatorName == "health_deprivation_and_disability_rank_where_1_is_most_deprived" ~"Health & disability (rank)",
      IndicatorName == "health_deprivation_and_disability_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Health & disability (rank)",
      IndicatorName == "crime_rank_where_1_is_most_deprived" ~"Crime (rank)",
      IndicatorName == "crime_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Crime (decile)",
      IndicatorName == "barriers_to_housing_and_services_rank_where_1_is_most_deprived" ~ "Barriers to housing & services (rank)",
      IndicatorName == "barriers_to_housing_and_services_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~ "Barriers to housing & services (decile)",
      IndicatorName == "living_environment_rank_where_1_is_most_deprived" ~"Living environment (rank)",
      IndicatorName == "living_environment_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Living environment (decile)",
      TRUE ~IndicatorName
      ),
      )

  # idaci & idopi
  imd_overall3 <- read.xlsx("https://assets.publishing.service.gov.uk/media/5d8b3b0340f0b609967c214b/File_3_-_IoD2019_Supplementary_Indices_-_IDACI_and_IDAOPI.xlsx",
                            sheet = 2) %>%
    janitor::clean_names() %>%
    pivot_longer(cols = -c(lsoa_code_2011:local_authority_district_name_2019),
                 values_to = "Value", names_to = "IndicatorName") %>%
    filter(!IndicatorName %in% c(
      "index_of_multiple_deprivation_imd_rank_where_1_is_most_deprived",                                       
      "index_of_multiple_deprivation_imd_decile_where_1_is_most_deprived_10_percent_of_lso_as"      
    )) %>%
    mutate(
      IndicatorName = case_when(
        IndicatorName == "income_deprivation_affecting_children_index_idaci_rank_where_1_is_most_deprived"  ~"Income deprivation affecting children (rank)",                     
        IndicatorName == "income_deprivation_affecting_children_index_idaci_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Income deprivation affecting children (decile)",
        IndicatorName == "income_deprivation_affecting_older_people_idaopi_rank_where_1_is_most_deprived" ~"Income deprivation affecting older people (rank)",                       
        IndicatorName == "income_deprivation_affecting_older_people_idaopi_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Income deprivation affecting older people (decile)",
        TRUE ~IndicatorName
      )
      )
  
  # just subdomains (some interesting stuff)
  imd_overall4 <- read.xlsx("https://assets.publishing.service.gov.uk/media/5d8b3b24ed915d0373d35410/File_4_-_IoD2019_Sub-domains_of_Deprivation.xlsx",
                           sheet = 2) %>%
    janitor::clean_names() %>%
    pivot_longer(cols = -c(lsoa_code_2011:local_authority_district_name_2019),
                 values_to = "Value", names_to = "IndicatorName") %>%
    mutate(
      IndicatorName = case_when(
        IndicatorName == "education_skills_and_training_rank_where_1_is_most_deprived" ~"Education skills & training (rank)",                             
        IndicatorName == "education_skills_and_training_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Education skills & training (decile)",      
        IndicatorName == "children_and_young_people_sub_domain_rank_where_1_is_most_deprived" ~"Education: children & young people (rank)",                      
        IndicatorName == "children_and_young_people_sub_domain_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Education: children & young people (decile)",  
        IndicatorName == "adult_skills_sub_domain_rank_where_1_is_most_deprived" ~"Education: adults (rank)",                                  
        IndicatorName == "adult_skills_sub_domain_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Education: adults (decile)",            
        IndicatorName == "barriers_to_housing_and_services_rank_where_1_is_most_deprived" ~ "Barriers to housing & services (rank)",                           
        IndicatorName == "barriers_to_housing_and_services_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~ "Barriers to housing & services (decile)",    
        IndicatorName == "geographical_barriers_sub_domain_rank_where_1_is_most_deprived" ~ "Barriers: geographical (rank)",                           
        IndicatorName == "geographical_barriers_sub_domain_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~ "Barriers: geographical (decile)",   
        IndicatorName == "wider_barriers_sub_domain_rank_where_1_is_most_deprived" ~ "Barriers: wider (rank)",                                   
        IndicatorName == "wider_barriers_sub_domain_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~ "Barriers: wider (decile)",          
        IndicatorName == "living_environment_rank_where_1_is_most_deprived" ~"Living environment (rank)",                                         
        IndicatorName == "living_environment_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Living environment (decile)",                   
        IndicatorName == "indoors_sub_domain_rank_where_1_is_most_deprived"  ~"Living environment: indoors (rank)",                                       
        IndicatorName == "indoors_sub_domain_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Living environment: indoors (decile)",                  
        IndicatorName == "outdoors_sub_domain_rank_where_1_is_most_deprived"  ~"Living environment: outdoors (rank)",                                       
        IndicatorName == "outdoors_sub_domain_decile_where_1_is_most_deprived_10_percent_of_lso_as" ~"Living environment: outdoors (decile)", 
        TRUE ~IndicatorName
      ),
    ) %>%
    # take out domains leave only subdomains
    filter(!IndicatorName %in% 
    c("Education skills & training (rank)", "Education skills & training (decile)",
      "Barriers to housing & services (rank)", "Barriers to housing & services (decile)",
      "Living environment (rank)", "Living environment (decile)"
      )
    )
  
  # bind together different IMDs
  imd_overall <- bind_rows(imd_overall2, imd_overall3) %>%
    bind_rows(imd_overall4)
  
  # calculate ENgland & bolton
  lsoa_standardised <- imd_overall %>%
    group_by(IndicatorName) %>%
      mutate(
        lsoa_z = (Value - mean(Value, na.rm = TRUE))/ sd(Value, na.rm = TRUE)
      ) %>%
        mutate(
          england_min = min(Value, na.rm = TRUE),
          england_max = max(Value, na.rm = TRUE),
          england_q1 = quantile(Value, 0.25, na.rm = TRUE),
          england_median = median(Value, na.rm = TRUE),
          england_q3 = quantile(Value, 0.75, na.rm = TRUE)) %>%
    # filter to just bolton
    filter(local_authority_district_name_2019 == "Bolton")  %>%
    ungroup()

  # add in neighbourhood (lsoa only in 1 neighbourhood)
  nbourhood_indicators <- lsoa_standardised %>%
    left_join(lsoa_neighbourhood,
              by = c("lsoa_code_2011" = "lsoa_code")) %>%
    rename(neighbourhood = neighbourhood_name) %>%
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
           bolton_q3 = quantile(Value, 0.75, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(DomainName = "Deprivation") %>%
  rename(geography = lsoa_name_2011,
         geography_code = lsoa_code_2011)
  
  deprivation <- nbourhood_indicators
  
  # tidyup
    rm(nbourhood_indicators)
    rm(imd_overall)
    rm(lsoa_standardised)
    rm(imd_overall2)
    rm(imd_overall3)
    rm(imd_overall4)

  #################################################################
  # census age info - 5 year bands only at lsoa
  
  age <- read.csv("census2021-ts007a-lsoa.csv") %>%
    janitor::clean_names() %>%
    pivot_longer(cols = -c(date:age_total),
                 values_to = "Value",
                 names_to = "IndicatorName") %>%
    rename(num = Value) %>%
    mutate(Value = num/age_total *100,
           IndicatorName = stringr::str_sub(IndicatorName, 6, -1),
           IndicatorName = paste0("A", IndicatorName),
           IndicatorName = stringr::str_replace_all(IndicatorName, "_", " "))
  
    # calculate England values
    lsoa_standardised <- age %>%
      group_by(IndicatorName) %>%
      mutate(
        lsoa_z = (Value - mean(Value, na.rm = TRUE))/ sd(Value, na.rm = TRUE)
      ) %>%
      mutate(
        england_min = min(Value, na.rm = TRUE),
        england_max = max(Value, na.rm = TRUE),
        england_q1 = quantile(Value, 0.25, na.rm = TRUE),
        england_median = median(Value, na.rm = TRUE),
        england_q3 = quantile(Value, 0.75, na.rm = TRUE)) %>%
      # filter to just bolton
      filter(stringr::str_detect(geography, "^Bolton"))  %>%
      ungroup()  


  # calculate neighbourhood values  

  nbourhood_indicators <- lsoa_standardised %>%
    left_join(lsoa_neighbourhood,
              by = c("geography_code" = "lsoa_code")) %>%
    rename(neighbourhood = neighbourhood_name) %>%
    group_by(neighbourhood) %>%
      mutate(nbourhood_denominator = sum(num)) %>%
    ungroup() %>%
    group_by(IndicatorName, neighbourhood) %>%
    mutate(nbourhood_count = sum(num), 
           nbourhood_pct = nbourhood_count/nbourhood_denominator*100,
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
    mutate(DomainName = "Census 2021 - Age")

  age <- nbourhood_indicators
  
  # tidyup
  rm(nbourhood_indicators)
  rm(neighbourhood_pop)
  rm(lsoa_standardised)
 
  ########################################
  
  ################# general health & disability unstandardised #############################
  # standardised not available at lsoa
  # census2021-ts037-lsoa.csv - gen health unstandardised
  # census2021-ts038-lsoa.csv - disabilty unstandardised
  
  
  gen_health <- read.csv("census2021-ts037-lsoa.csv") %>%
    janitor::clean_names() %>%
    pivot_longer(cols = -c(1:4),
                 values_to = "Value",
                 names_to = "IndicatorName") %>%
    rename(num = Value,
           area_total = 4) %>%
    mutate(Value = num/area_total *100,
           DomainName = "Census 2021 - general health",
           IndicatorName = case_when(
             IndicatorName == "general_health_very_good_health" ~ "Very good health",
             IndicatorName == "general_health_good_health" ~"Good health",
             IndicatorName == "general_health_fair_health" ~"Fair health",
             IndicatorName == "general_health_bad_health" ~"Bad health",
             IndicatorName == "general_health_very_bad_health"  ~"Very bad health",
            TRUE ~IndicatorName
           )
           )
  
  disab <- read.csv("census2021-ts038-lsoa.csv") %>%
    janitor::clean_names() %>%
    pivot_longer(cols = -c(1:4),
                 values_to = "Value",
                 names_to = "IndicatorName") %>%
    rename(num = Value,
           area_total = 4) %>%
    mutate(Value = num/area_total *100,
           DomainName = "Census 2021 - Disability",
           IndicatorName = case_when(
           IndicatorName == "disability_disabled_under_the_equality_act" ~"Disabled under the equality act", 
           IndicatorName == "disability_disabled_under_the_equality_act_day_to_day_activities_limited_a_lot" ~"Disabled - activities limited a lot",
            IndicatorName == "disability_disabled_under_the_equality_act_day_to_day_activities_limited_a_little"  ~"Disabled - activities limited a little",
            IndicatorName == "disability_not_disabled_under_the_equality_act" ~"Not disabled under the equality act",
            IndicatorName == "disability_not_disabled_under_the_equality_act_has_long_term_physical_or_mental_health_condition_but_day_to_day_activities_are_not_limited" ~"Not disabled - long term condition no limitation",
            IndicatorName == "disability_not_disabled_under_the_equality_act_no_long_term_physical_or_mental_health_conditions" ~"Not disabled - no long term condition",
           TRUE ~IndicatorName
           )
           )
  
  together <- bind_rows(gen_health, disab)
  
  # calculate england values
  lsoa_standardised <- together %>%
    group_by(IndicatorName) %>%
    mutate(
      lsoa_z = (Value - mean(Value, na.rm = TRUE))/ sd(Value, na.rm = TRUE),
        england_min = min(Value, na.rm = TRUE),
        england_max = max(Value, na.rm = TRUE),
        england_q1 = quantile(Value, 0.25, na.rm = TRUE),
        england_median = median(Value, na.rm = TRUE),
        england_q3 = quantile(Value, 0.75, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(stringr::str_detect(geography, "^Bolton"))  %>%
    left_join(lsoa_neighbourhood,
              by = c("geography_code" = "lsoa_code")) %>%
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
  
  health_disab <- nbourhood_indicators
  
  rm(disab)
  rm(gen_health)
  rm(lsoa_standardised)
  rm(nbourhood_indicators)  
  rm(neighbourhood_pop)
  rm(together)
  
  
  ##############################################################################
  ## join together & save ######################################################
  ##############################################################################
   
  lsoa_data <- bind_rows(age, deprivation) %>%
    bind_rows(health_disab) %>%
    select(-c("date", 
              "local_authority_district_code_2019", "local_authority_district_name_2019", "area_total")) %>%
    rename(neighbourhood_name = neighbourhood)
             
saveRDS(lsoa_data, "lsoa_data.RDS")
