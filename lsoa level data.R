# get lsoa data - minimum needed
 # imd, life expectancy, population

library(dplyr)
library(openxlsx)
library(tidyr)

lsoa_neighbourhood <- read.xlsx("6 neighbourhoods final option.xlsx") %>%
  janitor::clean_names()

# english indices of deprivation 2019
  # https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019

  imd_overall <- read.xlsx("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx",
                           sheet = 2) %>%
    janitor::clean_names() %>%
  rename(imd_rank = index_of_multiple_deprivation_imd_rank, 
           imd_decile = index_of_multiple_deprivation_imd_decile) %>%
    pivot_longer(cols = -c(lsoa_code_2011:local_authority_district_name_2019),
                 values_to = "Value", names_to = "indicator")
  
  # add in neighbourhood
  lsoa_standardised <- imd_overall %>%
    full_join(lsoa_neighbourhood,
                                 by = c("lsoa_code_2011" = "lsoa_name")) %>%
    group_by(indicator) %>%
    mutate(
      lsoa_z = (Value - mean(Value, na.rm = TRUE))/ sd(Value, na.rm = TRUE)
    ) %>%
    filter(local_authority_district_name_2019 == "Bolton")  %>%
    ungroup() 
  
  ##### REACHED HERE #################
  
  nbourhood_lsoa <- lsoa_standardised %>%
    left_join(imd_overall %>%
                filter(local_authority_district_name_2019 == "Bolton") %>%
                select(lsoa_code_2011, indicator, Value) , 
              by = c("indicator", "lsoa_code_2011")
    ) %>%
    

    
    group_by(indicator, neighbourhood) %>%
    mutate(nbourhood_count = NA, 
           nbourhood_denominator = NA,
           nbourhood_pct = NA,
           nbourhood_median = median(Value, na.rm = TRUE),
           nbourhood_max = max(Value, na.rm = TRUE),
           nbourhood_min = min(Value, na.rm = TRUE),
           nbourhood_q1 = quantile(Value, 0.25, na.rm = TRUE),
           nbourhood_q3 = quantile(Value, 0.75, na.rm = TRUE),
           z_nbourhood_median = median(msoa_z, na.rm = TRUE),
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
    group_by(IndicatorId, Sex, Age, TimePeriodSortable) %>%
    mutate(bolton_min = min(Value, na.rm = TRUE),
           bolton_max = max(Value, na.rm = TRUE),
           bolton_q1 = quantile(Value, 0.25, na.rm = TRUE),
           bolton_median = median(Value, na.rm = TRUE),
           bolton_q3 = quantile(Value, 0.75, na.rm = TRUE))
  
  
  # pivot to get bolton value in a different column
  nbourhood_indicators2 <- left_join(
    nbourhood_indicators %>%
      ungroup() %>%
      filter(neighbourhood != "Bolton"),
    nbourhood_indicators %>%
      ungroup() %>%
      filter(neighbourhood == "Bolton") %>%
      select(IndicatorId, Sex, Age, TimePeriodSortable, bolton_value = nbourhood_median), # median will be the value as all bolton
    by = c("IndicatorId", "Sex", "Age", "TimePeriodSortable"),
    suffix = c("_neighbourhood", "_bolton")
  )
  
  # get England values 
  
  # actual England figure
  england_indicators <- local_health_all_msoa %>%
    filter(AreaType == "England") %>%
    # keep latest value only - only seems to include latest anyway
    group_by(IndicatorId, Sex, Age) %>%
    filter(TimePeriodSortable == max(TimePeriodSortable)) %>%
    ungroup()
  
  # england MSOA max/min
  england_min_max <- local_health_all_msoa %>%
    filter(AreaType == "MSOA") %>%
    # keep latest value only - only seems to include latest anyway
    group_by(IndicatorId, Sex, Age) %>%
    filter(TimePeriodSortable == max(TimePeriodSortable)) %>%
    mutate(
      england_min = min(Value, na.rm = TRUE),
      england_max = max(Value, na.rm = TRUE),
      england_q1 = quantile(Value, 0.25, na.rm = TRUE),
      england_median = median(Value, na.rm = TRUE),
      england_q3 = quantile(Value, 0.75, na.rm = TRUE)) %>%
    slice(1)
  
  # combined for joining
  england_values <- full_join(
    england_indicators %>%
      select(IndicatorId, Sex, Age, TimePeriodSortable, Value)
    ,
    england_min_max %>%
      select(IndicatorId, Sex, Age, TimePeriodSortable, england_min: england_q3)
    ,
    by = c("IndicatorId", "Sex", "Age", "TimePeriodSortable")
  )
  
  # join in england
  nbourhood_indicators2b <- left_join(nbourhood_indicators2, 
                                      england_values %>%
                                        rename("england_value"= "Value"),
                                      by = c("IndicatorId", "Sex", "Age", "TimePeriodSortable"), 
                                      suffix = c("", "_england")) %>%
    # move absolute median z direction as it's not numeric so mutate across to 1 decimal place still works
    relocate(z_nbourhood_median_abs_direction, .after = england_q3) %>%
    # give new indicator name for sex disaggregated indicators
    mutate(IndicatorName = ifelse(!Sex %in% c("Persons", "Not applicable"), 
                                  paste(IndicatorName, Sex, sep = " - "),
                                  IndicatorName)
    )
  