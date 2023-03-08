# imd indicators

library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)

# [1] "msoa11cd"                            "IndicatorId"                         "IndicatorName"                      
# [4] "ParentCode"                          "ParentName"                          "AreaCode"                           
# [7] "AreaName"                            "AreaType"                            "Sex"                                
# [10] "Age"                                 "CategoryType"                        "Category"                           
# [13] "TimePeriod"                          "Value"                               "LowerCi95_0Limit"                   
# [16] "UpperCi95_0Limit"                    "LowerCi99_8Limit"                    "UpperCi99_8Limit"                   
# [19] "Count"                               "Denominator"                         "ValueNote"                          
# [22] "RecentTrend"                         "ComparedToEnglandValueOrPercentiles" "ComparedToPercentiles"              
# [25] "TimePeriodSortable"                  "NewData"                             "ComparedToGoal"                     
# [28] "TimePeriodRange"                     "ComparedToMsoaValueOrPercentiles"    "hoc_msoa_name"                      
# [31] "x6_areas_number"                     "neighbourhood"                       "num_lsoas"                          
# [34] "GroupId"                             "DomainName"                          "msoa_z"                             
# [37] "nbourhood_count"                     "nbourhood_denominator"               "nbourhood_pct"                      
# [40] "nbourhood_median"                    "nbourhood_max"                       "nbourhood_min"                      
# [43] "nbourhood_q1"                        "nbourhood_q3"                        "z_nbourhood_median"                 
# [46] "z_nbourhood_max"                     "z_nbourhood_min"                     "z_nbourhood_q1"                     
# [49] "z_nbourhood_q3"                      "z_nbourhoood_median_abs"             "z_nbourhood_iqr_abs"                
# [52] "z_nbourhood_range_abs"               "bolton_min"                          "bolton_max"                         
# [55] "bolton_q1"                           "bolton_median"                       "bolton_q3"                          
# [58] "bolton_value"                        "england_value"                       "england_min"                        
# [61] "england_max"                         "england_q1"                          "england_median"                     
# [64] "england_q3"                          "z_nbourhood_median_abs_direction"    "geometry"

lsoa_neighbourhood <- read.xlsx("6 neighbourhoods final option.xlsx") %>%
  janitor::clean_names()

# english indices of deprivation 2019
# https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019

  imd_pivot_clean <- function(x){
    pivot_longer(x, cols = -c(1:4), # area name info
                 values_to = "Value", names_to = "IndicatorName") %>%
      janitor::clean_names() %>%
    rename(Value = value, IndicatorName = indicator_name)
  }

  imd_overall <- read.xlsx("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx",
                           sheet = 2) %>%
    imd_pivot_clean()
    
  imd_domains <- read.xlsx("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833973/File_2_-_IoD2019_Domains_of_Deprivation.xlsx",
                           sheet = 2) %>%
    imd_pivot_clean()
  
  suppl_incices <- read.xlsx("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833974/File_3_-_IoD2019_Supplementary_Indices_-_IDACI_and_IDAOPI.xlsx",
                           sheet = 2) %>%
    imd_pivot_clean()
  
  sub_domains <- read.xlsx("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833976/File_4_-_IoD2019_Sub-domains_of_Deprivation.xlsx",
                             sheet = 2) %>%
    imd_pivot_clean()
  
  imd_all <- bind_rows(imd_overall, imd_domains, suppl_incices, sub_domains) %>%
    mutate(IndicatorName = str_replace_all(IndicatorName, "\\.", " "),
           IndicatorName = str_replace(IndicatorName, "\\s\\(where 1 is most deprived\\)", ""),
           IndicatorName = str_replace(IndicatorName, "\\s\\(where 1 is most deprived 10% of LSOAs\\)", "")
    )

# add in neighbourhood
lsoa_standardised <- imd_overall %>%
  full_join(lsoa_neighbourhood,
            by = c("lsoa_code_2011" = "lsoa_name")) %>%
  group_by(IndicatorName) %>%
  mutate(
    lsoa_z = (Value - mean(Value, na.rm = TRUE))/ sd(Value, na.rm = TRUE)
  ) %>%
  filter(local_authority_district_name_2019 == "Bolton")  %>%
  ungroup() %>%
  rename(neighbourhood = x6_areas_name)


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
         bolton_q3 = quantile(Value, 0.75, na.rm = TRUE))