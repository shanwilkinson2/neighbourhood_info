# census data

library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)
library(sf)

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

census_files <- c("census2021-ts007a-lsoa.csv", "census2021-ts037-lsoa.csv",
                  "census2021-ts038-lsoa.csv", "census2021-ts039-lsoa.csv",
                  "census2021-ts040-lsoa.csv")

# census data
# https://www.nomisweb.co.uk/sources/census_2021_bulk

pivot_clean <- function(x){
  pivot_longer(x, cols = -c(1:4), # area name info
                values_to = "Num", names_to = "Name") %>%
    tidyr::separate(Name, c("DomainName", "IndicatorName"), ": ") %>%
    rename(Denominator = 4,
           lsoa_code = `geography code`) %>%
    mutate(DomainName = paste("Census 2021 -", DomainName),
           IndicatorName = paste(IndicatorName, "%")) %>%
  # keep England only (includes Wales)
  filter(str_detect(lsoa_code, "E")) 
  }

# read in & clean
  for(i in 1:length(census_files)){ 
    if(i == 1){ 
    census_all <- data.table::fread(census_files[i]) %>% 
      pivot_clean() 
    } else {
      census_all <- census_all %>%
        bind_rows(
          data.table::fread(census_files[i]) %>% 
            pivot_clean() 
        )
    }
    census_all 
  }

census_all2 <- census_all %>%
  mutate(
    Value = Num/Denominator*100,
    Sex = NA,
    Age = NA,
    lsoa_z = (Value - mean(Value, na.rm = TRUE))/ sd(Value, na.rm = TRUE)
  ) %>%
  group_by(IndicatorName) %>%
  mutate(IndicatorId = cur_group_id()+100) %>%
  ungroup()

# add in neighbourhood
nbourhood_indicators <- census_all2 %>%
  full_join(lsoa_neighbourhood,
            by = c("lsoa_code" = "lsoa_name")) %>%
  group_by(IndicatorName) %>%
  filter(str_detect(geography, "Bolton")) %>%
  ungroup() %>%
  rename(neighbourhood = x6_areas_name) %>%
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

# england LSOA max/min
england_min_max <- census_all2 %>%
  group_by(IndicatorName) %>%
  mutate(
    england_min = min(Value, na.rm = TRUE),
    england_max = max(Value, na.rm = TRUE),
    england_q1 = quantile(Value, 0.25, na.rm = TRUE),
    england_median = median(Value, na.rm = TRUE),
    england_q3 = quantile(Value, 0.75, na.rm = TRUE)) %>%
  slice(1) %>%
  select(IndicatorName, england_min:england_q3) 


# get lsoa boundaries
lsoa_boundaries <- readRDS("lsoa_boundaries.RDS")

# join in england values
nbourhood_indicators2 <- lsoa_boundaries %>%
  left_join( 
    nbourhood_indicators,
    by = c("LSOA21CD" = "lsoa_code")) %>%
  full_join(england_min_max, by = "IndicatorName") %>%
  relocate(z_nbourhood_median_abs_direction, .before = nbourhood_pct) %>%
  rename(AreaName = LSOA21NM, hoc_msoa_name = msoa_hoc_name)

# join to msoa data
msoa_data <- readRDS("./bolton_neighbourhoods/neighbourhood_indicators.RDS") %>%
  bind_rows(nbourhood_indicators2) %>%
  saveRDS("./bolton_neighbourhoods/neighbourhood_indicators.RDS")


###################

msoa_data <- msoa_data %>%
  filter(!str_detect(DomainName, "Census 2021 - ")) %>%
  saveRDS("./bolton_neighbourhoods/neighbourhood_indicators.RDS")
