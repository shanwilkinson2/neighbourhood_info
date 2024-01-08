# final dataset uses:
# IndicatorId, Sex, Age, neighbourhood, IndicatorName, DomainName, AreaName
# z_nbourhood_bedian_abs, z_nbourhood_median_abs_direction, nbourhood_median, england_median
# z_nbourhood_iqr_abs, z_nbourhood_range_abs
# nbourhood_min, bolton_min, england_min, 
# nbourhood_q1, bolton_q1, england_q1, 
# nbourhood_median, bolton_median, england_median
# nbourhood_pct, bolton_value, england_value
# nbourhood_q3, bolton_q3, england_q3
# nbourhood_max, bolton_max, england_max
# hoc_msoa_name, Value, 
# geometry

library(dplyr)
library(sf)

# read in data created in other scripts

# # ensure working directory is the upper folder neighbourhood_info file for testting the app
#   setwd("./bolton_neighbourhoods")
#   setwd("..") # up one level

# boundaries
  msoa_boundaries <- readRDS("msoa boundaries.RDS")
  lsoa_boundaries <- readRDS("lsoa_boundaries.RDS")
  neighbourhood_boundaries <- readRDS("neighbourhood boundaries6.RDS") 

  # # add hoc msoa code to lsoa
  # lsoa_boundaries2 <- lsoa_boundaries %>%
  #   mutate(msoa_name = stringr::str_sub(LSOA21NM, 1, -2)) %>%
  #   left_join(msoa_boundaries %>% 
  #               st_drop_geometry() %>%
  #               select(msoa_name, hoc_msoa_name),
  #             by = c("msoa_name")) %>%
  #   mutate(hoc_msoa_name = paste("Part of", hoc_msoa_name)) %>%
  #   select(-msoa_name)
  # saveRDS(lsoa_boundaries2, "lsoa_boundaries.RDS")
    
  
# lookups
  lsoa_neighbourhood <- readRDS("lsoa_neighbourhood_lookup.RDS")
  msoa_neighbourhood_multiple <- readRDS("msoas_neighbourhood_multiple3.RDS")  

# # save lookups to the app folder
#   saveRDS(neighbourhood_boundaries, "./bolton_neighbourhoods/neighbourhood_boundaries.RDS")
#   saveRDS(msoa_neighbourhood_multiple, "./bolton_neighbourhoods/msoa_neighbourhood_multiple.RDS")

# data
  
  # local health
  local_health_processed <- readRDS("local_health_processed.RDS")
  # add msoa boundary
  local_health_plus_boundaries <- right_join(msoa_boundaries %>%
                                       select(msoa11cd), # only want the join field & geometry whcih sticks anyway
                                       # # right join to keep geometry, 
                                       local_health_processed,
                                      by = c("msoa11cd" = "AreaCode")
                                      ) %>%
                                        rename(AreaCode = msoa11cd)
  
  
  # lsoa data
  lsoa_data <- readRDS("lsoa_data.RDS")
  lsoa_standardised_data <- readRDS("lsoa_health_disab_standardised.RDS")
  
  lsoa_data2 <- bind_rows(lsoa_data, lsoa_standardised_data) %>%
    rename(AreaName = geography, AreaCode = geography_code)
  
  lsoa_data_plus_boundaries <- right_join(lsoa_boundaries, 
                                          lsoa_data2,
                                           by = c("LSOA21CD" = "AreaCode")) %>%
    rename(AreaCode = LSOA21CD)
  
# single dataset for the app
  data_for_app <- bind_rows(local_health_plus_boundaries, lsoa_data_plus_boundaries) 
  
  saveRDS(data_for_app, "./bolton_neighbourhoods/neighbourhood_indicators.RDS")
  
  # neighbourhood_indicators <- readRDS("./bolton_neighbourhoods_app/neighbourhood_indicators.RDS") # in the app folder

  


# saveRDS(msoa_neighbourhood_multiple, "./bolton_neighbourhoods/msoa_neighbourhood_multiple.RDS") # in the app folder
