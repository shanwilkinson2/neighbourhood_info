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



# read in data created in other scripts

# ensure working directory is the upper folder neighbourhood_info

# boundaries
  msoa_boundaries <- readRDS("msoa boundaries.RDS")
  lsoa_boundaries <- readRDS("lsoa_boundaries.RDS")
  neighbourhood_boundaries <- readRDS("neighbourhood boundaries6.RDS") 

# lookups
  lsoa_neighbourhood <- readRDS("lsoa_neighbourhood.RDS")
  msoa_neighbourhood_multiple <- ("msoas_neighbourhood_multiple3.RDS")  

# save lookups to the app folder
  saveRDS(neighbourhood_boundaries, "./bolton_neighbourhoods/neighbourhood_boundaries.RDS")
  saveRDS(msoa_neighbourhood_multiple, "./bolton_neighbourhoods/msoa_neighbourhood_multiple.RDS")

# data
  
  # local health
  local_health_processed <- readRDS("local_health_processed.RDS")
  # add msoa boundary
  local_health_plus_boundaries <- right_join(msoa_boundaries %>%
                                        select(msoa11cd), # only want the join field & geometry whcih sticks anyway
                                      local_health_processed, # right join to keep geometry
                                      by = c("msoa11cd" = "msoa_code")
  )
  
  # lsoa data
  lsoa_data <- readRDS("lsoa_data.RDS")
  
# single dataset for the app
  neighbourhood_indicators <- readRDS("./bolton_neighbourhoods_app/neighbourhood_indicators.RDS") # in the app folder



# saveRDS(msoa_neighbourhood_multiple, "./bolton_neighbourhoods/msoa_neighbourhood_multiple.RDS") # in the app folder
