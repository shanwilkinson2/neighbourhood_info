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