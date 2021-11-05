# read in data created in other scripts

# ensure working directory is the upper folder neighbourhood_info

# boundaries
  msoa_boundaries <- readRDS("msoa boundaries.RDS")
  neighbourhood_boundaries <- readRDS("./bolton_neighbourhoods/neighbourhood boundaries.RDS") # in the app folder

# single dataset for the app
  neighbourhood_indicators <- readRDS("./bolton_neighbourhoods/neighbourhood_indicators.RDS") # in the app folder

# lookups
  lsoa_neighbourhood <- readRDS("lsoa_neighbourhood.rds")
  msoa_neighbourhood_multiple <- fread("msoas_neighbourhood_multiple.csv")

# saveRDS(msoa_neighbourhood_multiple, "./bolton_neighbourhoods/msoa_neighbourhood_multiple.RDS") # in the app folder