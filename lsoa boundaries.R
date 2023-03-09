# lsoa boundaries

library(dplyr)
library(sf)

# LSOA
# LSOA boundaries 2021 (current)
# https://geoportal.statistics.gov.uk/
  lsoas_2021 <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_Dec_2021_Boundaries_Generalised_Clipped_EW_BGC_V2/FeatureServer/0/query?where=1%3D1&outFields=LSOA21CD,LSOA21NM&outSR=4326&f=json")

# filter to bolton only, transform to lat long for online mapping  
  lsoas_2021_bolton <- lsoas_2021 %>%
    st_transform(crs = 4326) %>% # transforms to lat/ long from OSGB36 
    filter(stringr::str_detect(LSOA21NM, "^Bolton "))

  plot(st_geometry(lsoas_2021_bolton)) # check areas look right  
  saveRDS(lsoas_2021_bolton, "lsoa_boundaries.RDS") # save boundaries
  
  rm(lsoas_2021) # remove whole country of lsoas
