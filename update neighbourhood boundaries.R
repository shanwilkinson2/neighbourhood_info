# update neighbourhood boundaries

# update neighbourhoood boundary file
  # download off jsna website, save as RDS. 

  library(sf)
  
  neighbourhood_boundaries6 <- st_read("C:/temp/Neighbourhood_boundaries_2023_10.geojson")
  saveRDS(neighbourhood_boundaries6, "neighbourhood boundaries6.RDS")

# update lookups
  # lsoa - neighbourhood
  # download off jsna website, save as RDS. 
  
  nbh_lsoa_lookup <- read.csv("C:/Temp/Neighbourhoods_LSOA_lookup_2023_10.csv")
  saveRDS(nbh_lsoa_lookup, "neighbourhood_lsoa_lookup.RDS")

  # msoa_neighbourhood
  library(stringr)
  
  old_msoa_lookup <- read.csv("msoas_neighbourhood_multiple2.csv") %>%
    select(1:3) %>% #just keep msoa code, name, hoc_name
    unique()
  
  nbh_msoa_lookup <- nbh_lsoa_lookup %>%
    select(lsoa_name, neighbourhood_name = neighbourhood_analytical_name,
           neighbourhood_num = neighbourhood_analytical_num) %>%
    mutate(msoa_name = str_sub(lsoa_name, 1, -2)) %>%
    select(-lsoa_name) %>%
    unique() %>%
    left_join(old_msoa_lookup,
              by = "msoa_name")
  
  saveRDS(nbh_msoa_lookup, "msoas_neighbourhood_multiple3.RDS")
  

  

  