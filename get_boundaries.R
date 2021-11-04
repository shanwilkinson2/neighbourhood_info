# get & manipulate boundaries - saved as static files

library(dplyr)
library(sf)

# # Bolton boundary - not actually using this
# # Bolton boundary clipped to 20m https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-bgc
# la_boundary <- st_read("https://opendata.arcgis.com/datasets/0e07a8196454415eab18c40a54dfbbef_0.geojson") %>%
#   filter(lad19nm == "Bolton") %>%
#   st_set_crs(4326) # lat long


# LSOA boundaries & turning them into neighbourhood boundaries
# LSOA boundaries 2011 (current)
# https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-generalised-clipped-boundaries-in-england-and-wales  
lsoas_2011 <- st_read("G:\\Mapping Data\\R\\map\\Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")

# add boroughs variable from LSOA name
lsoas_2011 <- lsoas_2011 %>%
  mutate(borough = stringr::str_sub(lsoa11nm, 1, nchar(as.character(lsoa11nm))-5)) %>%
  st_transform(crs = 4326) # transforms to lat/ long from OSGB36

# filter lsoas 2011 Bolton only
lsoas_bolton <- filter(lsoas_2011, borough %in% "Bolton")
# plot(st_geometry(lsoas_bolton)) # check areas look right  
rm(lsoas_2011) # remove whole country of lsoas

# get lsoa-neighbourhood link (off Boltonjsna.org.uk)
# includes value for Bolton to keep whole borough value
lsoa_neighbourhood <- readRDS("lsoa_neighbourhood.rds")

# add in neighbourhoood name to lsoa boundaries
lsoas_bolton2 <- left_join(lsoas_bolton %>%
                             select(lsoa11cd, lsoa11nm),
                           lsoa_neighbourhood %>%
                             select(neighbourhood_name, lsoa_name),
                           by = c("lsoa11cd" = "lsoa_name"))

# get just outer neighbourhood boundary
neighbourhood_boundaries <- lsoas_bolton2 %>%
  select(neighbourhood_name) %>%
  group_by(neighbourhood_name) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

# save as outer neighbourhood boundaries
saveRDS(neighbourhood_boundaries, "neighbourhood boundaries.RDS")


# MSOA boundaries
# https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-bgc
msoas_2011 <- st_read("G:\\Mapping Data\\R\\map\\MSOA/Middle_Layer_Super_Output_Areas_December_2011_Boundaries_BGC.shp")

# add borough variable from MSOA name
msoas_2011 <- msoas_2011 %>%
  mutate(borough = stringr::str_sub(msoa11nm, 1, nchar(as.character(msoa11nm))-4)) %>%
  st_transform(crs = 4326) # transforms to lat/ long from OSGB36

# filter msoas 2011 Bolton only
msoas_bolton <- filter(msoas_2011, borough %in% "Bolton")
# plot(st_geometry(msoas_bolton)) # check areas look right  
rm(msoas_2011) # remove whole country of lsoas

# join in neighbourhood name & all msoa names lookup 
msoa_neighbourhood <- readRDS("msoas_neighbourhood.rds")

msoas_bolton2 <- msoas_bolton %>%
  select(msoa11cd) %>%
  left_join(msoa_neighbourhood,
            by = c("msoa11cd" = "msoa_code"))

# save as neighbourhood boundaries with all msoa names & neighbourhood
saveRDS(msoas_bolton2, "msoa boundaries.RDS")