# fingertipsR is no longer on CRAN
install.packages("fingertipsR", repos = "https://dev.ropensci.org")
# also need "shinycssloaders" 
install.packages("shinycssloaders")
install.packages("miniUI")

#################### load packages ################################

library(dplyr)
library(shinycssloaders)
library(fingertipsR)
library(data.table)
library(nomisr)
library(readxl)
library(sf)
library(leaflet)
library(leaflet.extras)

#################### read in datasets created below ###############

# ensure working directory is the upper folder neighbourhood_info

# boundaries
  msoa_boundaries <- readRDS("msoa boundaries.RDS")
  neighbourhood_boundaries <- readRDS("./bolton_neighbourhoods/neighbourhood boundaries.RDS") # in the app folder

# data
  neighbourhood_data <- readRDS("./bolton_neighbourhoods/neighbourhood_indicators.RDS")
  local_health_data_msoa <- readRDS("./bolton_neighbourhoods/local health data with boundaries.RDS") # in the app folder
  neighbourhood_indicators <- readRDS("./bolton_neighbourhoods/dashboard_indicators.RDS")
  
  # hopefully single dataset to replace the 2 above
  neighbourhood_indicators <- readRDS("./bolton_neighbourhoods/neighbourhood_indicators.RDS") # in the app folder

  # lookups
    lsoa_neighbourhood <- readRDS("lsoa_neighbourhood.rds")
    msoa_neighbourhood <- readRDS("msoas_neighbourhood.rds")

###################### boundaries ################################

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

################# get data from phe fingertips local health #########################  
  
# local health 
# https://fingertips.phe.org.uk/profile/local-health/data#page/0/gid/1938133180/ati/3/iid/93744/age/28/sex/4/cid/4/tbm/1

# available indicators in local health profile
  local_health_indicators <- indicators(ProfileID = 143)

# get Bolton msoa data for all local health indicators 
  # msoa codes
    bolton_area_codes <- c( #"E08000001", # bolton itself
      "E02000984", "E02000985", "E02000986", "E02000987",
      "E02000988", "E02000989", "E02000990", "E02000991", "E02000992",
      "E02000993", "E02000994", "E02000995", "E02000996", "E02000997",
      "E02000998", "E02000999", "E02001000", "E02001001", "E02001002",
      "E02001003", "E02001004", "E02001005", "E02001006", "E02001007",
      "E02001008", "E02001009", "E02001010", "E02001011", "E02001012",
      "E02001013", "E02001014", "E02001015", "E02001016", "E02001017",
      "E02001018")

  # get msoa local health data
    local_health_msoa <- fingertips_data(IndicatorID = local_health_indicators$IndicatorID,
                                        ProfileID = 143,
                                        AreaTypeID = 3, # 3 = MSOA
                                        AreaCode = bolton_area_codes
                                        ) 
  # get borough local health data
    local_health_borough <- fingertips_data(IndicatorID = local_health_indicators$IndicatorID,
                                       ProfileID = 143,
                                       AreaTypeID = 402, # 402 = UTLA with boundary changes post Apr 2021
                                       AreaCode = "E08000001" # bolton
                                        ) 

  # join msoa & borough data
    local_health <- bind_rows(local_health_msoa, local_health_borough)

  # get rid of seperate files
    rm(local_health_msoa)
    rm(local_health_borough)

# MSOA best fit (local health doesn't go down to lsoa)
  msoa_neighbourhood <- readRDS("msoas_neighbourhood.RDS")
# includes value for Bolton to keep whole borough value

  # add in neighbourhood
    bolton_local_health2 <- left_join(local_health, msoa_neighbourhood, 
                                      by = c("AreaName"= "msoa_name")) %>%
      # keep latest value only - only seems to include latest anyway
      group_by(IndicatorID, Sex, Age, AreaName) %>%
      filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
      ungroup() %>%
      # add in domain ie part of the profile 
      left_join(local_health_indicators %>% 
                  select(IndicatorID, DomainID, DomainName, ProfileID, ProfileName),
                by = "IndicatorID") %>%
      filter(ProfileID == 143) %>% # local health profile = 143, some indicators are in multiple
      arrange(ProfileID, DomainID) %>%
      group_by(IndicatorID, Sex, Age, neighbourhood)

        
####### transform to neighbourhood level ##############################################################

# combine by neighbourhood for those indicators with a count & denominator     
  count_denom_indicators <- bolton_local_health2 %>%
    group_by(DomainID, DomainName, IndicatorID, IndicatorName, Sex, Age, Timeperiod, TimeperiodSortable, neighbourhood) %>%
    summarise(Count = sum(Count), 
              Denominator = sum(Denominator)) %>%
    filter(!is.na(Count)) %>%
    mutate(pct_value = Count/Denominator*100) 

  count_denom_indicators2 <- left_join(
    count_denom_indicators %>%
      ungroup() %>%
      filter(neighbourhood != "Bolton"),
    count_denom_indicators %>%
      ungroup() %>%
      filter(neighbourhood == "Bolton") %>%
      select(IndicatorID, Sex, Age, TimeperiodSortable, pct_value),
    by = c("IndicatorID", "Sex", "Age", "TimeperiodSortable"),
    suffix = c("_neighbourhood", "_bolton")
  )

# save for dashboard
  saveRDS(count_denom_indicators2 %>% ungroup(), "G:/Mapping Data/R/neighbourhood profiles/bolton_neighbourhoods/dashboard_indicators.RDS")

# View indicators

  bolton_local_health %>%
    select(IndicatorID, IndicatorName) %>%
    left_join(local_health_indicators %>% 
                select(IndicatorID, DomainID, DomainName, ProfileID, ProfileName),
              by = "IndicatorID") %>%
    filter(ProfileID == 143) %>% # local health profile = 143, some indicators are in multiple
    unique() %>%
    arrange(ProfileID, DomainID) %>%
    View()
  # fwrite("C:/Temp/temp.csv")

# MSOA level for map

  bolton_local_health_msoa_boundaries <-  right_join(msoa_boundaries, # right join to keep geometry
                                                     bolton_local_health2,
                                                     by = c("msoa11cd" = "AreaCode")
                                                    )
  
  saveRDS(bolton_local_health_msoa_boundaries, "local health data with boundaries.RDS")

  
# combine indicators but keep msoa level so can have 1 dataset
  
  nbourhood_indicators <- bolton_local_health2 %>%
    group_by(IndicatorID, Sex, Age, TimeperiodSortable, neighbourhood) %>%
    mutate(nbourhood_count = sum(Count), 
           nbourhood_denominator = sum(Denominator),
           nbourhood_pct = nbourhood_count/ nbourhood_denominator*100,
           nbourhood_median = median(Value),
           nbourhood_max = max(Value),
           nbourhood_min = min(Value)
           ) %>%
    ungroup() %>%
    # bolton min & max
    group_by(IndicatorID, Sex, Age, TimeperiodSortable) %>%
    mutate(bolton_min = min(Value),
           bolton_max = max(Value))
  
  # pivot to get bolton value in a different column
  nbourhood_indicators2 <- left_join(
    nbourhood_indicators %>%
      ungroup() %>%
      filter(neighbourhood != "Bolton"),
    nbourhood_indicators %>%
      ungroup() %>%
      filter(neighbourhood == "Bolton") %>%
      select(IndicatorID, Sex, Age, TimeperiodSortable, bolton_value = nbourhood_median),
    by = c("IndicatorID", "Sex", "Age", "TimeperiodSortable"),
    suffix = c("_neighbourhood", "_bolton")
  )
  
  msoa_boundaries <- readRDS("msoa boundaries.RDS")
  
    # add msoa boundary
  nbourhood_indicators3 <- right_join(msoa_boundaries %>%
                                      select(msoa11cd), # only want the join field & geometry whcih sticks anyway
                                      nbourhood_indicators2, # right join to keep geometry
               by = c("msoa11cd" = "msoa_code")
  ) %>%
    right_join(# right join to keep geometry
               bolton_local_health2,
               by = c("msoa11cd" = "AreaCode")
    )
  


##################### map ###############################

mytitle <- glue::glue("<b>Title</b><br>
                      Subtitle | Turn layers on & off to explore")

lsoa_labels <- (glue::glue("<b>LSOA</b><br>
                    Code: {lsoas_bolton$lsoa11cd}<br>
                    Name: {lsoas_bolton$lsoa11nmw} <br>
                    All age population: {lsoas_bolton$all_age_pop}"))

msoa_labels = (glue::glue("<b>MSOA</b><br>
                    Code: {msoas_bolton$msoa11cd}<br>
                    Name: {msoas_bolton$msoa11nmw}<br>
                    House of Commons Library name: {msoas_bolton$local_name}<br>
                    Covid cases rolling rate: {msoas_bolton$new_cases_by_specimen_date_rolling_rate}<br>
                    Covid cases date: {format(msoas_bolton$date, '%d/%m/%Y')}"))

# make colour palatte 
msoa_colours <- colorNumeric(
  palette = "Greens", 
  domain = msoas_bolton$new_cases_by_specimen_date_rolling_rate, 
  na.color = "white")

leaflet() %>%
  addResetMapButton() %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addPolylines(data = neighbourhood_boundaries, weight = 4, color = "black") %>%
  addPolygons(data = msoas_bolton, weight = 0.75, color = "grey", 
              fillColor = ~msoa_cases_colours(new_cases_by_specimen_date_rolling_rate), fillOpacity = 0.5, group = "MSOA cases",
              highlight = highlightOptions(weight = 4, color = "grey", bringToFront = TRUE),
              popup = ~msoa_labels, 
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))


###################### nomis ##########################

# https://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1249907237,1249907272,1249907273,1249907276,1249907277,1249907257,1249907259,1249907274,1249907275,1249907279,1249907238