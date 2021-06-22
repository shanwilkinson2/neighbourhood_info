# fingertipsR is no longer on CRAN
install.packages("fingertipsR", repos = "https://dev.ropensci.org")
# also need "shinycssloaders" 
install.packages("shinycssloaders")
install.packages("miniUI")


library(dplyr)
library(shinycssloaders)
library(fingertipsR)
library(data.table)
library(nomisr)
library(readxl)
library(sf)
library(leaflet)
library(leaflet.extras)

msoa_boundaries <- readRDS("msoa boundaries.RDS")
neighbourhood_boundaries <- readRDS("neighbourhood boundaries.RDS")
local_health_data_msoa <- readRDS("local health data with boundaries.RDS")

# local health 
# https://fingertips.phe.org.uk/profile/local-health/data#page/0/gid/1938133180/ati/3/iid/93744/age/28/sex/4/cid/4/tbm/1


# get data off fingertips local health
  # fingertips_live_indicators <- indicators() # all indicators
# available indicators in local health profile
  local_health_indicators <- indicators(ProfileID = 143)

# get Bolton msoa data for all local health indicators  
  bolton_area_codes <- c( #"E08000001", # bolton itself
    "E02000984", "E02000985", "E02000986", "E02000987",
    "E02000988", "E02000989", "E02000990", "E02000991", "E02000992",
    "E02000993", "E02000994", "E02000995", "E02000996", "E02000997",
    "E02000998", "E02000999", "E02001000", "E02001001", "E02001002",
    "E02001003", "E02001004", "E02001005", "E02001006", "E02001007",
    "E02001008", "E02001009", "E02001010", "E02001011", "E02001012",
    "E02001013", "E02001014", "E02001015", "E02001016", "E02001017",
    "E02001018")
  
  local_health_msoa <- fingertips_data(IndicatorID = local_health_indicators$IndicatorID,
                                  ProfileID = 143,
                                  AreaTypeID = 3, # "All" #  - doesn't seem to work202, 302, 402 UTLA with boundary changes, MSOA = 3
                                  AreaCode = bolton_area_codes
                                  ) 

  local_health_ua <- fingertips_data(IndicatorID = local_health_indicators$IndicatorID,
                                      ProfileID = 143,
                                      AreaTypeID = 402, # "All" #  - doesn't seem to work202, 302, 402 UTLA with boundary changes, MSOA = 3
                                      AreaCode = "E08000001" # bolton
  ) 

  # join msoa & ua
  local_health <- bind_rows(local_health_msoa, local_health_ua)
  
  # get rid of seperate files
  rm(local_health_msoa)
  rm(local_health_ua)

# MSOA best fit (local health doesn't go down to lsoa)

  msoa_neighbourhood <- read_xlsx("neighbourhoods_lsoa_lookup.xlsx", sheet = "msoa best fit")
  # includes value for Bolton to keep whole borough value
  
# add in neighbourhood
  bolton_local_health2 <- left_join(bolton_local_health, msoa_neighbourhood, 
                                    by = c("AreaName"= "msoa")) %>%
    # keep latest value only - only seems to include latest anyway
    group_by(IndicatorID, Sex, Age, AreaName) %>%
    filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
    ungroup() %>%
    # add in domain ie part of the profile 
    left_join(fingertips_live_indicators %>% 
      select(IndicatorID, DomainID, DomainName, ProfileID, ProfileName),
      by = "IndicatorID") %>%
  filter(ProfileID == 143) %>% # local health profile = 143, some indicators are in multiple
  arrange(ProfileID, DomainID) %>%
    group_by(IndicatorID, Sex, Age, neighbourhood)
 
  
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
  
# indicators
  
bolton_local_health %>%
  select(IndicatorID, IndicatorName) %>%
left_join(fingertips_live_indicators %>% 
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
  
  ###################### boundaries #####################


# Bolton boundary clipped to 20m https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-bgc
la_boundary <- st_read("https://opendata.arcgis.com/datasets/0e07a8196454415eab18c40a54dfbbef_0.geojson") %>%
  filter(lad19nm == "Bolton") %>%
  st_set_crs(4326) # lat long

# LSOA
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

lsoa_neighbourhood <- read_xlsx("neighbourhoods_lsoa_lookup.xlsx", sheet = "neighbourhoods_lsoa_lookup")
# includes value for Bolton to keep whole borough value

# add in neighbourhoood name
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

# saveRDS(neighbourhood_boundaries, "neighbourhood boundaries.RDS")

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

# house of commons library names
msoa_localnames <- read.csv("G:\\Mapping Data\\COV19/HoC library MSOA-Names-v1.1.0.csv") 

names(msoa_localnames)[1] <- "msoa11cd"

msoa_localnames <- msoa_localnames %>%
  select(msoa11cd, local_name = msoa11hclnm)

# merge in local names
msoas_bolton <- left_join(msoas_bolton, msoa_localnames, by = "msoa11cd")
rm(msoa_localnames) # remove localnames as no longer needed

msoas_bolton2 <- msoas_bolton %>%
  select(msoa11cd, msoa11nm, hoc_msoa_name = local_name) %>%
  left_join(msoa_neighbourhood,
            by = c("msoa11nm" = "msoa"))

# saveRDS(msoas_bolton2, "msoa boundaries.RDS")


##################### map ###############################

mytitle <- glue::glue("<b>CONFIDENTIAL PERSON IDENTIFIABLE | Sample locations mapped</b><br>
                      Walk at 5kmph (3mph) | Cases data downloaded {format(Sys.Date(), '%d/%m/%Y')} | Turn layers on & off to explore")

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

# https://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1249907237,1249907272,1249907273,1249907276,1249907277,1249907257,1249907259,1249907274,1249907275,1249907279,1249907238...1249907240,1249907242,1249907278,1249907246,1249907349,1249907356...1249907358,1249907235,1249907236,1249907241,1249907243,1249907292,1249907253...1249907255,1249907258,1249907260,1249907261,1249907248,1249907250,1249907350...1249907352,1249907332,1249907335,1249907388,1249907391,1249907392,1249907247,1249907249,1249907251,1249907252,1249907271,1249907393,1249907394,1249907396,1249907289,1249907293,1249907294,1249907333,1249907334,1249907308...1249907310,1249907386,1249907387,1249907256,1249907262...1249907264,1249907266,1249907244,1249907353...1249907355,1249907336...1249907339,1249907389,1249907290,1249907291,1249907316,1249934947,1249934950,1249907287,1249907288,1249907395,1249907397...1249907399,1249907302,1249907306,1249907311,1249907313,1249907390,1249907265,1249907267...1249907270,1249907384,1249907303...1249907305,1249907312,1249907365,1249907295,1249907317...1249907319,1249907321,1249907280...1249907282,1249907284,1249907286,1249907296,1249907297,1249907300,1249907322,1249907346,1249907378...1249907383,1249907385,1249907314,1249907315,1249907320,1249907345,1249907347,1249907298,1249907299,1249907301,1249907307,1249907283,1249907323...1249907325,1249907359,1249907361,1249907362,1249907368,1249934948,1249934949,1249907285,1249907340,1249907341,1249907343,1249907348,1249907360,1249907364,1249907366,1249907367,1249907245,1249907363,1249907403,1249907405,1249907406,1249907326...1249907328,1249907369,1249907373,1249907329...1249907331,1249907342,1249907344,1249907370...1249907372,1249907374...1249907377,1249907400...1249907402,1249907404,1249907407&date=latest&gender=0&c_age=200,1,3...18,210&measures=20100