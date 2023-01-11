
#################### load packages ################################

library(dplyr)
library(data.table)
# library(shinycssloaders)
#library(readxl)

################# get data from phe fingertips local health #########################  

  # downloads data, adds boundaries, saves nbourhood_indicators3 as neighbourhood_indicators for app
  
# local health 
# https://fingertips.phe.org.uk/profile/local-health/data#page/0/gid/1938133180/ati/3/iid/93744/age/28/sex/4/cid/4/tbm/1

# gets indicator details direct from API as fingertipsR is no longer on cran

# group names & numbers
    local_health_metadata <- httr::GET("https://fingertips.phe.org.uk/api/profile?profile_id=143")$content %>%
      rawToChar() %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      .$GroupMetadata %>%
      select(Id, Name)
    
    # get group metadata direct from API using group id's obtained from above
    local_health_group_metadata <- httr::GET("https://fingertips.phe.org.uk/api/indicator_names/by_group_id?group_ids=1938133180%2C%201938133183%2C1938133184%2C1938133185")$content %>%
      rawToChar() %>%
      jsonlite::fromJSON(flatten = TRUE)
    
    local_health_indicators <- local_health_group_metadata %>%
      left_join(local_health_metadata, by = c("GroupId" = "Id")) %>%
      rename(DomainName = Name) %>%
      relocate(DomainName, .after = "GroupId")
    
    # remove unjoined file
    rm(local_health_metadata)
    rm(local_health_group_metadata)

  # get borough local health data+
    # gets Bolton & England from API direct as csv. only available as csv download
    # ProfileID = 143,
    # AreaTypeID = 402, # 402 = UTLA with boundary changes post Apr 2021
    # Area
    # AreaCode = "E08000001" # bolton

    local_health_borough <- fread("https://fingertips.phe.org.uk/api/all_data/csv/by_profile_id?child_area_type_id=402&parent_area_type_id=3&profile_id=143&parent_area_code=E08000001") %>%
      janitor::clean_names()

    # # prone to breaking...
    # saveRDS(local_health_borough, "local_health_borough.RDS")
    # local_healht_borough <- readRDS("local_health_borough.RDS")

    # get all msoas local health data - takes a bit of a while
    local_health_all_msoa <- fread("https://fingertips.phe.org.uk/api/all_data/csv/by_profile_id?child_area_type_id=3&parent_area_type_id=15&profile_id=143") %>%
      janitor::clean_names()
    
    # # prone to breaking...
    # saveRDS(local_health_all_msoa, "local_health_all_msoa.RDS")
    # local_healht_all_msoa <- readRDS("local_health_all_msoa.RDS")

    # filter just bolton
    local_health_bolton_msoa <- local_health_all_msoa %>%
      filter(stringr::str_detect(area_name, "^Bolton") & area_type == "MSOA")

  # join msoa & borough data
    local_health <- bind_rows(local_health_bolton_msoa, local_health_borough)

# # MSOA best fit (local health doesn't go down to lsoa)
#   msoa_neighbourhood <- readRDS("msoas_neighbourhood.RDS")
# includes value for Bolton to keep whole borough value
  # version where MSOAs appear in more than 1 neighbourhood
  msoa_neighbourhood_multiple <- data.table::fread("msoas_neighbourhood_multiple2.csv")

    # add in neighbourhood using multiple file
    bolton_local_health2 <- full_join(local_health, msoa_neighbourhood_multiple, 
                                      by = c("area_name"= "msoa_name")) %>%
      # keep latest value only - only seems to include latest anyway
      group_by(indicator_id, sex, age, area_name) %>%
      filter(time_period_sortable == max(time_period_sortable)) %>%
      ungroup() %>%
      # add in domain ie part of the profile 
      left_join(local_health_indicators %>% 
                  select(IndicatorID, DomainID, DomainName, ProfileID, ProfileName),
                by = "IndicatorID") %>%
      filter(ProfileID == 143) %>% # local health profile = 143, some indicators are in multiple
      arrange(ProfileID, DomainID) %>%
      group_by(IndicatorID, Sex, Age, neighbourhood)  
        
####### transform to neighbourhood level ##############################################################

# combine indicators but keep msoa level so can have 1 dataset
  
  nbourhood_indicators <- bolton_local_health2 %>%
    group_by(IndicatorID, Sex, Age, TimeperiodSortable, neighbourhood) %>%
    mutate(nbourhood_count = sum(Count), 
           nbourhood_denominator = sum(Denominator),
           nbourhood_pct = nbourhood_count/ nbourhood_denominator*100,
           nbourhood_median = median(Value),
           nbourhood_max = max(Value, na.rm = TRUE),
           nbourhood_min = min(Value, na.rm = TRUE),
           nbourhood_q1 = quantile(Value, 0.25, na.rm = TRUE),
           nbourhood_q3 = quantile(Value, 0.75, na.rm = TRUE)
           ) %>%
    ungroup() %>%
    # bolton min & max
    group_by(IndicatorID, Sex, Age, TimeperiodSortable) %>%
    mutate(bolton_min = min(Value, na.rm = TRUE),
           bolton_max = max(Value, na.rm = TRUE),
           bolton_q1 = quantile(Value, 0.25, na.rm = TRUE),
           bolton_median = median(Value, na.rm = TRUE),
           bolton_q3 = quantile(Value, 0.75, na.rm = TRUE))
  
  # pivot to get bolton value in a different column
  nbourhood_indicators2 <- left_join(
    nbourhood_indicators %>%
      ungroup() %>%
      filter(neighbourhood != "Bolton"),
    nbourhood_indicators %>%
      ungroup() %>%
      filter(neighbourhood == "Bolton") %>%
      select(IndicatorID, Sex, Age, TimeperiodSortable, bolton_value = nbourhood_median), # median will be the value as all bolton
    by = c("IndicatorID", "Sex", "Age", "TimeperiodSortable"),
    suffix = c("_neighbourhood", "_bolton")
  )
  
  # get England values 
  
    # actual England figure
    england_indicators <- local_health_all_msoa %>%
      filter(AreaType == "England") %>%
      # keep latest value only - only seems to include latest anyway
      group_by(IndicatorID, Sex, Age) %>%
      filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
      ungroup()

    # england MSOA max/min
    england_min_max <- local_health_all_msoa %>%
      filter(AreaType == "MSOA") %>%
      # keep latest value only - only seems to include latest anyway
      group_by(IndicatorID, Sex, Age) %>%
      filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
      mutate(
        england_min = min(Value, na.rm = TRUE),
        england_max = max(Value, na.rm = TRUE),
        england_q1 = quantile(Value, 0.25, na.rm = TRUE),
        england_median = median(Value, na.rm = TRUE),
        england_q3 = quantile(Value, 0.75, na.rm = TRUE)) %>%
      slice(1)
    
    # combined for joining
    england_values <- full_join(
      england_indicators %>%
        select(IndicatorID, Sex, Age, TimeperiodSortable, Value)
      ,
      england_min_max %>%
        select(IndicatorID, Sex, Age, TimeperiodSortable, england_min: england_q3)
      ,
      by = c("IndicatorID", "Sex", "Age", "TimeperiodSortable")
    )
  
    # join in england
    nbourhood_indicators2b <- left_join(nbourhood_indicators2, 
                                        england_values %>%
                                          rename("england_value"= "Value"),
                                        by = c("IndicatorID", "Sex", "Age", "TimeperiodSortable"), 
                                        suffix = c("", "_england")) %>%
      # give new indicator name for sex disaggregated indicators
      mutate(IndicatorName = ifelse(!Sex %in% c("Persons", "Not applicable"), 
                                    paste(IndicatorName, Sex, sep = " - "),
                                    IndicatorName)
      )
  
  # get msoa boundaries    
    msoa_boundaries <- readRDS("msoa boundaries.RDS")
  
    # add msoa boundary
  nbourhood_indicators3 <- right_join(msoa_boundaries %>%
                                      select(msoa11cd), # only want the join field & geometry whcih sticks anyway
                                      nbourhood_indicators2b, # right join to keep geometry
               by = c("msoa11cd" = "msoa_code")
                 )

  
# save for app
  saveRDS(nbourhood_indicators3, "./bolton_neighbourhoods/neighbourhood_indicators.RDS")

  
# get rid of separate & intermediate files
   rm(bolton_local_health2, england_indicators, england_min_max, england_values, local_health, 
     local_health_msoa, local_health_borough,
     local_health_all_msoa, local_health_indicators, msoa_boundaries, msoa_neighbourhood_multiple,nbourhood_indicators,
     nbourhood_indicators2, nbourhood_indicators2b, nbourhood_indicators3, bolton_area_codes
     )
  
###################### nomis ##########################

# https://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1249907237,1249907272,1249907273,1249907276,1249907277,1249907257,1249907259,1249907274,1249907275,1249907279,1249907238