
#################### load packages ################################

library(dplyr)
library(data.table)
# library(shinycssloaders)
#library(readxl)

################# get data from phe fingertips local health #########################  

app_location <- "./bolton_neighbourhoods_app/"

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

  # get borough local health data
    # gets Bolton & England from API direct as csv. only available as csv download
    # ProfileID = 143,
    # AreaTypeID = 402, # 402 = UTLA with boundary changes post Apr 2021
    # Area
    # AreaCode = "E08000001" # bolton

    local_health_borough <- fread("https://fingertips.phe.org.uk/api/all_data/csv/by_profile_id?child_area_type_id=402&parent_area_type_id=3&profile_id=143&parent_area_code=E08000001") %>%
      janitor::clean_names(case = "upper_camel") # upper camel case used in API json output & fingertipsR

    # # prone to breaking...
    # saveRDS(local_health_borough, "local_health_borough.RDS")
    # local_health_borough <- readRDS("local_health_borough.RDS")

    # get all msoas local health data - takes a bit of a while
    local_health_all_msoa <- fread("https://fingertips.phe.org.uk/api/all_data/csv/by_profile_id?child_area_type_id=3&parent_area_type_id=15&profile_id=143") %>%
      janitor::clean_names(case = "upper_camel") # upper camel case used in API json output & fingertipsR
    
    # # prone to breaking...
    # saveRDS(local_health_all_msoa, "local_health_all_msoa.RDS")
    # local_healht_all_msoa <- readRDS("local_health_all_msoa.RDS")

    # filter just bolton
    local_health_bolton_msoa <- local_health_all_msoa %>%
      filter(stringr::str_detect(AreaName, "^Bolton") & AreaType == "MSOA")

  # join msoa & borough data
    local_health <- bind_rows(local_health_bolton_msoa, local_health_borough)

# # MSOA best fit (local health doesn't go down to lsoa)
#   msoa_neighbourhood <- readRDS("msoas_neighbourhood.RDS")
# includes value for Bolton to keep whole borough value
  # version where MSOAs appear in more than 1 neighbourhood
  msoa_neighbourhood_multiple <- data.table::fread("msoas_neighbourhood_multiple2.csv")
 # saveRDS(msoa_neighbourhood_multiple, paste0(app_location, "msoa_neighbourhood_multiple.RDS"))
  
  # save to app folder
  

    # add in neighbourhood using multiple file
    bolton_local_health2 <- full_join(local_health, msoa_neighbourhood_multiple, 
                                      by = c("AreaName"= "msoa_name")) %>%
      # keep latest value only - only seems to include latest anyway
      group_by(IndicatorId, Sex, Age, AreaName) %>%
      filter(TimePeriodSortable == max(TimePeriodSortable)) %>%
      ungroup() %>%
      # add in domain ie part of the profile 
      left_join(local_health_indicators %>% select(-IndicatorName),
                by = "IndicatorId") %>%
      arrange(GroupId) %>%
      group_by(IndicatorId, Sex, Age, neighbourhood)  
        
####### transform to neighbourhood level ##############################################################

    # msoa z score
    msoa_standardised <- local_health_all_msoa %>%
      filter(AreaType == "MSOA") %>%
      # keep latest value only - only seems to include latest anyway
      group_by(IndicatorId, Sex, Age) %>%
      filter(TimePeriodSortable == max(TimePeriodSortable)) %>%
      mutate(
        msoa_z = (Value - mean(Value, na.rm = TRUE))/ sd(Value, na.rm = TRUE)
      ) %>%
      filter(stringr::str_detect(AreaName, "^Bolton"))  %>%
    ungroup()        
    
# combine indicators but keep msoa level so can have 1 dataset
  
    nbourhood_indicators <- bolton_local_health2 %>%
      left_join(msoa_standardised %>%
                  select(IndicatorId, Sex, Age, TimePeriodSortable, AreaCode, msoa_z), 
                by = c("IndicatorId", "Sex", "Age", "TimePeriodSortable", "AreaCode")
      ) %>%
      group_by(IndicatorId, Sex, Age, TimePeriodSortable, neighbourhood) %>%
      mutate(nbourhood_count = sum(Count), 
             nbourhood_denominator = sum(Denominator),
             nbourhood_pct = nbourhood_count/ nbourhood_denominator*100,
             nbourhood_median = median(Value, na.rm = TRUE),
             nbourhood_max = max(Value, na.rm = TRUE),
             nbourhood_min = min(Value, na.rm = TRUE),
             nbourhood_q1 = quantile(Value, 0.25, na.rm = TRUE),
             nbourhood_q3 = quantile(Value, 0.75, na.rm = TRUE),
             z_nbourhood_median = median(msoa_z, na.rm = TRUE),
             z_nbourhood_max = max(msoa_z, na.rm = TRUE),
             z_nbourhood_min = min(msoa_z, na.rm = TRUE),
             z_nbourhood_q1 = quantile(msoa_z, 0.25, na.rm = TRUE),
             z_nbourhood_q3 = quantile(msoa_z, 0.75, na.rm = TRUE),
             z_nbourhoood_median_abs = abs(z_nbourhood_median),
             z_nbourhood_iqr_abs = abs(z_nbourhood_q3 - z_nbourhood_q1),
             z_nbourhood_range_abs = abs(z_nbourhood_max - z_nbourhood_min)
      ) %>%
      ungroup() %>%
      # get direction of absolute values
      mutate(
        z_nbourhood_median_abs_direction = case_when(
          z_nbourhood_median >1.96 ~ "much higher", # 95% of a normal distribution lie between +1.96 & -1.96
          z_nbourhood_median >0 ~ "higher",
          z_nbourhood_median <0 ~ "lower",
          z_nbourhood_median <1.96 ~ "much lower",
          z_nbourhood_median ==0 ~ "average")
      ) %>%
      # bolton min & max
      group_by(IndicatorId, Sex, Age, TimePeriodSortable) %>%
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
      select(IndicatorId, Sex, Age, TimePeriodSortable, bolton_value = nbourhood_median), # median will be the value as all bolton
    by = c("IndicatorId", "Sex", "Age", "TimePeriodSortable"),
    suffix = c("_neighbourhood", "_bolton")
  )
  
  # get England values 

  # actual England figure
  england_indicators <- local_health_all_msoa %>%
    filter(AreaType == "England") %>%
    # keep latest value only - only seems to include latest anyway
    group_by(IndicatorId, Sex, Age) %>%
    filter(TimePeriodSortable == max(TimePeriodSortable)) %>%
    ungroup()
  
  # england MSOA max/min
  england_min_max <- local_health_all_msoa %>%
    filter(AreaType == "MSOA") %>%
    # keep latest value only - only seems to include latest anyway
    group_by(IndicatorId, Sex, Age) %>%
    filter(TimePeriodSortable == max(TimePeriodSortable)) %>%
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
      select(IndicatorId, Sex, Age, TimePeriodSortable, Value)
    ,
    england_min_max %>%
      select(IndicatorId, Sex, Age, TimePeriodSortable, england_min: england_q3)
    ,
    by = c("IndicatorId", "Sex", "Age", "TimePeriodSortable")
  )
  
  # join in england
  nbourhood_indicators2b <- left_join(nbourhood_indicators2, 
                                      england_values %>%
                                        rename("england_value"= "Value"),
                                      by = c("IndicatorId", "Sex", "Age", "TimePeriodSortable"), 
                                      suffix = c("", "_england")) %>%
    # move absolute median z direction as it's not numeric so mutate across to 1 decimal place still works
    relocate(z_nbourhood_median_abs_direction, .after = england_q3) %>%
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
                 ) %>%
    mutate(DomainName = paste("Local health -", DomainName))

  
# save for app ###############################
  saveRDS(nbourhood_indicators3, "./bolton_neighbourhoods_app/neighbourhood_indicators.RDS")

  #################### remove existing to update
  
  msoa_data <- readRDS("./bolton_neighbourhoods/neighbourhood_indicators.RDS") %>%
    filter(stringr::str_detect(DomainName, "Deprivation|Census 2021 -"))

  
  msoa_data %>%
    bind_rows(nbourhood_indicators3) %>%
    saveRDS("./bolton_neighbourhoods/neighbourhood_indicators2.RDS")  
  
  
# get rid of separate & intermediate files
   rm(bolton_local_health2, england_indicators, england_min_max, england_values, local_health, 
     local_health_bolton_msoa, local_health_borough,
     local_health_all_msoa, local_health_indicators, msoa_boundaries, msoa_neighbourhood_multiple,nbourhood_indicators,
     nbourhood_indicators2, nbourhood_indicators2b, nbourhood_indicators3,
     )
  
