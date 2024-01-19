# create file "local_health_processed.RDS"
local_health_processed <- readRDS("local_health_processed.RDS")

#################### load packages ################################

library(dplyr)
library(data.table)


################# get data from phe fingertips local health #########################  

# open neighbourhood lookup to get just bolton msoas
bolton_msoa_codes <- readRDS("msoas_neighbourhood_multiple3.RDS") %>%
  select(-c(neighbourhood_num, neighbourhood_name)) %>%
  unique()

############

  
# local health 
# https://fingertips.phe.org.uk/profile/local-health/data#page/0/gid/1938133180/ati/3/iid/93744/age/28/sex/4/cid/4/tbm/1

# gets indicator details direct from API as fingertipsR is no longer on cran

### downloads metadata off fingertips ###################

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

## download data ##################################    
    
  # get borough local health data
    # gets Bolton & England from API direct as csv. only available as csv download
    # ProfileID = 143,
    # AreaTypeID = 402, # 402 = UTLA with boundary changes post Apr 2021
    # Area
    # AreaCode = "E08000001" # bolton

    local_health_borough <- fread("https://fingertips.phe.org.uk/api/all_data/csv/by_profile_id?child_area_type_id=402&parent_area_type_id=3&profile_id=143&parent_area_code=E08000001") %>%
      janitor::clean_names(case = "upper_camel") # upper camel case used in API json output & fingertipsR

    # get all msoas local health data - takes a bit of a while
    local_health_all_msoa <- fread("https://fingertips.phe.org.uk/api/all_data/csv/by_profile_id?child_area_type_id=3&parent_area_type_id=15&profile_id=143") %>%
      janitor::clean_names(case = "upper_camel") # upper camel case used in API json output & fingertipsR

## msoa ####################################    
    
    
    # msoa z score
    msoa_standardised <- local_health_all_msoa %>%
      select(c(IndicatorId, IndicatorName, 
               AreaCode, AreaName, AreaType, 
               Sex, Age, 
               Value,
               Count, Denominator,
               TimePeriodSortable,
      )) %>%
      filter(AreaType == "MSOA") %>%
      # keep latest value only - only seems to include latest anyway
      group_by(IndicatorId, Sex, Age) %>%
        filter(TimePeriodSortable == max(TimePeriodSortable)) %>%
      # get standardised value
        mutate(
          msoa_z = (Value - mean(Value, na.rm = TRUE))/ sd(Value, na.rm = TRUE)
        ) %>%
      # get england values from all msoas
      mutate(
        england_min = min(Value, na.rm = TRUE),
        england_max = max(Value, na.rm = TRUE),
        england_q1 = quantile(Value, 0.25, na.rm = TRUE),
        england_median = median(Value, na.rm = TRUE),
        england_q3 = quantile(Value, 0.75, na.rm = TRUE)) %>%
      # areaname is now MSOA hoc name, no longer e.g. "Bolton 001" so can't filter by that. 
      # want Bolton only now have used all for standardisation
        filter(AreaCode %in% bolton_msoa_codes$msoa_code) %>%
      # get bolton values now it's just bolton msoas
      mutate(bolton_min = min(Value, na.rm = TRUE),
             bolton_max = max(Value, na.rm = TRUE),
             bolton_q1 = quantile(Value, 0.25, na.rm = TRUE),
             bolton_median = median(Value, na.rm = TRUE),
             bolton_q3 = quantile(Value, 0.75, na.rm = TRUE)) %>%
      ungroup()        
    
## get england & bolton values ##########################################    
    
    # get borough data 
    local_health_bolton <- local_health_borough %>%
      filter(AreaName == "Bolton" & 
               TimePeriodSortable == max(TimePeriodSortable)) %>%
      select(c(IndicatorId, IndicatorName, 
               AreaCode, AreaName, AreaType, 
               Sex, Age, 
               Value,
               )) 
      
    
    # get england data 
    local_health_england <- local_health_borough %>%
      filter(AreaName == "England" &
               TimePeriodSortable == max(TimePeriodSortable)) %>%
      select(c(IndicatorId, IndicatorName, 
               AreaCode, AreaName, AreaType, 
               Sex, Age, 
               Value,
      ))

    
  # join england & bolton data
    local_health_bolton_eng <- full_join(local_health_bolton %>% 
                                select(IndicatorId, IndicatorName, Sex, Age, Value), 
                              local_health_england %>%
                                select(IndicatorId, IndicatorName, Sex, Age, Value),
                              by = c("IndicatorId", "IndicatorName", "Sex", "Age"),
                              suffix = c("_bolton", "_england"), 
                              relationship = "many-to-many")
    
    # tidyup
    rm(local_health_bolton)
    rm(local_health_england)
    
    
## get neighbourhood lookup #########################################

# # MSOA best fit (local health doesn't go down to lsoa)
#   msoa_neighbourhood <- readRDS("msoas_neighbourhood.RDS")
# includes value for Bolton to keep whole borough value
  # version where MSOAs appear in more than 1 neighbourhood
  msoa_neighbourhood_multiple <- readRDS("msoas_neighbourhood_multiple3.RDS")
  

####### transform to neighbourhood level ##############################################################

    nbourhood_indicators <- msoa_standardised %>%
      # add neighbourhoods (msoa may be in several)
      left_join(msoa_neighbourhood_multiple,
                by = c("AreaCode" = "msoa_code"),
                relationship = "many-to-many") %>%
      relocate(neighbourhood_name:hoc_msoa_name, .after = AreaName) %>%
      group_by(IndicatorId, Sex, Age, neighbourhood_name) %>%
      mutate(
             nbourhood_count = sum(Value),
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
      ) 
  
  ### add in bolton & england ##################################
 
  nbourhood_indicators2 <- left_join(
    nbourhood_indicators, 
    local_health_bolton_eng %>%
      select(-IndicatorName),
    by = c("IndicatorId", "Sex", "Age")
  )
    
  # add in Domain name ############################
    
  nbourhood_indicators3 <- nbourhood_indicators2 %>%
      left_join(local_health_indicators %>% 
                  select(IndicatorId, DomainName), 
                by = "IndicatorId") %>%
      relocate(DomainName, .after = IndicatorName) %>%
      mutate(DomainName = paste("Local health -", DomainName),
             AreaName = msoa_name,
             # add sex into indicator name if its' split m/f
             IndicatorName = ifelse(Sex == "Male" | Sex == "Female",
                                    paste0(IndicatorName, " (", Sex, ")"),
                                    IndicatorName)
             ) %>%
      select(-msoa_name)

      
  
  # save
  saveRDS(nbourhood_indicators3, "local_health_processed.RDS")

  # cleanup
  rm(local_health_all_msoa)
  rm(local_health_borough)
  rm(local_health_indicators)
  rm(msoa_neighbourhood_multiple)
  rm(msoa_standardised)
  rm(nbourhood_indicators)
  rm(nbourhood_indicators2)
  rm(nbourhood_indicators3)
  rm(bolton_msoa_codes)
  rm(local_health_bolton_eng)
  
  #################### remove existing to update
  
  msoa_data <- readRDS("./bolton_neighbourhoods/neighbourhood_indicators.RDS") %>%
    filter(stringr::str_detect(DomainName, "Deprivation|Census 2021 -"))

  
  msoa_data %>%
    bind_rows(nbourhood_indicators3) %>%
    saveRDS("./bolton_neighbourhoods/neighbourhood_indicators2.RDS")  
  

