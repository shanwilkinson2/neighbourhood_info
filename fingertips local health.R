# fingertipsR is no longer on CRAN
install.packages("fingertipsR", repos = "https://dev.ropensci.org")
# also need "shinycssloaders" 
install.packages("shinycssloaders")
install.packages("miniUI")

#################### load packages ################################

library(dplyr)
# library(shinycssloaders)
library(fingertipsR)
#library(data.table)
#library(nomisr)
#library(readxl)

################# get data from phe fingertips local health #########################  

  # downloads data, adds boundaries, saves nbourhood_indicators3 as neighbourhood_indicators for app
  
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

  # # get msoa local health data
  #   local_health_msoa <- fingertips_data(IndicatorID = local_health_indicators$IndicatorID,
  #                                       ProfileID = 143,
  #                                       AreaTypeID = 3, # 3 = MSOA
  #                                       AreaCode = bolton_area_codes
  #                                       )
    
  # get borough local health data
    local_health_borough <- fingertips_data(IndicatorID = local_health_indicators$IndicatorID,
                                       ProfileID = 143,
                                       AreaTypeID = 402, # 402 = UTLA with boundary changes post Apr 2021
                                       AreaCode = "E08000001" # bolton
                                        ) 
    
    # get all msoas local health data - takes a bit of a while
    local_health_all_msoa <- fingertips_data(IndicatorID = local_health_indicators$IndicatorID,
                                         ProfileID = 143,
                                         AreaTypeID = 3#, # 3 = MSOA
                                         # AreaCode = bolton_area_codes
                                          ) 
    
    # filter just bolton
    local_health_msoa <- local_health_all_msoa %>%
      filter(AreaCode %in% bolton_area_codes)

  # join msoa & borough data
    local_health <- bind_rows(local_health_msoa, local_health_borough)

# # MSOA best fit (local health doesn't go down to lsoa)
#   msoa_neighbourhood <- readRDS("msoas_neighbourhood.RDS")
# includes value for Bolton to keep whole borough value
  # version where MSOAs appear in more than 1 neighbourhood
  msoa_neighbourhood_multiple <- data.table::fread("msoas_neighbourhood_multiple.csv")

    # add in neighbourhood using multiple file
    bolton_local_health2 <- full_join(local_health, msoa_neighbourhood_multiple, 
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

# combine indicators but keep msoa level so can have 1 dataset
  
  nbourhood_indicators <- bolton_local_health2 %>%
    group_by(IndicatorID, Sex, Age, TimeperiodSortable, neighbourhood) %>%
    mutate(nbourhood_count = sum(Count), 
           nbourhood_denominator = sum(Denominator),
           nbourhood_pct = nbourhood_count/ nbourhood_denominator*100,
           nbourhood_median = median(Value),
           nbourhood_max = max(Value),
           nbourhood_min = min(Value),
           nbourhood_q1 = quantile(Value, 0.25, na.rm = TRUE),
           nbourhood_q3 = quantile(Value, 0.75, na.rm = TRUE)
           ) %>%
    ungroup() %>%
    # bolton min & max
    group_by(IndicatorID, Sex, Age, TimeperiodSortable) %>%
    mutate(bolton_min = min(Value),
           bolton_max = max(Value),
           bolton_q1 = quantile(Value, 0.25, na.rm = TRUE),
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
      mutate(england_min = min(Value),
             england_max = max(Value),
             england_q1 = quantile(Value, 0.25, na.rm = TRUE),
             england_q3 = quantile(Value, 0.75, na.rm = TRUE)) %>%
      slice(1)
    
    # combined for joining
    england_values <- full_join(
      england_indicators %>%
        select(IndicatorID, Sex, Age, TimeperiodSortable, Value)
      ,
      england_min_max %>%
        select(IndicatorID, Sex, Age, TimeperiodSortable, england_min, england_max, england_q1, england_q3)
      ,
      by = c("IndicatorID", "Sex", "Age", "TimeperiodSortable")
    )
  
    # join in england
    nbourhood_indicators2b <- left_join(nbourhood_indicators2, 
                                        england_values %>%
                                          rename("england_value"= "Value"),
                                        by = c("IndicatorID", "Sex", "Age", "TimeperiodSortable"), 
                                        suffix = c("", "_england"))
  
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

  
# get rid of seperate & intermediate files
   rm(bolton_local_health2, england_indicators, england_min_max, england_values, local_health, 
     local_health_msoa, local_health_borough,
     local_health_all_msoa, local_health_indicators, msoa_boundaries, msoa_neighbourhood_multiple,nbourhood_indicators,
     nbourhood_indicators2, nbourhood_indicators2b, nbourhood_indicators3, bolton_area_codes
     )
  
###################### nomis ##########################

# https://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1249907237,1249907272,1249907273,1249907276,1249907277,1249907257,1249907259,1249907274,1249907275,1249907279,1249907238