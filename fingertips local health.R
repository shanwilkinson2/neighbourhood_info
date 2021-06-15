# fingertipsR is no longer on CRAN
install.packages("fingertipsR", repos = "https://dev.ropensci.org")
# also need "shinycssloaders" 
install.packages("shinycssloaders")


library(dplyr)
library(fingertipsR)
library(data.table)
library(nomisr)
library(readxl)

# local health 
# https://fingertips.phe.org.uk/profile/local-health/data#page/0/gid/1938133180/ati/3/iid/93744/age/28/sex/4/cid/4/tbm/1

fingertips_live_indicators <- indicators()
local_health_indicators <- fingertips_live_indicators %>%
  filter(ProfileID == 143)

local_health <- fingertips_data(IndicatorID = local_health_indicators$IndicatorID,
                                  ProfileID = 143 #,
                                # AreaTypeID = c(3, 202, 302, 402) #  - doesn't seem to work202, 302, 402 UTLA with boundary changes, MSOA = 3
                                ) 

bolton_local_health <- local_health %>%
  filter(ParentName == "Bolton" | AreaName == "Bolton")

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
  
  
  
  ###################### nomis ##########################

# https://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1249907237,1249907272,1249907273,1249907276,1249907277,1249907257,1249907259,1249907274,1249907275,1249907279,1249907238...1249907240,1249907242,1249907278,1249907246,1249907349,1249907356...1249907358,1249907235,1249907236,1249907241,1249907243,1249907292,1249907253...1249907255,1249907258,1249907260,1249907261,1249907248,1249907250,1249907350...1249907352,1249907332,1249907335,1249907388,1249907391,1249907392,1249907247,1249907249,1249907251,1249907252,1249907271,1249907393,1249907394,1249907396,1249907289,1249907293,1249907294,1249907333,1249907334,1249907308...1249907310,1249907386,1249907387,1249907256,1249907262...1249907264,1249907266,1249907244,1249907353...1249907355,1249907336...1249907339,1249907389,1249907290,1249907291,1249907316,1249934947,1249934950,1249907287,1249907288,1249907395,1249907397...1249907399,1249907302,1249907306,1249907311,1249907313,1249907390,1249907265,1249907267...1249907270,1249907384,1249907303...1249907305,1249907312,1249907365,1249907295,1249907317...1249907319,1249907321,1249907280...1249907282,1249907284,1249907286,1249907296,1249907297,1249907300,1249907322,1249907346,1249907378...1249907383,1249907385,1249907314,1249907315,1249907320,1249907345,1249907347,1249907298,1249907299,1249907301,1249907307,1249907283,1249907323...1249907325,1249907359,1249907361,1249907362,1249907368,1249934948,1249934949,1249907285,1249907340,1249907341,1249907343,1249907348,1249907360,1249907364,1249907366,1249907367,1249907245,1249907363,1249907403,1249907405,1249907406,1249907326...1249907328,1249907369,1249907373,1249907329...1249907331,1249907342,1249907344,1249907370...1249907372,1249907374...1249907377,1249907400...1249907402,1249907404,1249907407&date=latest&gender=0&c_age=200,1,3...18,210&measures=20100