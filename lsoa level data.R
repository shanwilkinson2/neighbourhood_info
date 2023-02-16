# get lsoa data - minimum needed
 # imd, life expectancy, population

library(dplyr)
library(openxlsx)

lsoa_neighbourhood <- read.xlsx("6 neighbourhoods final option.xlsx") %>%
  janitor::clean_names()

# english indices of deprivation 2019
  # https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019

  imd_overall <- read.xlsx("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx",
                           sheet = 2) %>%
    janitor::clean_names()
  