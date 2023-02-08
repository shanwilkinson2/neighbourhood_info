# rename neighbourhoods


# set wd as app folder
  setwd(./bolton_neighbourhoods)
# read in app files
  
  msoa_neighbourhood_multiple <- readRDS("msoa_neighbourhood_multiple.RDS")
  
  neighbourhood_names <- unique(msoa_neighbourhood_multiple$neighbourhood) %>%
    sort()
  
  neighbourhood_boundaries <- readRDS("neighbourhood boundaries.RDS")
  
# new names

newnames <- data.frame(nb_num = c(1, 2, 3, 4, 5, 6), 
                       neighbourhood = c("1 - South",
                                   "2 - West",
                                   "3 - North",
                                   "4 - East",
                                   "5 - Central S",
                                   "6 - Central N")
                       )

neighbourhood_indicators2 <- neighbourhood_indicators %>%
  select(-neighbourhood) %>%
  left_join(newnames, by = c("x6_areas_number" = "nb_num")) %>%
  relocate(neighbourhood, .after = x6_areas_number)

msoa_neighbourhood_multiple2 <- msoa_neighbourhood_multiple %>%
  select(-neighbourhood) %>%
  left_join(newnames, by = c("x6_areas_number" = "nb_num")) %>%
  relocate(neighbourhood, .after = x6_areas_number)

neighbourhood_boundaries2 <- neighbourhood_boundaries %>%
  select(-neighbourhood_name) %>%
  left_join(newnames %>% 
              rename(neighbourhood_name = neighbourhood), 
            by = c("neighbourhood_num" = "nb_num")) %>%
  relocate(neighbourhood_num, .after = neighbourhood_num)

saveRDS(neighbourhood_indicators2, "neighbourhood_indicators.RDS")
saveRDS(msoa_neighbourhood_multiple2, "msoa_neighbourhood_multiple.RDS")
saveRDS(neighbourhood_boundaries2, "neighbourhood_boundaries.RDS")