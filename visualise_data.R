# map/ chart files

library(dplyr)
# library(shinycssloaders)
# library(fingertipsR)
library(data.table)
#library(nomisr)
#library(readxl)

##################### map ###############################

library(sf)
library(leaflet)
library(leaflet.extras)

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

###################### chart ##########################

library(plotly)

neighbourhood_indicators <- readRDS("./bolton_neighbourhoods/neighbourhood_indicators.RDS") # in the app folder

neighbourhood_name <- "Turton"
neighbourhood_name <- "Central/Great Lever"
neighbourhood_name <- "Chorley Roads"

indicator_name <- "Percentage of the total resident population who are 65 and over"

chart_data <- neighbourhood_indicators %>%
  st_drop_geometry() %>%
  filter(neighbourhood == neighbourhood_name &   
           IndicatorName == indicator_name) %>%
  #group_by(neighbourhood) %>%
  slice(1) 

# chart
plot_ly() %>%
  add_trace(type = "box",
            y =1,
            q1 = chart_data$nbourhood_min,
            # calculated value if it's available otherwise median
            median = ifelse(is.na(chart_data$nbourhood_pct), chart_data$nbourhood_median,chart_data$nbourhood_pct),
            q3 = chart_data$nbourhood_max,
            lowerfence = chart_data$bolton_min,
            upperfence = chart_data$bolton_max,
            notchspan = 0.3
  )

# chart
plot_ly() %>%
  add_trace(type = "box",
            y = list("Bolton", "Neighbourhood"),
            q1 = list(chart_data$bolton_min, chart_data$nbourhood_min),
            # calculated value if it's available otherwise median
            median = list(chart_data$bolton_value,
                          ifelse(is.na(chart_data$nbourhood_pct), 
                                 chart_data$nbourhood_median,
                                 chart_data$nbourhood_pct)
            ),
            q3 = list(chart_data$bolton_max, chart_data$nbourhood_max),
            #min = list(chart_data$bolton_min, chart_data$nbourhood_min),
            #  upperfence = chart_data$bolton_max,
            notchspan = list(0.3, 0.3),
            #hovertemplate = "some text {chart_data$bolton_max}",
            hovertext = ~chart_data$nbourhood_count,
            color = "orange"
  ) %>%
  layout(title = indicator_name,
         xaxis = list(hoverformat = ".1f",
                      title = "Area overall value & maximum & minimum"))

chart_data %>%
  plot_ly() %>%
  add_trace(type = "box",
            y = list("Bolton", "Neighbourhood"),
            q1 = ~list(bolton_min, nbourhood_min),
            # calculated value if it's available otherwise median
            median = ~list(bolton_value,
                           ifelse(is.na(nbourhood_pct), 
                                  nbourhood_median,
                                  nbourhood_pct)
            ),
            q3 = ~list(bolton_max, nbourhood_max),
            notchspan = list(0.3, 0.3),
            # hovertext = ~chart_data$nbourhood_count,
            color = "orange"
  ) %>%
  layout(title = ~IndicatorName,
         xaxis = list(hoverformat = ".1f",
                      title = "Area overall value & maximum & minimum"))

### table for chart - want to have diff areas in diff rows. NOT WORKING YET

library(sf)

neighbourhood_data <- neighbourhood_indicators %>%
  st_drop_geometry() %>%
  group_by(IndicatorID, Sex, Age, neighbourhood) %>%
  slice(1) %>% # first row only per neighbourhood, so will give neighbourood values as same for every msoa in the neighbourhood
  ungroup() %>%
  select(-c(msoa11cd, ParentCode:AreaType, Value:hoc_msoa_name)) %>%
  relocate(nbourhood_min, .before = nbourhood_max) %>%
  mutate(across(.cols = nbourhood_pct:england_q3, 
                .fns = ~round(.x, 1)
  ))

neighbourhood_data2 <-neighbourhood_data %>%
  filter(IndicatorName == "Child Poverty, Income deprivation affecting children index (IDACI)"  &
           neighbourhood == "Turton"
         ) 

library(magrittr)
neighbourhood_data3 <-neighbourhood_data2 %$%
  data.frame(area = c(neighbourhood, "Bolton", "England")
                                 ,
                                 min = c(nbourhood_min, 
                                         bolton_min, 
                                         england_min)
                                 ,
                                 q1 = c(nbourhood_q1, 
                                        bolton_q1, 
                                        england_q1)
                                 ,
                                 value = c(ifelse(!is.na(nbourhood_pct), 
                                                  nbourhood_pct, 
                                                  nbourhood_median),
                                           bolton_value, 
                                           england_value)
                                 ,
                                 q3 = c(nbourhood_q3, 
                                        bolton_q3, 
                                        england_q3)
                                 ,
                                 max = c(nbourhood_max, 
                                         bolton_max, 
                                         england_max)
             )

# boxplot


neighbourhood_data2 %>%
  plot_ly() %>%
  add_trace(type = "box",
            y = list(
              "England",
              "Bolton", 
              # neighbourhood_data2$neighbourhood
              "Turton"
            ),
            q1 = list(
              neighbourhood_data2$england_q1,
              # neighbourhood_data2$bolton_q1, 
              10.3,
              11#5.3
              # neighbourhood_data2$nbourhood_q1
            ),
            # lowerfence = list(
            #   neighbourhood_data2$england_min,
            #   neighbourhood_data2$bolton_min,
            #   neighbourhood_data2$nbourhood_min
            # ),
            # calculated value if it's available otherwise median
            median = list(
              neighbourhood_data2$england_value,
              # neighbourhood_data2$bolton_value,
              21.9,
              20 #10.4
              # neighbourhood_data2$nbourhood_pct
              # ifelse(is.na(neighbourhood_data2$nbourhood_pct),
              #        neighbourhood_data2$nbourhood_median,
              #        neighbourhood_data2$nbourhood_pct)
            ),
            q3 = list(neighbourhood_data2$england_q3,
                      # neighbourhood_data2$bolton_q3,
                      29,
                      28 #9.6
                      # neighbourhood_data2$nbourhood_q3
            ),
            # upperfence = list(
            #   neighbourhood_data2$england_max,
            #   neighbourhood_data2$bolton_max,
            #   neighbourhood_data2$nbourhood_max
            # ),
            # notchspan = list(0.3, 0.3),
            #hovertemplate = "some text {boxplot_data()$bolton_max}",
            #hovertext = ~chart_data$nbourhood_count,
            color = "orange"
  ) %>%
  layout(title = neighbourhood_data2$IndicatorName,
         xaxis = list(hoverformat = ".1f",
                      title = "Area values"))
