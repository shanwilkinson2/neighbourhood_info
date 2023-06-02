# load packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(magrittr)
library(DT)

# load static datasets
data_refresh_date <- "05/05/2023"

# neighbourhood/ msoa lookup table
msoa_neighbourhood_multiple <- readRDS("msoa_neighbourhood_multiple.RDS")

neighbourhood_names <- unique(msoa_neighbourhood_multiple$neighbourhood) %>%
  sort()

neighbourhood_boundaries <- readRDS("neighbourhood_boundaries.RDS")

# msoa data with summary & boundaries - single dataset to load for both neighbourhood & msoa level
neighbourhood_indicators <- readRDS("neighbourhood_indicators.RDS")

# single dataset filtered for neighbourhood only
neighbourhood_data <- neighbourhood_indicators %>%
  st_drop_geometry() %>%
  group_by(IndicatorId, Sex, Age, neighbourhood) %>%
  slice(1) %>% # first row only per neighbourhood, so will give neighbourood values as same for every msoa in the neighbourhood
  ungroup() %>%
  #select(-c(msoa11cd, ParentCode:AreaType, Value:hoc_msoa_name)) %>%
  relocate(nbourhood_min, .before = nbourhood_max) %>%
  mutate(across(.cols = nbourhood_pct:england_q3, 
                .fns = ~round(.x, 1)
  ))

# single dataset filtered for msoa only
msoa_data <- neighbourhood_indicators %>%
  select(neighbourhood, AreaName, hoc_msoa_name, msoa11cd:IndicatorName, Value)


#######################################################################

ui <-  dashboardPage(skin = "yellow",  
                     
                     # Application title
                     dashboardHeader(title = "Bolton Neighbourhood Information",
                                     titleWidth = 350),
                     
                     # Sidebar 
                     dashboardSidebar(
                       # tab selector menu
                       sidebarMenu(
                         menuItem("Using this tool", tabName = "using", icon = icon("map-signs")),
                         menuItem("Neighbourhood map", tabName = "neigh_map", icon = icon("map-location-dot")),
                         menuItem("All areas map", tabName = "allareas_map", icon = icon("globe")),
                         menuItem("Chart", tabName = "chart_tab", icon = icon("chart-line")),
                         menuItem("Differences", tabName = "z_scores", icon = icon("arrows-alt-h")),
                         menuItem("Table", tabName = "table_tab", icon = icon("table")),
                         menuItem("About neighbourhoods", tabName = "about_neighbourhoods", icon = icon("map-marked-alt")),
                         menuItem("About the data", tabName = "about", icon = icon("info"))
                       ),
                       # neighbourhood selector
                       selectInput(inputId = "select_neighbourhood",
                                   label = "Select neighbourhood:",
                                   choices = neighbourhood_names
                       ),
                       
                       # domain selector (loads of indicators)
                       selectInput(inputId = "select_domain",
                                   label = "Select domain:",
                                   choices = unique(neighbourhood_data$DomainName)
                       ),
                       # indicator selector
                       uiOutput("indicators_in_domain")
                       
                     ),
                     
                     dashboardBody(
                       tabItems(
                         # using this
                         tabItem(tabName = "using",
                                 h2("What is this?"),
                                 p("This tool was developed by Bolton Council's public health intelligence team.",
                                 "It gives some information about the populations in Bolton's integrated care neighbourhoods & factors affecting their current & future health.",
                                 "It has been developed by combining data already available at small area level."),
                                 p("The Office for Health Improvement and Disparities (OHID)'s Local Health profile forms the basis as it is a collection of quality assured information covering: population and demographic factors; wider determinants of health; health outcomes.",
                                   "The tool also contains some indicators from the 2021 census that are particularly relevant to health.",
                                   "It also contains all domains from the English Indices of Deprivation, as there are many differences in health by deprivation."),
                                 br(),
                                 h2("What should I do with this?"),
                                 p("Consider questions such as:"),
                                 tags$li("How is your neighbourhood different from or similar to Bolton or England as a whole? Small differences may not be important but big differences may be."),
                                 tags$li("Are there big differences within your neighbourhood or is it quite similar overall?"),
                                 br(),
                                 p("Consider this as one source of information in your decision making. Data, evidence & intelligence can help you decide, or shape your thinking but does not hold all the answers!"),
                                 tags$li("What decisions do you need to make? What information do you need to determine direction of travel & choose between options?"),
                                 tags$li("The information you need will most likely involve using data, evidence & intelligence alongside your professional judgement of what is feasible within your constraints & most likely to be successful."),
                                 tags$li("Often the perfect piece of information won't be available, do you have enough? Sometimes you won't feel you have enough but will still have to make a decision."),
                                 br(),
                                 p("More data, evidence & intelligence always raises more questions, that's good! Use it to develop & test your questions & assumptions which hopefully will help you make better decisions."),
                                 br(),
                                 h2("How do I use this?"),
                                 p("This tool presents information on a range of indicators in a range of ways to help you explore different aspects of a neighbourhood, such as how a neighbourhood compares to Bolton as a whole & how much variation there is within a neighbourhood."),
                                 p("Select a neighbourhood, domain & indicator from the dropdowns on the left. Use the different ways the data is displayed to explore & inform your decision making."),
                                 tags$li("Table - shows all information in a traditional spreadsheet format."),
                                 tags$li("Chart - shows how the neighbourhood values compare to Bolton and England as a whole."),
                                 tags$li("Neighbourhood map - shows variation within the neighbourhood."),
                                 tags$li("All areas map - shows variation across the whole of Bolton"),
                                 tags$li("Differences - lets you see where neighbourhoods are most different from England as a whole."),
                                 tags$li("About neighbourhoods - explains what neighbourhoods are and where all the boundaries fall"),
                                 tags$li("About the data - gives data sources"),
                                 p("")
                         ),
                         
                         # neighbourhood map
                         tabItem(tabName = "neigh_map",
                                 #h2(textOutput("selected_neighbourhood")), # makes all reactive stuff not show
                                 h3("Neighbourhood Map"),
                                 tags$style(type = "text/css", "#indicator_map {height: calc(100vh - 100px) !important;}"),
                                 leafletOutput("indicator_map"),
                                 br(),
                                 h3("How to interpret this map"),
                                 p("This map shows variation within the neighbourhood.",
                                 "The thick green boundary shows the neighbourhood boundary.",
                                 "The coloured areas show the smaller areas which are combined in this tool to give neighbourhood level information."),
                                 tags$li("For some indicators, the boundaries don't quite match, you can see where they are different."),
                                 tags$li("A darker colour indicates an area is high for Bolton on the chosen indicator, a lighter colour indicates an area is low for Bolton on the chosen indicator."),
                                 tags$li("A neighbourhood may be made up of all darker areas, or all lighter areas or a mix. If the neighbourhood is more similar overall on an indicator, the colours will be more simliar. Check out the chart for a clearer view of how the neighbourhood compares to Bolton as a whole."),
                                 tags$li("Very different colours indicate where there may be pockets of difference."),
                                 tags$li("The hover gives the actual values for each area. Check the values - Bolton may be all quite similar on an indicator so a big change in colour may not reflect a difference that is big enough to be useful in the real world."),
                                 tags$li("Indicators marked 'standardised' are adjusted so it's as if all areas had the same population age structure.",
                                         "This lets you compare indicators that are strongly associated with age, and let you see if areas are higher or lower than expected.")
                              ),
                         
                         # all areas map
                         tabItem(tabName = "allareas_map",
                                 h3("All areas map"),
                                 # adjusts height of map to be full window minus some for header + box at bottom
                                 tags$style(type = "text/css", "#allareas_map {height: calc(100vh - 100px) !important;}"),
                                 leafletOutput("allareas_map"),
                                 br(),
                                 h3("How to interpret this map"),
                                 p("This map shows variation across the whole of Bolton."),
                                 tags$li("A darker colour indicates an area is high for Bolton on the chosen indicator, a lighter colour indicates an area is low for Bolton on the chosen indicator."),
                                 tags$li("The hover gives the actual values for each area. Check the values - Bolton may be all quite similar on an indicator so a big change in colour may not reflect a difference that is big enough to be useful in the real world."),
                                 tags$li("Indicators marked 'standardised' are adjusted so it's as if all areas had the same population age structure.",
                                 "This lets you compare indicators that are strongly associated with age, and let you see if areas are higher or lower than expected.")
                                 ),
                         
                         # chart
                         tabItem(tabName = "chart_tab",
                                 h3("Visualising the difference between neighbourhoods, Bolton & England"),
                                 br(),
                                 plotlyOutput("boxplot"),
                                 br(),
                                 DT::DTOutput("boxplot_table"),
                                 br(),
                                 h3("How to interpret this chart"),
                                 p("This chart shows how the neighbourhood values compare to Bolton and England as a whole. It shows a mid point for each area, but also how big the variation is within the areas.",
                                   "Each area is built up of smaller areas, either MSOAs (a medium sized administrative geography) or LSOAs (a small administrative geography) depending on the indicator."),
                                 tags$li("The average (median) value for the smaller areas making up the larger area is the line inside the bar. For some (but not all) indicators, there is a calculated value for the larger area as a whole, this is given in the table below the chart."),
                                 tags$li("The whiskers show the values for the lowest and highest smaller areas within the larger area."),
                                 tags$li("The bar shows the range that the middle half of smaller areas within the larger area fall into.",
                                         "The left edge of the bar shows Quartile 1/ the 25th percentile, this is the value that a quarter (25%) of values are lower than. The right edge of the bar shows Quartile 3/ the 75th perceentile, this is the value that three-quarters (75%) of values are lower than. The middle half therefore fall between these values."),
                                 tags$li("The whiskers and bar may be symmetrical or not. If most areas are high on an indicator, but some are much lower (or vice versa), the chart may not look at all symmetrical."),
                                 tags$li("Indicators marked 'standardised' are adjusted so it's as if all areas had the same population age structure.",
                                         "This lets you compare indicators that are strongly associated with age, and let you see if areas are higher or lower than expected."),
                                 br(),
                                 h3("How to use this chart"),
                                 tags$li("A neighbourhood bar & whiskers much further left than for Bolton shows the neighbourhood is at the low end for Bolton on this indicator, while a neighbourhood bar & whiskers much further right than for Bolton shows the neighbourhood is at the high end for Bolton."),
                                 tags$li("But look at the numbers at the bottom of the chart - it may be that the whole of Bolton is quite similar on an indicator & differences may not be big enough to be useful in the real world."),
                                 tags$li("Are the neighbourhoood bar and whiskers narrow? This indicates the whole neighbourhood is quite similar on this indicator."),
                                 tags$li("A wide bar and whiskers suggest a neighbourhood with a lot of variation on this indicator. Check out the map for this indicator to find out more about where the variation is."),
                                 tags$li("Compare Bolton and England in a similar way - Bolton may be much higher or lower than England, so an issue may still be important for a neighbourhood even if it is similar to the Bolton picture.")
                         ),

                         # difference from England
                         tabItem(tabName = "z_scores",
                                 h3("How different is the selected neighbourhood from England?"),
                                 DT::DTOutput("neighbourhood_z"),
                                 h3("How to interpret this table"),
                                 p("This table shows how the smaller areas making up the selected neighbourhood compare with other smaller areas all across England."),
                                 tags$li("Indicators are adjusted using 'z scores' so it puts each indicator all on the same scale, to help see where the neighbourhood is most different."),
                                 tags$li("By default, indicators are sorted by standardised average score, so the most different from England are at the top. They might be different because they are much higher or much lower."),
                                 tags$li("If a z score is zero, then it is exactly the same as the England average, so the further from zero the more different it is. If something is distributed 'normally' (many measures occur like this, with similar numbers low & high, with most values in the middle) 95% will fall between +1.96 & -1.96"),
                                 tags$li("Range per neighbourhood is also given, you can sort by these columns to identify indicators where there is a great deal of or very little variation within the neighbourhood."),
                                 tags$li("You can use this table together with the map and chart to see which indicators to focus on and find out more about from elsewhere."),
                                 tags$li("Indicators marked 'standardised' are adjusted so it's as if all areas had the same population age structure.",
                                         "This lets you compare indicators that are strongly associated with age, and let you see if areas are higher or lower than expected.")
                         ),
                         
                         # table tab
                         tabItem(tabName = "table_tab",
                                 h2(textOutput("selected_neighbourhood")),
                                 br(),
                                 downloadButton("bttn_data", "Download all neighbourhood data (csv)"),
                                 br(),
                                 h3(textOutput("selected_domain")),
                                 DT::DTOutput("table1")
                         ),
                         
                         # about neighbourhoods
                         tabItem(tabName = "about_neighbourhoods",
                                 h2("Bolton's neighbourhoods"),
                                 leafletOutput("neighbourhoods_map"),
                                 h3("What are neighbourhoods?"),
                                 p("Neighbourhoods are a local geography, created for integrated health and social care."),
                                 p("The map above shows Bolton's neighbourhoods."),
                                 p("Bolton's neighbourhoods are made up of Lower Super Output Areas (LSOAs), but not all data in this tool is only available at MSOA, which is bigger so the boundaries don't quite match."),
                                 h3("MSOA neighbourhood lookup"),
                                 DT::DTOutput("msoa_neighbourhood_lookup")
                         ),
                         
                         # data sources
                         tabItem(tabName = "about",
                                 h2("Data sources"),
                                 p("Data in this tool is sourced from a variety of places.",
                                   "This tab gives more information about where the indicators come from.", 
                                   "This will help you if you need to to find out more about precisely how the indicators are gathered, or have queries about any further data issues."),
                                 br(),
                                 
                                 h2("Local Health"),
                                 tags$li("This data is sourced from the ",
                                 a("OHID Fingertips Local Health dashboard", href = "https://fingertips.phe.org.uk/profile/local-health/data#page/0/gid/1938133180/pat/402/par/E08000001/ati/3/iid/93744/age/28/sex/4/cid/4/tbm/1", 
                                   target = "_blank"), "."),
                                 tags$li("Local Health only goes down to MSOA (Middle Super Output Area, a medium sized administrative geography) whereas Bolton's Neighbourhoods are built of LSOAs (Lower Super Output Areas, a small administrative geography) which have different boundaries.",
                                 "MSOAs have been used to approximate neighbourhoods. MSOAs are included in every neighbourhood in which they at least partly fall. The difference in boundaries is visible on the map."), 
                                 tags$li(glue::glue("Data last refreshed: {data_refresh_date}")),
                                 br(),
                                 
                                 h2("Deprivation"),
                                 tags$li("Deprivation data is from the ", a("English Indices of Deprivation, 2019", 
                                                                      href = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019", 
                                                                      target = "_blank"), "."), 
                                 tags$li("The main geography for deprivation is LSOA (Lower Super Output Area, a small administrative geography) so the boundaries exactly match with neighbourhoods which are built of LSOAs."),
                                 br(),
                                 
                                 h2("Census 2021"),
                                 tags$li("Data for this tool obtained from ", a("Nomis", 
                                                            href = "https://www.nomisweb.co.uk/sources/census_2021_bulk", 
                                                            target = "_blank"), " and for the standardised data ",
                                         a("ONS create a custom dataset",
                                           href = "https://www.ons.gov.uk/datasets/create",
                                           target = "_blank"), "."),
                                 tags$li("There are many other useful census data resources, see ",
                                         a("Bolton JSNA", 
                                           href = "https://www.boltonjsna.org.uk/newandnotable#census", 
                                           target = "_blank"), "."),
                                 tags$li("Census data is available for a large range of geographies, LSOA (Lower Super Output Area, a small administrative geography) has been used here so the boundaries exactly match with neighbourhoods which are built of LSOAs."),
                                 h3("Age standardising census data"),
                                 tags$li("Unless otherwise stated, Census 2021 figrues are not age standardised."), 
                                 tags$li("ONS usually presents indicators related to health as age standardised, as in an older population, you would expect poorer health, more disability and more unpaid care.", 
                                         "Where the data is not standardised, differences between areas may be due to different population makeups, so please take care when interpreting."),
                                 tags$li("Where age standardising has been done, 6 age groups were used: Aged 15 years and under; Aged 16 to 24 years; Aged 25 to 34 years; Aged 35 to 49 years; ",    
                                          "Aged 50 to 64 years; Aged 65 years and over."),
                                 tags$li("Each age group's rate on that indicator was multiplied by the proportion of the England population that this age group represents, following the methodology given by ",
                                         a("US National Cancer Institute",
                                           href = "https://seer.cancer.gov/seerstat/tutorials/aarates/step1.html",
                                           target = "_blank"),
                                         ". This has the effect of adjusting as if the population of each area was the exact same age makeup as that of the England population as a whole."),
                                 tags$li("Standardised values are useful to compare areas to see if an area has higher or lower rates than expected, regardless if the areas are overall younger or older than each other."),
                                 tags$li("Sometimes you will want to use the raw values, for example if you want to know how many people to provide a service to."),
                                 tags$li("Not all of the indicators were available in the custom dataset tool so could not be age standardised."),
                                 
                                 br(),

                                 h2("Code"),
                                 p("The code for this app is on my github."),
                                 a("Github", href = "https://github.com/shanwilkinson2/neighbourhood_info", 
                                   target = "_blank")
                         )
                       )
                     )
)


##########################################################################################################

server <- function(input, output) {
  
  # name of selected neighbourhood
  output$selected_neighbourhood <- renderText({input$select_neighbourhood})
  
  # name of selected domain
  output$selected_domain <- renderText({input$select_domain})
  
  # name of selected indicator
  output$selected_indicator <- renderText({input$select_indicator})
  
  
  # indicators in selected domain for dropdown
  output$indicators_in_domain <- renderUI({
    selectInput(inputId = "select_indicator",
                label = "Select indicator to map:",
                choices = unique(neighbourhood_data[neighbourhood_data$DomainName == input$select_domain,"IndicatorName"])
    )
  })
  
  # data for table for selected neighbourhood for selected domain
  table_data <- reactive({
    neighbourhood_data %>%
      # filtered_data() %>%
      filter(neighbourhood == input$select_neighbourhood & 
               DomainName == input$select_domain) %>%
      # mutate(across(.cols = nbourhood_pct:bolton_max, 
      #               .fns = ~round(.x, 1)
      #                 )) %>%
      select(IndicatorName, nbourhood_pct:bolton_value) %>%
      rename(`Indicator name` = IndicatorName, `N'b'hood calculated value` = nbourhood_pct, 
             `N'b'hood average` = nbourhood_median, `N'b'hood min` = nbourhood_min, `N'b'hood max` = nbourhood_max, 
             `Bolton value` = bolton_value, `Bolton min` = bolton_min, `Bolton max` = bolton_max)
  })
  
  
  # create table
  output$table1 <- DT::renderDT({
    table_data()
  }, 
  filter = "top", 
  rownames = FALSE,
  extensions = "Buttons", 
  options = list(dom = "Bprti", # order of buttons/ filter etc
                 buttons = c("copy", "csv", "excel"))
  
  )
  
  # reactive dataset for map
  # includes all neighbourhoods here for indicator palatte
  map_data <- reactive({
    msoa_data %>%
      filter(IndicatorName == input$select_indicator) 
  })
  
  # reactive boundary of selected neighbourhood
  neighbourhood_boundary <- reactive({
    neighbourhood_boundaries %>%
      filter(neighbourhood_name == input$select_neighbourhood) 
  })
  
  
  # map palette
  msoa_pal <- reactive({
    colorNumeric(
      palette = "YlOrRd", 
      domain = map_data()$Value, 
      na.color = "white")
  })
  
  # neighbourhood map
  output$indicator_map <- renderLeaflet({
    
    mylabels <- as.list(glue::glue("{map_data()$AreaName}, {map_data()$hoc_msoa_name}<br>
                                     Indicator value: {round(map_data()$Value)}"))
    
    map_data() %>%
      filter(neighbourhood == input$select_neighbourhood) %>%
      leaflet() %>%
      addResetMapButton() %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addPolygons( #data = map_data(), 
        weight = 0.75, color = "black", 
        fillColor = ~msoa_pal()(Value), fillOpacity = 0.5, 
        highlight = highlightOptions(weight = 4, color = "grey"),
        #label = lapply(mylabels, HTML),
        label = ~paste0(AreaName, ", ", hoc_msoa_name, ". Indicator value: ", round(Value)), 
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addPolylines(data = neighbourhood_boundary(), weight = 4, color = "aquamarine") %>%
      addControl(glue::glue("<b>{input$select_indicator}</b><br>Neighbourhood: {input$select_neighbourhood}{ifelse(input$select_domain == 'Deprivation', ', Low number = more deprived', '')}"), position = "topright") %>%
      addLegend(
        "bottomright",
        pal = msoa_pal(),
        values = ~Value,
        labFormat = labelFormat(digits = 0),
        title = "Area values",
        opacity = 1
      )
  })
  
  # allareas map
  output$allareas_map <- renderLeaflet({
    
    mylabels <- as.list(glue::glue("{map_data()$AreaName}, {map_data()$hoc_msoa_name}<br>
                                     Indicator value: {round(map_data()$Value)}"))
    
    map_data() %>%
      leaflet() %>%
      addResetMapButton() %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addPolygons( 
        weight = 0.75, color = "black", 
        fillColor = ~msoa_pal()(Value), fillOpacity = 0.5, 
        highlight = highlightOptions(weight = 4, color = "grey"),
        label = ~paste0(AreaName, ", ", hoc_msoa_name, ". Indicator value: ", round(Value)), 
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addPolylines(data = neighbourhood_boundaries, weight = 4, color = "black", fillOpacity = 0,
                  label = ~paste(neighbourhood_name, "neighbourhood"), group = "Neighbourhoods")  %>%
      addControl(glue::glue("<b>{input$select_indicator}</b>{ifelse(input$select_domain == 'Deprivation', '<br>Low number = more deprived', '')}"), position = "topright") %>%
      addLayersControl(overlayGroups = c("Neighbourhoods")) %>%
      addLegend(
        "bottomright",
        pal = msoa_pal(),
        values = ~Value,
        labFormat = labelFormat(digits = 0),
        title = "Area values",
        opacity = 1
      )
  })
  
  # reactive dataset for boxplot
  # includes all neighbourhoods here for indicator palatte
  boxplot_data <- reactive({
    neighbourhood_data %>%
      filter(IndicatorName == input$select_indicator &
               neighbourhood == input$select_neighbourhood) 
  })
  
  # boxplot to show how neighbourhood value & range compares with Bolton
  output$boxplot <- renderPlotly({
    boxplot_data() %>%
      plot_ly() %>%
      add_trace(type = "box",
                y = list(
                  "England",
                  "Bolton", 
                  boxplot_data()$neighbourhood
                ),
                q1 = list(boxplot_data()$england_q1,
                          boxplot_data()$bolton_q1, 
                          boxplot_data()$nbourhood_q1
                ),
                lowerfence = list(
                  boxplot_data()$england_min,
                  boxplot_data()$bolton_min, 
                  boxplot_data()$nbourhood_min
                ),
                # calculated value if it's available otherwise median
                median = list(
                  boxplot_data()$england_median,
                  boxplot_data()$bolton_median,
                  boxplot_data()$nbourhood_median
                ),
                q3 = list(boxplot_data()$england_q3,
                          boxplot_data()$bolton_q3, 
                          boxplot_data()$nbourhood_q3
                ),
                upperfence = list(
                  boxplot_data()$england_max,
                  boxplot_data()$bolton_max, 
                  boxplot_data()$nbourhood_max
                ),
                notchspan = list(0.3, 0.3),
                #hovertemplate = "some text {boxplot_data()$bolton_max}",
                #hovertext = ~chart_data$nbourhood_count,
                color = "orange"
      ) %>%
      layout(title = glue::glue("<b>{boxplot_data()$IndicatorName}{ifelse(input$select_domain == 'Deprivation', '<br>Low number = more deprived', '')}</b>"),
             xaxis = list(hoverformat = ".1f",
                          title = "Area values"))
  })
  
  # create table
  output$boxplot_table <- DT::renderDT({
    boxplot_data() %$%
      data.frame(Area = c(neighbourhood, "Bolton", "England")
                 ,
                 Min = c(nbourhood_min, 
                         bolton_min, 
                         england_min)
                 ,
                 `Quartile1` = c(nbourhood_q1, 
                                 bolton_q1, 
                                 england_q1)
                 ,
                 Median = c(nbourhood_median, 
                            bolton_median,
                            england_median)
                 ,
                 Value = c(nbourhood_pct,
                           bolton_value, 
                           england_value)
                 ,
                 `Quartile3` = c(nbourhood_q3, 
                                 bolton_q3, 
                                 england_q3)
                 ,
                 Max = c(nbourhood_max, 
                         bolton_max, 
                         england_max) 
                 
      )
  }, rownames = FALSE)
  
  # create table
  output$neighbourhood_z <- DT::renderDT({
    neighbourhood_data %>%
      filter(neighbourhood == input$select_neighbourhood) %>%
      select(Indicator = IndicatorName, Domain = DomainName,  
             `N'b'hood standardised average` = z_nbourhoood_median_abs, `N'b'hood direction` = z_nbourhood_median_abs_direction,
             `N'b'hood average` = nbourhood_median, `England average` = england_median,
             `N'b'hood standardised mid half range` = z_nbourhood_iqr_abs, `N'b'hood standardised range` =  z_nbourhood_range_abs
      ) %>%
      arrange(desc(`N'b'hood standardised average`)) 
  }, 
  filter = "top", 
  rownames = FALSE,
  extensions = "Buttons", 
  options = list(dom = "Bprti", # order of buttons/ filter etc
                 buttons = c("copy", "csv", "excel"))
  
  )
  
  # create table
  output$msoa_neighbourhood_lookup <- DT::renderDT({
    msoa_neighbourhood_multiple %>%
      rename(`MSOA code` = msoa_code, 
             `MSOA name` = msoa_name,
             `MSOA House of Commons Library name` = hoc_msoa_name,
             `Neighbourhood number` = x6_areas_number,
             `Neighbourhood name` = neighbourhood, 
             `Number of lsoas` = num_lsoas
      )
  }, 
  filter = "top", 
  rownames = FALSE,
  extensions = "Buttons", 
  options = list(dom = "Bprti", # order of buttons/ filter etc
                 buttons = c("copy", "csv", "excel"))
  
  )
  
  # create map of all neighbourhoods
  output$neighbourhoods_map <- renderLeaflet({
    leaflet(neighbourhood_boundaries) %>%
      addResetMapButton() %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addPolygons(weight = 4, color = "red", fillOpacity = 0,
                  label = ~paste(neighbourhood_name, "neighbourhood")) 
  })
  
  # generate data for download button
  output$bttn_data <- downloadHandler(filename = "all neighbourhoods data.csv",
                                      # create file for downloading
                                      content = function(file){
                                        write.csv(neighbourhood_data %>%
                                                    st_drop_geometry()
                                                  , file)
                                      })
  
  
}

##########################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
