# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load packages
    library(shiny)
    library(shinydashboard)
    library(dplyr)
    library(sf)
    library(leaflet)
    library(leaflet.extras)
    library(plotly)
    library(magrittr)

# load static datasets
    data_refresh_date <- "04/11/2021"

    neighbourhood_names <- c("Breightmet/Little Lever", "Central/Great Lever", "Chorley Roads", 
                             "Crompton/Halliwell", "Farnworth/Kearsley", "Horwich",
                             "Rumworth", "Turton", "Westhoughton")
    
    # neighbourhood/ msoa lookup table
    msoa_neighbourhood_multiple <- readRDS("msoa_neighbourhood_multiple.RDS")
    
    neighbourhood_boundaries <- readRDS("neighbourhood boundaries.RDS")
    
    # msoa data with summary & boundaries - single dataset to load for both neighbourhood & msoa level
    neighbourhood_indicators <- readRDS("neighbourhood_indicators.RDS")
    
    # single dataset filtered for neighbourhood only
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
                    menuItem("Table", tabName = "table_tab", icon = icon("table")),
                    menuItem("Chart", tabName = "chart_tab", icon = icon("chart-line")),
                    menuItem("Map", tabName = "map", icon = icon("globe")),
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
                    p("This tool was developed by Bolton Council's public health intelligence team. It gives some information about the populations in Bolton's integrated care neighbourhoods & factors affecting their current & future health."),
                    p("It has been developed by combining data already available at small area level."),
                    p("Public Health England's Local Health profile forms the basis as it is a collection of quality assured information covering: population & demographic factors; wider determinants of health; health outcomes."),
                    br(),
                    h2("What should I do with this?"),
                    p("Consider questions such as:"),
                    p("How is your neighbourhood different from or similar to Bolton as a whole? Small differences may not be important but big differences may be."),
                    p("Are there big differences within your neighbourhood or is it quite similar overall?"),
                    br(),
                    p("Consider this as one source of information in your decision making. Data, evidence & intelligence can help you decide, or shape your thinking but does not hold all the answers!"),
                    p("What decisions do you need to make? What information do you need to determine direction of travel & choose between options?"),
                    p("The information you need will most likely involve using data, evidence & intelligence alongside your professional judgement of what is feasible within your constraints & most likely to be successful."),
                    p("Often the perfect piece of information won't be available, do you have enough? Sometimes you won't feel you have enough but will still have to make a decision."),
                    p("More data, evidence & intelligence always raises more questions, that's good! Use it to develop & test your questions & assumptions which hopefully will help you make better decisions."),
                    br(),
                    h2("How do I use this?"),
                    p("This tool presents information on a range of indicators in a range of ways to help you explore different aspects of a neighbourhood, such as how a neighbourhood compares to Bolton as a whole & how much variation there is within a neighbourhood."),
                    p("Select a neighbourhood, domain & indicator (indicator not needed for the table) from the dropdowns on the left."),
                    p("The table shows all information in a traditional spreadsheet format."),
                    p("The chart shows how the neighbourhood values compare to Bolton as a whole."),
                    p("The map shows variation within the neighbourhood."),
                    p("Use the different formats to explore the data & inform your decision making.")
            ),
                    
            # tab1
            tabItem(tabName = "table_tab",
                    h2(textOutput("selected_neighbourhood")),
                    h3(textOutput("selected_domain")),
                    DT::DTOutput("table1")
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
                    p("This chart shows how the neighbourhood values compare to Bolton and England as a whole."),
                    p("The overall value is the line inside the bar."),
                    p("The whiskers show the values for the lowest and highest MSOAs within the area."),
                    p("The bar shows the range that the middle half of MSOAs (the areas that this data is built up from) within this area fall within."),
                    p("The left edge of the bar shows Quartile 1/ the 25th percentile, this is the value that a quarter (25%) of values are lower than. The right edge of the bar shows Quartile 3/ the 75th perceentile, this is the value that three-quarters (75%) of values are lower than. The middle half therefore fall between these values."),
                    p("The whiskers and bar may be symmetrical or not. If most areas are high on an indicator, but some are much lower (or vice versa), the chart may not look at all symmetrical."),
                    p("A neighbourhood bar & whiskers much further left than for Bolton shows the neighbourhood is at the low end for Bolton on this indicator, while a neighbourhood bar & whiskers much further right than the Bolton notch shows the neighbourhood is at the high end for Bolton."),
                    p("But look at the numbers at the bottom of the chart - it may be that the whole of Bolton is quite similar on an indicator & differences may not be big enough to be useful in the real world."),
                    p("Is the neighbourhoood bar narrow? This indicates the whole neighbourhood is quite similar on this indicator."),
                    p("A wide bar suggests a neighbourhood with a lot of variation on this indicator. Check out the map for this indicator to find out more about where the variation is."),
                    p("Compare Bolton & England in a similar way - Bolton may be much higher or lower than England, so an issue may still be important for a neighbourhood even if it is similar to the Bolton picture."),
                    ),
            
            # map
            tabItem(tabName = "map",
                    #h2(textOutput("selected_neighbourhood")), # makes all reactive stuff not show
                    h3(textOutput("selected_indicator")),
                    leafletOutput("indicator_map"),
                    br(),
                    h3("How to interpret this map"),
                    p("This map shows variation within the neighbourhood."),
                    p("The orange boundary shows the neighbourhood boundary. The coloured areas show the smaller areas which are combined in this tool to give neighbourhood level information."),
                    p("The boundaries don't quite match, you can see where they are different."),
                    p("A darker colour indicates an area is high for Bolton on the chosen indicator, a lighter colour indicatos an area is low for Bolton on the chosen indicator."),
                    p("A neighbourhood may be made up of all darker areas, or all lighter areas or a mix. If the neighbourhood is more similar overall on an indicator, the colours will be more simliar. Check out the chart for a clearer view of how the neighbourhood compares to Bolton as a whole."),
                    p("Very different colours indicate where there may be pockets of difference."),
                    p("The hover gives the actual values for each area. Check the values - Bolton may be all quite similar on an indicator so a big change in colour may not reflect a difference that is big enough to be useful in the real world.")
            ),
            
            # data sources
            tabItem(tabName = "about",
                    h2("Data source"),
                    p("Data is sourced from PHE fingertips local health dashboard. This gives more information about where the indicators come from."),
                    a("PHE Local Health profiles", href = "https://fingertips.phe.org.uk/profile/local-health/data#page/0/gid/1938133180/pat/402/par/E08000001/ati/3/iid/93744/age/28/sex/4/cid/4/tbm/1", 
                      target = "_blank"),
                    p("Local Health only goes down to MSOA (Middle Super Output Area) whereas Bolton's Neighbourhoods are built of LSOAs (Lower Super Output Areas) which have different boundaries."),
                    p("MSOAs have been used to approximate neighbourhoods. MSOAs are included in every neighbourhood in which they at least partly fall. The difference in boundaries is visible on the map."), 
                    p(glue::glue("Data last refreshed: {data_refresh_date}")),
                    br(),
                    h2("Interim issues while this tool is under development"),
                    p("Columns named as 'neighbourhood calculated value' relate to those where the data is provided as a numerator & denominator, which is then combined to create a % for the neighbourood. Some of these indicators are not percentages so look dodgy."), 
                    p("Columns named as 'neighbourhood average, min or max' give the median, minimum & maximum of the values for MSOAs falling within that neighbourhood."),
                    p("Columns marked as 'Bolton min or max' give the minimum and maximum values out of all Bolton MSOAs."),
                    br(),
                    h2("Code"),
                    p("The code for this app is on my github."),
                    a("Github", href = "https://github.com/shanwilkinson2/neighbourhood_info", 
                      target = "_blank"),
                    br(),
                    h2("MSOA neighbourhood lookup"),
                    DT::DTOutput("msoa_neighbourhood_lookup")
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
    
    # map itself
    output$indicator_map <- renderLeaflet({
      
      mylabels <- as.list(glue::glue("{map_data()$AreaName}, {map_data()$hoc_msoa_name}<br>
                                     Indicator value: {round(map_data()$Value)}"))
      
        map_data() %>%
        filter(neighbourhood == input$select_neighbourhood) %>%
        leaflet() %>%
        addResetMapButton() %>%
        addProviderTiles("Stamen.TonerLite") %>%
        addPolylines(data = neighbourhood_boundary(), weight = 4, color = "red") %>%
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
                    boxplot_data()$england_value,
                    boxplot_data()$bolton_value,
                    ifelse(is.na(boxplot_data()$nbourhood_pct), 
                           boxplot_data()$nbourhood_median,
                           boxplot_data()$nbourhood_pct)
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
        layout(title = boxplot_data()$IndicatorName,
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
                 Value = c(ifelse(!is.na(nbourhood_pct), 
                                  nbourhood_pct, 
                                  nbourhood_median),
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
    output$msoa_neighbourhood_lookup <- DT::renderDT({
      msoa_neighbourhood_multiple %>%
        rename(`MSOA code` = msoa_code, 
               `MSOA name` = msoa_name,
               `MSOA House of Commons Library name` = hoc_msoa_name,
               Neighbourhood = neighbourhood, 
               `% of 2019 population in neighbourhood` = pct_of_lsoa)
    }, 
    filter = "top", 
    rownames = FALSE,
    extensions = "Buttons", 
    options = list(dom = "Bprti", # order of buttons/ filter etc
                   buttons = c("copy", "csv", "excel"))
    
    )
    
}

##########################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
