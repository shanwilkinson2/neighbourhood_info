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

# load static datasets
    data_refresh_date <- "23/06/2021"
    # neighbourhood_data <- readRDS("dashboard_indicators.RDS")
    neighbourhood_names <- c("Breightmet/Little Lever", "Central/Great Lever", "Chorley Roads", 
                             "Crompton/Halliwell", "Farnworth/Kearsley", "Horwich",
                             "Rumworth", "Turton", "Westhoughton")
    neighbourhood_boundaries <- readRDS("neighbourhood boundaries.RDS")
    local_health_data_msoa <- readRDS("local health data with boundaries.RDS")
    
    # msoa data with summary & boundaries - hopefully single dataset to load
    neighbourhood_indicators <- readRDS("neighbourhood_indicators.RDS")
    neighbourhood_data <- neighbourhood_indicators %>%
      st_drop_geometry() %>%
      group_by(IndicatorID, Sex, Age, neighbourhood) %>%
      slice(1) %>% # first row only per neighbourhood, so will give neighbourood values as same for every msoa in the neighbourhood
      ungroup() %>%
      select(-c(msoa11cd, ParentCode:AreaType, Value:hoc_msoa_name))
    
# Define UI for application that draws a histogram
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
                    p("It has been developed by combining data already avalable at small area level."),
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
                    p("More data, evidence & intelligence always raises more questions, that's good! Use it to develop & test your questions & assumptions which hopepfully will help you make better decisions.")
            ),
                    
            # tab1
            tabItem(tabName = "table_tab",
                    h2(textOutput("selected_neighbourhood")),
                    h3(textOutput("selected_domain")),
                    DT::DTOutput("table1")
            ),
            
            # map
            tabItem(tabName = "map",
                    #h2(textOutput("selected_neighbourhood")), # makes all reactive stuff not show
                    h3(textOutput("selected_indicator")),
                    leafletOutput("indicator_map")
            ),
            
            # data sources
            tabItem(tabName = "about",
                    h2("Data source"),
                    p("Data is sourced from PHE fingertips local health dashboard. This gives more information about where the indicators come from."),
                    a("PHE Local Health profiles", href = "https://fingertips.phe.org.uk/profile/local-health", 
                      target = "_blank"),
                    p("Local Health only goes down to MSOA (Middle Super Output Area) whereas Bolton's Neighbourhoods are built of LSOAs (Lower Super Output Areas) which have different boundaries."),
                    p("MSOAs have been used to approximate neighbourhoods, based on the Neighbourhood in which most population falls. The slight difference in boundaries is visible on the map."), 
                    p(glue::glue("Data last refreshed: {data_refresh_date}")),
                    br(),
                    h2("Interim issues while this tool is under development"),
                    p("Columns named as 'neighbourhood calculated value' relate to those where the data is provided as a numerator & denominator, which is then combined to create a % for the neighbourood. Some of these indicators are not percentages so look dodgy."), 
                    p("Columns named as 'neighbourhood average, min or max' give the median, minimum & maximum of the values for MSOAs falling within that neighbourhood."),
                    p("Incicators wihtout a 'neighbourhood calculated value' are not currently available to map."),
                    br(),
                    h2("Code"),
                    p("The code for this app is on my github."),
                    a("Github", href = "https://github.com/shanwilkinson2/neighbourhood_info", 
                      target = "_blank"),
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
            mutate(across(.cols = nbourhood_pct:bolton_value, 
                          .fns = ~round(.x, 1)
                            )) %>%
            select(IndicatorName, nbourhood_pct:bolton_value) %>%
            rename(`Indicator name` = IndicatorName, `Nbourhood calculated value` = nbourhood_pct, 
                   `Nbourhood average` = nbourhood_median, `Nbourhood min` = nbourhood_min, `Nbourhood max` = nbourhood_max, 
                   `Bolton value` = bolton_value)
    })


    # create table
    output$table1 <- DT::renderDT({
        table_data()
   }, filter = "top", rownames = FALSE)
    
    # reactive dataset for map
    # includes all neighbourhoods here for indicator palatte
    # why is neighbourhood called neighbourhood.y?
    map_data <- reactive({
        local_health_data_msoa %>%
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
    
    output$indicator_map <- renderLeaflet({
        map_data() %>%
        filter(neighbourhood.y == input$select_neighbourhood) %>%
        leaflet() %>%
        addResetMapButton() %>%
        addProviderTiles("Stamen.TonerLite") %>%
        addPolylines(data = neighbourhood_boundary(), weight = 4, color = "red") %>%
        addPolygons( #data = map_data(), 
                    weight = 0.75, color = "black", 
                    fillColor = ~msoa_pal()(Value), fillOpacity = 0.5, 
                    highlight = highlightOptions(weight = 4, color = "grey"),
                    label = ~paste(hoc_msoa_name, round(Value)), 
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
    })
}

##########################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
