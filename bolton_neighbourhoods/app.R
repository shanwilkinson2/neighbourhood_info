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
    data_refresh_date <- "14/06/2021"
    neighbourhood_data <- readRDS("dashboard_indicators.RDS")
    neighbourhood_names <- c("Breightmet/Little Lever", "Central/Great Lever", "Chorley Roads", 
                             "Crompton/Halliwell", "Farnworth/Kearsley", "Horwich",
                             "Rumworth", "Turton", "Westhoughton")
    neighbourhood_boundaries <- readRDS("neighbourhood boundaries.RDS")
    local_health_data_msoa <- readRDS("local health data with boundaries.RDS")

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
                    menuItem("About the data", tabName = "about", icon = icon("comment-dots"))
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
                    p("text")
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
                    p("data sourced from... link"),
                    p(glue::glue("Data last refreshed: {data_refresh_date}")),
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
            mutate(pct_value_neighbourhood = round(pct_value_neighbourhood, 1),
                   pct_value_bolton = round(pct_value_bolton, 1)) %>%
            select(IndicatorName, 
                   `Neighbourhood value` = pct_value_neighbourhood, `Bolton value` = pct_value_bolton,
                   Sex, Age, `Time period` = Timeperiod) 
    })


    # create table
    output$table1 <- DT::renderDT({
        table_data()
   }, filter = "top", rownames = FALSE)
    
    # reactive dataset for map
    # why is neighbourhood called neighbourhood.y?
    map_data <- reactive({
        local_health_data_msoa %>%
            filter(neighbourhood.y == input$select_neighbourhood & IndicatorName == input$select_indicator) 
    })
    
    # reactive boundary of selected neighbourhood
    neighbourhood_boundary <- reactive({
        neighbourhood_boundaries %>%
            filter(neighbourhood_name == input$select_neighbourhood) 
    })
    
    
    # map palette
    msoa_pal <- reactive({
        colorNumeric(
            palette = "Blues", 
            domain = map_data()$Value, 
            na.color = "white")
    })
    
    output$indicator_map <- renderLeaflet({
        map_data() %>%
        leaflet() %>%
        addResetMapButton() %>%
        addProviderTiles("Stamen.TonerLite") %>%
        addPolylines(data = neighbourhood_boundary(), weight = 4, color = "black") %>%
        addPolygons( #data = map_data(), 
                    weight = 0.75, color = "grey", 
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
