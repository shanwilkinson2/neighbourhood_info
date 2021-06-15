# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load packages
    library(shiny)
    library(shinydashboard)
    library(dplyr)

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
        p(glue::glue("Data last refreshed: {data_refresh_date}")),
        # tab selector menu
        sidebarMenu(
                    menuItem("Using", tabName = "using", icon = icon("map-signs")),
                    menuItem("Table 1", tabName = "table_tab", icon = icon("table")),
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
        # neighbourhood selector
        selectInput(inputId = "select_indicator",
                    label = "Select indicator to map:",
                    choices = unique(neighbourhood_data$IndicatorName)
        )
        
    ),
    
    dashboardBody(
        tabItems(
            # using this
            tabItem(tabName = "using",
                    p("text")
            ),
                    
            # tab1
            tabItem(tabName = "table_tab",
                    DT::DTOutput("table1")
            ),
            
            # map
            tabItem(tabName = "map",
                    leafletOutput("indicator_map")
            ),
            
            # data sources
            tabItem(tabName = "about",
                    p("some more text")
            )
        )
    )
)

    
        
    
    
#     ####
#     sidebarLayout(
#         sidebarPanel(
#             selectInput("select_neighbourhood",
#                         "Select neighbourhood:",
#                         choices = neighbourhood_names)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             tabsetPanel(type = "tabs",
#                         
#                 tabPanel(
#                     DT::DTOutput("table1")
#                 ),
# 
#                 tabPanel(
#                     h1("Data source"),
#                     p("Link to fingertips local health"),
#                     p("include caution re significant differences, useful differences")
#                 )
#         )
#     )
# )
# )

##########################################################################################################

server <- function(input, output) {
    
    # # doesn't work
    # # dataset of all indicators for selected neighbourhood & Bolton
    # # datatable makes a table widget, can format it
    # output$filtered_data <- DT::datatable(reactive{
    #    neighbourhood_data %>%
    #         filter(neighbourhood == input$select_neighbourhood | neighbourhood == "Bolton") %>%
    #         mutate(pct_value = round(pct_value, 1))
    # })
    

    # table
    output$table1 <- DT::renderDT({
        data = neighbourhood_data %>%
                    # filtered_data() %>%
                    filter(neighbourhood == input$select_neighbourhood & 
                               DomainName == input$select_domain) %>%
                    mutate(pct_value_neighbourhood = round(pct_value_neighbourhood, 1),
                           pct_value_bolton = round(pct_value_bolton, 1)) %>%
                    select(IndicatorName, 
                           `Neighbourhood value` = pct_value_neighbourhood, `Bolton value` = pct_value_bolton,
                           Sex, Age, `Time period` = Timeperiod) 

   }, filter = "top", rownames = FALSE)
    
    # reactive dataset for map
    output$map_data <- reactive({
        local_health_data_msoa %>%
            filter(neighbourhood == input$select_neighbourhood & IndicatorName == input$select_indicator) 
    })
    
    # selected neighbourhood
    output$neighbourhood_boundary <- reactive({
        neighbourhood_boundaries %>%
            filter(neighbourhood_name == input$select_neighbourhood) 
    })
    
    
    # map palette
    output$msoa_pal <- reactive({
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
        #addPolylines(data = neighbourhood_boundary(), weight = 4, color = "black") %>%
        addPolygons( #data = map_data(), 
                    weight = 0.75, color = "grey", 
                    fillColor = ~msoa_pal()(Value), fillOpacity = 0.5, 
                    highlight = highlightOptions(weight = 4, color = "grey"),
                    label = ~paste(Value), 
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
    })
}

##########################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
