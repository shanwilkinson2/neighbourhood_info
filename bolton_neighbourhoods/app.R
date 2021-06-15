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

# Define UI for application that draws a histogram
ui <-  dashboardPage(

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
                    menuItem("About the data", tabName = "about", icon = icon("comment-dots"))
         ),
        # neighbourhood selector
        selectInput(inputId = "select_neighbourhood",
                    label = "Select neighbourhood:",
                    choices = neighbourhood_names)
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
    

    
    output$table1 <- DT::renderDT({
        data = neighbourhood_data %>%
                    # filtered_data() %>%
                    filter(neighbourhood == input$select_neighbourhood) %>%
                    select(DomainName, IndicatorName, Sex, Age, Timeperiod, 
                           neighbourhood, pct_value_neighbourhood, pct_value_bolton) %>%
                    mutate(pct_value_neighbourhood = round(pct_value_neighbourhood, 1),
                           pct_value_bolton = round(pct_value_bolton, 1))
   }, filter = "top", rownames = FALSE)
    
}

##########################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
