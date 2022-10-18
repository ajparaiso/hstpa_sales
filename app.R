# Setup -----------------------------------------------------------------------

library(shiny)
library(maps)
library(mapproj)
library(leaflet)

# load data
source("data_prep.R")

# Define UI -------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Geography of Residential Sales Pre- and Post-HSTPA"),
  
  sidebarPanel(
    
    helpText("Sales from 04/01/2020 - 03/31/2021 are omitted.
             Sales are filtered for properties built before 1974,
             are trimmed to the 1st and 99th percentile of sales,
             and are restricted to 100 percent transfer sales."),
    
    checkboxGroupInput("sale_period",
                       label = "Period of Sale",
                       choices = unique(sort(sales_geo$sale_period)),
                       selected = NULL),
    
    checkboxGroupInput("rs_group_ariel", 
                       label = "Rent Stabilization Category",
                       choices = unique(sort(sales_geo$rs_group_ariel)),
                       selected = NULL),
    
    checkboxGroupInput("comarea_exist",
                       label = "Commerical Space",
                       choices = unique(sort(sales_geo$comarea_exist)),
                       selected = NULL)
  ),
  
  mainPanel(
    
    fluidRow(
      
      leafletOutput("map", height = 900)
    )
  )
)

# Define server logic ---------------------------------------------------------
server <- function(input, output, session) {
  
  # data to be plotted, filtering depends on inputs from the UI
  data <- reactive({
    
    data <- sales_geo 
    
    data <- filter(data, sale_period %in% input$sale_period)
    data <- filter(data, rs_group_ariel %in% input$rs_group_ariel)
    data <- filter(data, comarea_exist %in% input$comarea_exist)
  })
  
  # base layer leaflet map
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = -73.94248,
              lat = 40.70051,
              zoom = 12) %>%
      addPolygons(data = prime_cd_geo,
                  opacity = 0.4,
                  weight = 2,
                  color = "white",
                  fillColor = "black")
  })
  
  # observe modifies objects based on changes in inputs
  observe({if(nrow(data()) != 0){
    
    leafletProxy("map", data = data()) %>%
      clearMarkers() %>%
      addCircleMarkers(stroke = FALSE,
                       color = "navy",
                       radius = 5)
      
  }
    
  else{leafletProxy("map") %>% clearMarkers()}
  })
  
  output$num_matching <- renderText({format(nrow(data()), big.mark = ",")})
  
}

# Run the app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
