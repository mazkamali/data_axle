# app to show the map of data axle

# import necessary libraries for functions----
library(data.table)
library(plyr) #plyr needs to be loaded before dplyr
library(dplyr) #please keep dplyr near the top of libraries that are loaded; it's an important package in this script. 
library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(argonDash)
library(argonR)
library(shinyWidgets)
library(plotly)
library(htmlwidgets)
library(tmap)
library(sf)
library(shinyFiles)
library(shinyjs)
library(readxl)
library(shinyBS)
library(tidyr)
library(datasets)
library(rjson)
library(geojsonsf)
library(xlsx)
library(shinybusy)
library(reshape2)
library(scales)
library(datasets)
library(reactable)
library(magrittr)
library(readr)
library(RODBC)
library(stringr)
library(readxl)


# Load the dataset----
data.path <- "C:/Users/mkamali/OneDrive - HNTB/R Projects/data_axle"
data_file <- paste0(data.path, "/data/data_axle_SOTR_100prcntl_combined.csv")  # Ensure this file exists
df <- fread(data_file)

# Define UI----
ui <- fluidPage(
  titlePanel("Establishments Map"),
  
  # First row: Filters
  fluidRow(
    column(6,
           sliderInput("year_filter", label = "Select Year Range:",
                       min = 2015, max = 2023, value = 2023)
    ),
    column(6,
           selectInput("industry_filter", "Select Industry:",
                       choices = c("All", "Semiconductor and other electronic component manufacturing",
                                   "Industrial machinery manufacturing",
                                   "Communications equipment manufacturing",
                                   "Computer systems design and related services",
                                   "Employment services related to information",
                                   "Non-depository credit intermediation",
                                   "Oil and gas extraction",
                                   "Pipeline transportation of crude oil",
                                   "Pipeline transportation of natural gas",
                                   "Basic chemical manufacturing",
                                   "Support activities for mining",
                                   "Support activities for water transportation",
                                   "Grocery and convenience retailers",
                                   "Other heavy and civil engineering construction",
                                   "Amusement parks and arcades",
                                   "Cement and concrete product manufacturing",
                                   "Home health care services",
                                   "Depository credit intermediation"), selected = "All")
    )
  ),
  
  # Second row: Map
  fluidRow(
    column(12, leafletOutput("map", height = "800px"))
  )
)

# Define Server----
server <- function(input, output, session) {
  # Reactive dataset based on filters
  filtered_data <- reactive({
    data <- df %>% filter(Year == input$year_filter[1])
    if (input$industry_filter != "All") {
      data <- data %>% filter(Industry == input$industry_filter)
    }
    return(data)
  })

  # Render Leaflet Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(df$Longitude, na.rm = TRUE),
              lat = mean(df$Latitude, na.rm = TRUE),
              zoom = 6)
  })

  # Update map when filters change
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       radius = 1, color = "blue",
                       popup = ~paste0("<b>", Company, "</b><br>Industry: ", Industry,
                                       "<br>Employees: ", Employees,
                                       "<br>Year: ", Year))
  })
}

# Run the App
shinyApp(ui, server)