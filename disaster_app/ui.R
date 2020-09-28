library(shiny)
library(leaflet)
library(plotly)
navbarPage(
      "Disaster Analytics ",
      tabPanel("Geolocation of Twitter",
              leafletOutput("geotweets",height = "600px")),
      tabPanel("Google Trends",
              plotlyOutput("gt",height = "600px")),
      tabPanel("Analysis", 
               plotlyOutput("Analysis",height = "600px"))
    )
  