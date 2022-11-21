
# Pacakgae used
library(tidyverse)
library(ggplot2)
library(tmap)
library(ggmap)
library(sf)
library(mapview)
library(leaflet)
library(viridis)
library(geojsonio)
library(broom)

# Load dataset 
food_inspect<- read.csv("foodinspection.csv")
food_inspect$location<- str_remove_all(food_inspect$location, "\\(") 
food_inspect$location <- str_remove_all(food_inspect$location, "\\)")
food_inspect$resultdttm <- as.Date(food_inspect$resultdttm, na.rm = FALSE)


f_i<- food_inspect%>% separate(location,c("latitude", "longtitude"), sep = ",", fill = "right") 
f_i<- f_i %>% select(-dbaname)
f_i$latitude <- as.double(f_i$latitude)
f_i$longtitude <- as.double(f_i$longtitude)
f_i<- drop_na(f_i)

# select data for this year 
food1 <- f_i %>% group_by(businessname) %>% filter(resultdttm> '2022-01-01') %>% mutate(rank = row_number(businessname)) %>%
  filter(rank == 1)

fi_sf<- st_as_sf(food1, coords=c("longtitude","latitude" ),crs=4326)

# Create maps

## use ggplot2
  
ggplot(data = fi_sf)+
  geom_sf(aes(color = licstatus))+
  ggtitle(label = "Food Establishment Inspections", subtitle = "License Status of Business")

## use mapview
mapview(fi_sf,zcol="licstatus")

# use leaflet
m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()
m %>% addProviderTiles(providers$Stamen.Toner)

leaflet(data = food1) %>% addTiles() %>%
  addMarkers(~longtitude, ~latitude, popup = ~as.character(licstatus), label = ~as.character(licstatus))

# The Shiny App
library(shiny)
library(shinythemes)
library(ggplot2)

ui <- fluidPage(navbarPage("Food Establishment Inspections", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
                            tabPanel("Overview", leafletOutput("map"),height = 700),
                            tabPanel("Restaurant Information", 
                                     fluidPage(
                                       tabsetPanel(
                                         tabPanel("License Information", 
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      selectInput("license", label = "License status of restaurants", choices = fi_sf$licstatus)),
                                                    mainPanel(
                                                      plotOutput("plot")))),
                                         tabPanel("Other Information")))),
                            tabPanel("Location")
) 
)



server <- function(input, output, session) {
  # Tab: overview
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addMarkers(data = food1, ~longtitude, ~latitude,  clusterOptions = markerClusterOptions(), label = ~as.character(licstatus))})
  

  # Tab: Restaurant License
   restaurant <- reactive({
     input$license 
     })
  
  output$plot <- renderPlot({
    if (restaurant() == "Active"){
      fi_sf %>% filter(licstatus == "Active") %>% 
        ggplot()+
        geom_sf(aes(colour = licstatus))+
        ggtitle(label = "Food Establishment Inspections", subtitle = "Active Restaurant")
      } else {fi_sf%>% filter(licstatus != "Active")%>% 
          ggplot()+
          geom_sf()+
          ggtitle(label = "Food Establishment Inspections", subtitle = "Inactive Restaurant")}}, res = 96)
  # Tab: Other Information
  
  # Tab: Location
}

shinyApp(ui, server)