# Load the package set 1
library(shiny)
library(leaflet)
library(data.table)
library(choroplethrZip)
library(devtools)
library(MASS)
library(vcd)

# Load the package set 2
library(tidyverse)
library(viridis)
library(plotly)
library(dplyr)
library(lubridate)
library(stringr)

# Generate the crime data cleaned by Hongjie Liu
complaint = read_csv("Data/complaint_data_2020_2022.csv")

# complaint = 
#   complaint %>%
#   mutate(
#     offense = ifelse(offense == "MURDER & NON-NEGL. MANSLAUGHTER", "Murder", offense),
#     offense = str_to_title(tolower(offense))
#   )


# Main function
function(input, output) {
  
  # Interactive Map
  
  # Read and update the input data
  
  # Start_date of the crime data
  start_date = eventReactive(input$button, {
    start_date = input$Date_Range[1]
  })
  
  # End_date of the crime data
  end_date = eventReactive(input$button, {
    input$button
    end_date = input$Date_Range[2]
  })
  
  # The crime type of the complaint
  crime_type = eventReactive(input$button, {
    input$button
    crime_type = input$Crime_Type
  })
  
  # The start hour in each day
  start_hour = eventReactive(input$button, {
    start_hour = input$StartHour
  })
  
  # The end hour in each day
  end_hour = eventReactive(input$button, {
    end_hour = input$EndHour
  })
  
  # Suspect Information
  suspect_sex = eventReactive(input$button, {
    input$button
    suspect_sex = input$Suspect_Sex
  })
  
  suspect_age = eventReactive(input$button, {
    input$button
    suspect_age = input$Suspect_Age
  })
  
  suspect_race = eventReactive(input$button, {
    input$button
    suspect_race = input$Suspect_Race
  })
  
  # Victim Information
  victim_sex = eventReactive(input$button, {
    input$button
    victim_sex = input$Victim_Sex
  })
  
  victim_age = eventReactive(input$button, {
    input$button
    victim_age = input$Victim_Age
  })
  
  victim_race = eventReactive(input$button, {
    input$button
    victim_race = input$Victim_Race
  })  
  
  borough = eventReactive(input$button, {
    input$button
    borough = input$Borough
  })  
  
  # Filter the crime data based on user input
  updated_crime_data = eventReactive(input$button, {
    # Filter data by date_range, crime_type, and time_range
    updated_crime_data = 
      complaint %>% 
        filter(
          mdy(complaint$date_time) >= start_date() & 
          mdy(complaint$date_time) <= end_date()
          ) %>%
        filter(offense %in% crime_type()) %>%
        filter(as.numeric(hour) >= start_hour() & as.numeric(hour) <= end_hour()) %>%
        filter(susp_sex %in% suspect_sex()) %>%
        filter(susp_age_group %in% suspect_age()) %>%
        filter(tolower(susp_race) %in% tolower(suspect_race())) %>%
        filter(vic_sex %in% victim_sex()) %>%
        filter(vic_age_group %in% victim_age()) %>%
        filter(tolower(vic_race) %in% tolower(victim_race())) %>%
        filter(tolower(borough) %in% tolower(borough()))
  })
  
  # Set Color palette
  col = c('#fae60c','#67bbd6','#e67545',
          '#6859eb','#9858b8','#a5203f',
          '#00a562')
  
  # Crime type legend
  var = c("Rape", "Murder", "Burglary", "Robbery","Felony Assault",
          "Grand Larceny of Motor Vehicle", "Grand Larceny")
  
  
  # Color palette for map
  pal = colorFactor(col, domain = var)
  
  # Output interactive map
  output$map <- renderLeaflet({
    leaflet(data = updated_crime_data()) %>% 
      addProviderTiles('Stamen.TonerLite') %>% 
      setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>% 
      addCircles(# Location of the crime
                 lng = ~longitude, 
                 lat = ~latitude, 
                 radius = 40, 
                 stroke = FALSE, 
                 fillOpacity = 0.4,
                 # color of the crime
                 color = ~pal(offense),
                 popup = ~as.character(
                   paste(paste("Crime Type: ", offense),
                         paste("Crime Level:", tolower(level)),
                         paste("Daily Hour:", hour), 
                         paste("Precinct: ",  precinct), sep = "| " 
                 ))) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~var,
                title = "Crime Type ",
                opacity = 0.8 ) %>% 
      addMarkers(clusterOptions = markerClusterOptions())
  })
  


  
  
}
