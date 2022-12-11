# Load the packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(dplyr)
library(viridisLite)
library(markdown)
library(quantmod)
library(tidyr)
library(treemap)
library(forecast)
library(DT)
library(shiny)
library(leaflet)
library(plotly)
library(wordcloud2)
library(scatterD3)
library(shiny)
library(shinyWidgets)

rm(list = ls())

# UI design
dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "NYC Crime Map Before Covid", 
    disable = FALSE,
    titleWidth = 300
    ),
  
  dashboardSidebar(
    minified = FALSE,
    # Expand the width of the sidebar
    width = 300,
    
    # Crime Type
    prettyCheckboxGroup(
      "Crime_Type", 
       label = "Crime_Type",
       choices = c("Rape", "Murder", "Burglary", "Robbery", "Felony Assault", 
                   "Grand Larceny", "Grand Larceny of Motor Vehicle"
                   ),
       selected = c("Rape", "Murder", "Burglary", "Robbery", "Felony Assault", 
                    "Grand Larceny", "Grand Larceny of Motor Vehicle"
                    ),
      ),
    
    # Date time range
    dateRangeInput("Date_Range", "Change Date Range", 
                   start = "2019-09-01", end = "2019-09-30", 
                   min = "2017-01-01", max = "2019-12-31"),
    
    # Customize slider style
    setSliderColor(c("#0084ff ", "#e07f5c", "", "Teal"), c(1, 2, 4)),
    chooseSliderSkin("Flat"),
    
    # Daily time change
    sliderInput("StartHour", "Start Daily Hour", 0, 23, 0, step = 1),
    sliderInput("EndHour", "End Daily Hour", 0, 23, 23, step = 1),

    prettyCheckboxGroup(
      "Borough", 
       label = "Borough",
       choices = c("Bronx", "Brooklyn", "Staten Island",
                   "Manhattan", "Queens"),
       selected = c("Bronx", "Brooklyn", "Staten Island",
                    "Manhattan", "Queens")
      ),
    
    # Display and update button for interactive map
    actionButton("button", 
                 "Display & Update Interactive Map", 
                 style = "color: #f7f7f7; background-color: #2a6d91; border-color: #2e6da4",
                 width = 260
    )

  ),
  dashboardBody(
    sidebarLayout(position = "right", 
                  sidebarPanel
                  (
                    h4("Suspect Filter"),
                    # Suspect Information
                    
                    # Sex
                    prettyCheckboxGroup(
                    "Suspect_Sex", 
                    label = "Suspect Sex",
                    choices = c("Male", "Female", "Unclear"),
                    selected = c("Male", "Female", "Unclear"),
                    inline = TRUE
                    ),
                    
                    # Age
                    prettyCheckboxGroup(
                    "Suspect_Age", 
                    label = "Suspect Age",
                    choices = c("<18", "25-44", "45-64", 
                               "65+", "Unclear"),
                    selected = c("<18", "25-44", "45-64", 
                                "65+", "Unclear"),
                    inline = TRUE
                    ),
                    
                    # Race
                    prettyCheckboxGroup
                    (
                    "Suspect_Race", 
                     label = "Suspect Race",
                     choices = c("Black", "White Hispanic", "White", 
                                 "Black Hispanic", "Asian / Pacific Islander",
                                 "American Indian/Alaskan Native", "Unclear"),
                     selected = c("Black", "White Hispanic", "White", 
                                  "Black Hispanic", "Asian / Pacific Islander",
                                  "American Indian/Alaskan Native", "Unclear")
                    ),
                    
                    h4("Victim Filter"),
                    # Victim Information
                    
                    # Sex
                    prettyCheckboxGroup
                    (
                    "Victim_Sex", 
                    label = "Victim Sex",
                    choices = c("Male", "Female", "Unclear"),
                    selected = c("Male", "Female", "Unclear"),
                    inline = TRUE
                    ),
                    
                    # Age
                    prettyCheckboxGroup
                    (
                    "Victim_Age", 
                    label = "Victim Age",
                    choices = c("<18", "25-44", "45-64", 
                               "65+", "Unclear"),
                    selected = c("<18", "25-44", "45-64", 
                                "65+", "Unclear"),
                    inline = TRUE
                    ),
                    
                    
                    # Race
                    prettyCheckboxGroup(
                    "Victim_Race", 
                    label = "Victim Race",
                    choices = c("Black", "White Hispanic", "White", 
                               "Black Hispanic", "Asian/Pacific Islander",
                               "American Indian/Alaskan Native", "Unclear"),
                    selected = c("Black", "White Hispanic", "White", 
                                "Black Hispanic", "Asian/Pacific Islander",
                                "American Indian/Alaskan Native", "Unclear")
                                       
                    )
                  ),
                  
                  # crime map
                  mainPanel(leafletOutput("map", width = "100%", height = 800))    
                )
    )
)


