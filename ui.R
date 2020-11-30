library(shinydashboard)
library(tidyverse)
library(shiny)

# Importing all info str variables used in introduction tab
source("data/introduction_text.R")
# Importing available countries from csv
countries_data <- read.csv("data/Country_Groups.csv") 
available_countries <- countries_data$country


#Application
dashboardPage(
  skin = "green",
  dashboardHeader(title = "Country profiles"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",  icon = icon("play"), tabName = "introduction"),
      menuItem("Download Slides", icon = icon("file-powerpoint"), tabName = "download")
    ) #close sidebarMenu
  ), #close dashboardSidebar
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              column(9,
                     tags$h2(strong(p(intro_h1))),
                     tags$h4(p(intro_p1)),
                     tags$h2(strong(p(intro_h2))),
                     tags$h4(p(intro_p2)),
                     tags$h2(strong(p(intro_h3))),
                     tags$h4(p(intro_p3)),
                     tags$h2(strong(p(intro_h4))),
                     tags$h4(p(intro_p4)),
                     tags$h2(strong(p(intro_h5))),
                     tags$h4(p(intro_p5)),
              )
      ), # tabItem
      tabItem(tabName = "download",
              
              tags$h4(p("Introduce your name:")),
              textInput("name", "Introduce your name:", value = ""),
              
              tags$h4(p("Select country:")),
              selectInput("country", label = "Country", choices = available_countries),
              
              tags$h4(p("Press Submit")),
              downloadButton("report", "Generate report"),
              
              textOutput("debug"),
              )
    ) # tabItems
  ) # dashboardBory
) # dashboardPage










