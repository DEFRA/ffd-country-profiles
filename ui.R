
#Load Libraries
library(shiny)
library(shinydashboard)

source("R/download.R")


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
                     tags$h2(strong(p("introduction1"))),
                     tags$h4(p("why /how to use the app"))
              ) # column
      ), # tabItem
      tabItem(tabName = "download",
             tags$h4(p("down")),
             downloadButton("donwnload_button")
             )
    ) # tabItems
  ) # dashboardBory
) # dashboardPage


