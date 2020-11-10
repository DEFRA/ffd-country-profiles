
#Load Libraries
library(shiny)
library(shinydashboard)


#Application
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Country profiles"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",  icon = icon("play"), tabName = "introduction"),
      menuItem("Download Slides", icon = icon("file-powerpoint"), tabName = "slides")
    ) #close sidebarMenu
  ), #close dashboardSidebar
  
  dashboardBody( 
    tabItems(
      tabItem(tabName = "introduction",
              column(9,
                     tags$h2(strong(p("introduction1"))),
                     tags$h4(p("why /how to use the app"))
              ) # column
      ) # tabItem
    ) # tabItems
  ) # dashboardBory
) # dashboardPage

server <- function(input, output) {}
shinyApp(ui = ui, server = server)