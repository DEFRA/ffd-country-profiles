# Module to encapsulate download funcionality 
# ui is a button 
# server side allows to download an empty file report when button is clicked.

library(magrittr)
library(officer)


create_doc <- function(path = "app_reports/template.docx"){
  
  doc <- officer::read_docx() %>%
    body_add_par("Hello == SHINY ==  world!", style = "Normal")
  #print(doc, target = path)  
  return(doc)
}


createReportButton <- function(id, label="Create Report"){
  ns <- NS(id)
  tagList(
    actionButton(ns("button"), label = label),
    verbatimTextOutput(ns("out"))
  )
}


createReportServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(input$button,{ create_doc() })
      output$out <- renderPrint({"Downloading doc R. . ."})
    }
  )
}

downloadReportButton <- function(id){
  
}


# 
# countriesSectionDD <- function(id, label = "Countries"){
#   ns <- NS(id)
#   selectInput(ns("dropdown"), label = label,
#               choices = c("Spain", "UK", "USA", "France"))
# }
#   
# countriesSelectionServer <- function(){}
# 
# downloadServer <- function(id){
#   moduleServer(
#     id,
#     function(input, output, session){
#       output$donwnloadReport <- downloadH
#     }
#  )
#}
