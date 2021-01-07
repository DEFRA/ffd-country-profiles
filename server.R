# shiny background file 

source("R/report_generation.R")


server <- function(input, output, session){


  output$report <- downloadHandler(

    
    filename = function(){
      withProgress(message = "Naming your file", value = 0, {
        incProgress(1)
        paste("Country-profile", input$country, Sys.Date(), ".docx") %>%
        str_replace(" ", "-")
      })
    },
    
    content = function(file){
      withProgress(message = "Downloading your slides", value = 0, {
        incProgress(1)
        # input$name, 
        # input$years c(2017, 2018, 2019)
        my_word_doc <- main(input$country, c(2017, 2018, 2019))
        print(my_word_doc,"my_Word.docx")
        file.copy("my_Word.docx", file)
      }) # Close content
    }
  )
}

