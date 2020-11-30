

server <- function(input, output, session){


  output$report <- downloadHandler(

    
    filename = function(){
      paste0("Country-profile-", str_replace(input$country, " ", "-") , "-", Sys.Date(), ".docx")
    },
    
    content = function(file){
      tempReport <- file.path(tempdir(), "country_profiles_report_template.Rmd")
      file.copy("country_profiles_report_template.Rmd", tempReport, overwrite = TRUE)

      params <- list(name = input$name, country = input$country)

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )


}

