# Main File orchestrator to create Word reports if not found in the system directory under "data/reports/countryname/"


library(stringr)
library(officer)
source("R/data_wrangling.R")
source("R/word_doc.R")



file_exists <- function(f1, f2, f3, file_name){
  # Recursively asserts whether a folder exists in system
  # if folder does not exist it will create one. 
  # Returns whether the file name exists within the tree file system passed
  folders <- list(f1, f2, f3)
  file_path <- getwd()
  
  for (folder in folders){
    path <- file.path(file_path, folder)
    
    if(!dir.exists(path)){
      dir.create(path)
    }
    file_path <- path
  }
  file_path <- file.path(file_path, file_name)
  file_does_exists <- file.exists(file_path)
  
  return(file_does_exists)
}


##################### 
# MAIN ORCHESTRATOR # 
#####################
main <- function(country, years){
  
  # TEMPORALLY HERE 
  credentials <- get_db_credentials()
  sql_query <- get_db_sql_query()
  
  data_folder <- "data"
  reports_folder  <- "reports"
  preprocessed_folder  <- "processed"
  country_folder_name  <- str_replace(country, " ", "_")


  # setting word and rds file names convention 
  word_file_name <- paste(c("country_profile", country_folder_name, years,".docx"), collapse="_")
  rds_files <- c("cn_all_countries_agrigoods.RDS", "hmrc_cn_all_countries_allgoods.RDS", "hmrc_hs4_selected_country.RDS")

  # Check whether the country profile word report is already created and saved in system
  word_report_exists <- file_exists(data_folder,reports_folder,country_folder_name,word_file_name)

  # word report exist
  if(word_report_exists){
    word_doc <- read_docx(file.path(getwd(), data_folder, reports_folder, country_folder_name, word_file_name))

  } else {
    # Create rds files and other preprocessed files if those required are not present in the system. 
    # returns True when the process is completed
    print("About to create RDS files ")
    print(file.path(getwd(), data_folder, reports_folder, country_folder_name, word_file_name))
    rds_created <- create_rds_files(credentials, sql_query, years, country)

    if(rds_created){
      print("About to create Word doc ")
      word_doc <- create_report(country, years)
      print("Word created ?")
      print(word_doc, target = file.path(getwd(), data_folder, reports_folder, country_folder_name, word_file_name))

    }
    else{
      word_doc <- read_docx("data/word_templates/empty_word.docx")
      # Do not save
      # print(word_doc, target = file.path(getwd(), data_folder, reports_folder, country_folder_name, word_file_name))
    }
  }
  
  return(word_doc)

}








