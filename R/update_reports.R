# R File responsible to update processed data/word docx for specific years and all countries

source("R/report_generation.R")

years <- c("2015", "2016", "2017", "2018", "2019")
countries <- c("Singapore")



update_country_report <- function(country_name, years){
  # Chects preprocessed files for the selected country and specific years are prensent in system dir
  # Checks world data specific selected years are also prensent in the system 
  # If any of those are not fould in its respective folder it creates them by
  # 1. retriving raw data from source "hmrc database posgress" 
  # 2. preprocess the raw data and save tables for future word report creation. 
  
  # Note: 
  # - Name standards are set   "to explain"
  # - Some raw sources need manual extraction. Those are csv files contained in data/manual folther. 
  # - If updating world_data_all_trade with 2020 info fowollow name convention to:
  # - world_data_all_trade_2018_2019_2020.csv 
  
  # setting folther names used in all project 
 
  
  main(country_name, years)
}


for(country in countries){
  print(country)
  update_country_report(country, years)
  
}

