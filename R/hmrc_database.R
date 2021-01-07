# Provides all funcionality to connect to posgreSQL database and extract/pull data out. 
# This file provides SQL squeleton queries, database credentials, parsing user selected variales into SQL string format.


#Load Libraries
library("DBI")
library("RPostgreSQL")
library("dplyr") # %>%
library("tidyverse")



get_db_credentials <- function(){
  # Returns HMRC database credentials
  
  hmrc_driver_name = "PostgreSQL"
  hmrc_dbname = ""
  hmrc_host = "10.85.4.185"
  hmrc_port = 5432
  hmrc_user = "postgres"
  hmrc_password = "Summer2018"
  
  return (c(hmrc_driver_name, hmrc_dbname, hmrc_host, hmrc_port, hmrc_user, hmrc_password))
}

 
 
get_db_sql_query <- function(){
  # Returns sql query to extract data from db 
  
  sql_query <- "SELECT
                 trademonthly.year,
                 trademonthly.month,
                 trademonthly.type,
                 trademonthly.value,
                 trademonthly.netmass,
                 cn_codes_master_table.com_code,
                 cn_codes_master_table.com_description,
                 cn_codes_master_table.hs4_code,
                 cn_codes_master_table.hs4_description,
                 cn_codes_master_table.ffd,
                 cn_codes_master_table.ffd_desc,
                 cn_codes_master_table.ffd_plus,
                 geo_master_table.master_country_name,
                 months.month AS month_name
              FROM
                ((trademonthly
                  INNER JOIN cn_codes_master_table ON trademonthly.comcode = cn_codes_master_table.com_code)
                  INNER JOIN geo_master_table ON trademonthly.codseq = geo_master_table.country_id)
                  INNER JOIN months ON trademonthly.month = Months.number

              WHERE
                (trademonthly.year IN ?selected_years)
                AND
                (geo_master_table.master_country_name = ?selected_country);"
  
  return (sql_query)
}



get_hmrc_db_connection <- function(drv, dbname, host, port, user, password){
  # Logs into the database using credentials and
  # returns db connection
  
  db <- DBI::dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  
  return(db)
}



kill_db_connections <-function (driver = drv) {
  # Close all Db connections 
  # with the connection object used
  
  all_cons <- dbListConnections(driver)
  for(con in all_cons)
    +  dbDisconnect(con)
}



get_db_query <- function(hmrc_connection, sql_query,
                                    user_selected_years, user_selected_country){
  # Interpolates user selected variables with sql squeleton query 
  # Returns data extracted from the PosgreSQL database 
  
  sql_str_interpolation <- sqlInterpolate(hmrc_connection,  sql_query,  selected_years=SQL(user_selected_years), selected_country=SQL(user_selected_country))
  data_extract <- dbGetQuery(hmrc_connection, sql_str_interpolation)
  
  return (data_extract)
  
}



get_data_from_db <- function(hmrc_driver_name, hmrc_dbname, hmrc_host, 
                         hmrc_port, hmrc_user, hmrc_password, sql_query,
                         user_selected_years, user_selected_country){
  # Orchestraes all data estraction process. 
  # Gets the connction object using user credentials 
  # Interpolate user selected variables fo country and years with SQL squeleton query
  # Retreives the data from the data base 
  # Kills all open connections 
  # Returns the data extracted given the country and years 
  
  drv <- DBI::dbDriver(drvName = hmrc_driver_name)
  hmrc_conn <-get_hmrc_db_connection(drv, hmrc_dbname, hmrc_host, hmrc_port, hmrc_user, hmrc_password)
  
  print("ha!")
  print(user_selected_country)
  print(typeof(user_selected_country))
  print(user_selected_years)
  print(typeof(user_selected_years))
  
  data_extract <- get_db_query(hmrc_conn, sql_query,
                          user_selected_years = user_selected_years, user_selected_country=user_selected_country)
  kill_db_connections(drv)
  
  return(data_extract)
}


get_years_in_sql_format <- function(years){
  # Parses an array of integer years to suitable SQL string form
  # Eg: c(2017, 2018, 2019) returns "(2017, 2018, 2019)"
  years_sql_str <- paste0(years, collapse = ", ")
  selected_years_sql <- paste0(c("(", years_sql_str, ")"), collapse = "")
  
  return(selected_years_sql)
}
  
  
  
get_country_sql_format <- function(country){
  # Adds double cuotes arround the string country being passed. 
  # Eg: "Spain" returns "'Spain'". 
  
  return(shQuote(country))
}
  



