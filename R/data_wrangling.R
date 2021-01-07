# Provides all funcionality to transform raw data extracted from the database into processed data suitable to posterior write the country profile report. 
# This processed data is used to create the country profiles word document.


library(stringr)
library(reshape2)
source("R/hmrc_database.R")



merge_columns <- function(df,  years, imp_colname = "ti_", exp_colname = "te_", source_data = "db"){
  # Creates new columns in the dataframe df for total imports ti_ and total exports te_. 
  # Merges together arrivals and imports A+I in one single column named ti_ "total imports".
  # Merges departures and exports D+E in another single column te_ "toal exports" for each 
  # year being passed 
  
  # Note: new columns names created are: ti_2018, ti_2019, te_2018 and te_2019 if years = c(2018, 2019).
  # Returns dataframe with those new columns 
  
  if(source_data == "db"){
    
    eu_imp_prefx <- "A_"
    eu_exp_prefx <- "D_"
    no_eu_imp_prefx <- "I_"
    no_eu_exp_prefx <- "E_"
    
  } 
  if(source_data == "hmrc_web"){
  
    eu_imp_prefx <- "EU - Imports_"
    eu_exp_prefx <- "EU - Exports_"
    no_eu_imp_prefx <- "Non EU - Imports_"
    no_eu_exp_prefx <- "Non EU - Exports_"
  }
 
  for (year in years) {
    import_cols <- c(paste0(no_eu_imp_prefx, year), paste0(eu_imp_prefx, year))
    export_cols <- c(paste0(no_eu_exp_prefx, year), paste0(eu_exp_prefx, year))
    
    df[, paste0(imp_colname, year)] = rowSums(df[ , import_cols], na.rm = TRUE)
    df[, paste0(exp_colname, year)] = rowSums(df[ , export_cols], na.rm = TRUE)
  }
  
  return(df)
}



compute_average_ti_te <- function(df, years, imp_colname = "ti_", exp_colname = "te_", 
                                  imp_colname_avg = "ti_avg", exp_colname_avg = "te_avg"){
  # Creates two new columns on the dataframe "df".
  # These are total import average "ti_avg" and total export average "te_avg". 
  # Eg: if df has ti_2017, ti_2018, ti_2019 then computes the average of all the ti_years present in df.
  # This does the same for total exports te_. 
  # Returns the df with the total imported and exported average columns. 

  import_cols <- c()
  export_cols <- c()
  
  for (year in years){
    import_cols <- append(import_cols, paste0(imp_colname, year))
    export_cols <- append(export_cols, paste0(exp_colname, year))
  }
  df[, imp_colname_avg] = rowMeans(df[ , import_cols], na.rm = TRUE)
  df[, exp_colname_avg] = rowMeans(df[ , export_cols], na.rm = TRUE)
  
  return(df)
}



compute_rank_ti_te <- function(df, colname_imp_usedto_rank="ti_avg", colname_exp_usedto_rank="te_avg",
                               rank_imp_colname = "ti_ranking", rank_exp_colname="te_ranking", ties_method="first"){
  # Creates two new columns with ranking information. 
  # te_ranking column is created and orders rows from high to low export average quantities.
  # This is the same for ordering total import averages. 
  # This can be usefull when ordering countries or HS4 codes by total imports/ exports.
  # Returns the dataframe with the new columns on it. 
  
  df[, rank_imp_colname] <- rank(desc(df[, colname_imp_usedto_rank]), ties.method = ties_method)
  df[, rank_exp_colname] <- rank(desc(df[, colname_exp_usedto_rank]), ties.method = ties_method)
  
  return(df)
}



# Rename this to ti_te_agrigood_countries_ranking?? 
create_cn_allcountries_agrigoods <- function(raw_data, selected_years, file_path, agrigoods_hs4_bound=2400){
  # Orchestrates the creation of an RDS file containing country names "cn_" ranking by their average of imports and exports.  
  # This is used to create a place holder to know how important the Country profile selected is ranked by compared against all countries.
  # Eg: Country profile X is the UK's 1st most important market of agrigood products in terms of value. 
  
  # The funcion checks whether the RDS table existis in the system if not it creates it.
  # This RDS file is stored inside "/data/processed/world/" 
  # File name follows the name convetion indexInfo_countriesInfo_productsInfo_yearsInfo
  # Hence, its named cn_allcountries_agrigoods_2017_2018_2019_.RDS as an example for 2017, 2018 and 2019 report.  
  
  # The table has world country names "cn" on index and agregates all agrigoods products "hs4" by imports and exports. 
  # 
  # EG: 
  #
  # Country     ti_avg  te_avg  ti_ranking te_ranking
  # Canada      10000   20000       20          15
  # Sigapore     2333    4329       42          31
  # France    9322234 8884209        1           6
  # ....
  # 
  # On basic strokes table represents how countries are ranked by total imports average for the specific years selected. 
  # The aggregated data for each country considers agrigoods only. 
  # Same is applied to create total export ranking.
  
  preprocessed_data <- raw_data %>% filter(hs4_code < agrigoods_hs4_bound) %>% dcast(master_country_name ~ type + year, fun.aggregate = sum)
  preprocessed_data <- merge_columns(preprocessed_data, selected_years)
  preprocessed_data <- compute_average_ti_te(preprocessed_data, selected_years)
  preprocessed_data <- compute_rank_ti_te(preprocessed_data)
  saveRDS(preprocessed_data, file_path)
}



create_hs4_allcountries_allgoods <- function(raw_data, selected_years, file_path){
  # Orchestrates the creation of and RDS file containing hs4 codes on index and agregates
  # all world countries imports and exports by hs4 poducts "FFD and FFD+".
  # The only columns used are ti_avg and te_avg. 
  
  # Table utility
  # This table is not used on its own.
  # this table is merged with a table contaning specific info about the country selected by HS4 products.
  
  # The funcion checks whether the RDS table existis in the system or it creates it (Extracting data from posgress db).
  # The file is stored inside "/data/processed/world/" 
  # File name follows name convetion indexInfo_countriesInfo_productsInfo_yearsInfo
  # Hence, its name is: hs4_allcountries_allgoods_2017_2018_2019_.RDS as an example for 2017, 2018 and 2019 report. 
 
  # Simplified table ex:
  #
  # hs4     ti_2017    te_2017     ti_2018    te_2018    ti_2019    te_2019       ti_avg         te_avg
  # 0102   401136873  376227090  398854049  341296165  400838450  370996147  4.002765e+08  3.628398e+08
  # 0103    16411514    4517778   15965862    2488243   23703488    1792405  1.869362e+07  2.932809e+06
  # 0105 ... 
  # ...
  
  # On columns we have Exports/Imports from/To uk/rest of the world.
  # considering all agrigoods FFD +FFD and split by years.

  preprocessed_data <- raw_data %>% dcast(hs4_code ~ type + year, fun.aggregate = sum)
  preprocessed_data <- merge_columns(preprocessed_data, selected_years)
  preprocessed_data <- compute_average_ti_te(preprocessed_data, selected_years)
  saveRDS(preprocessed_data, file_path)
  
}


create_allcountries_agrigoods_tables <- function(credentials, 
                                                 sql_query, 
                                                 selected_years, 
                                                 selected_country="geo_master_table.master_country_name",
                                                 prefix_fname_cn = "cn_allcountries_agrigoods",
                                                 prefix_fname_hs4 = "hs4_allcountries_allgoods", 
                                                 fn_data = "data",
                                                 fn_prepoc = "processed",
                                                 fn_world = "world",
                                                 ext = ".RDS"
                                                 ){
  # This Function orchestrates de production of two RDS files inside "/data/processed/world" 
  # Those are: cn_allcountries_agrigoods_yearsSelected and hs4_allcountries_agrigoods_yearsSelected. 
  # One has country names "cn" on index the other hs4 codes. 
  # Both agregate world/country data extracted from HMRC/ posgreSQL database by imports and exports value. 
  # For more specific info look at each func. 
  
  # NOTE both files are created only ones for the specific years selected. 
  # Hence, the same RDS files are used to create all country profiles if selected years are the same. 
  # Therefore, new RDS files will be only created if the user selects different years in order to compute total import and exports average again.
  
  # The funcion checks whether the RDS table existis in the system if not it creates and save it using prefix file names + years.
  # Data source from PosgreSQL "HMRC" data. 
  # Data containing agrigoods information.
  
  # To create the tables we use
  # credentials -> password, port and others to conect to database
  # sql_query -> to extract the information required
  # selected_country -> if "geo_master_table.master_country_name" means selecting all countries not only one
  # prefix_fname_cn -> prefix file name used to save country names on index RDS file
  # prefix_fname_hs4 -> prefix file name used to save HS4 codes on index RDS file
  # fn_data -> parent data folder name 
  # fn_prepoc -> processed folder name 
  # fn_world -> woild folder name to save files into 
  
  # country names on index 
  fname_cn_allcountries_agrigoods <- paste0(c(prefix_fname_cn, selected_years, ext), collapse = "_")
  path_cn_allcountries_agrigoods <- file.path(getwd(), fn_data, fn_prepoc, fn_world, fname_cn_allcountries_agrigoods)
  
  # hs4 code on index 
  fname_hs4_allcountries_allgoods <- paste0(c(prefix_fname_hs4, selected_years, ext), collapse = "_")
  path_hs4_allcountries_allgoods <- file.path(getwd(), fn_data, fn_prepoc, fn_world, fname_hs4_allcountries_allgoods)
  
  if(!file.exists(path_cn_allcountries_agrigoods) || !file.exists((path_hs4_allcountries_allgoods))){
    
    years_sql_str <- get_years_in_sql_format(selected_years)
    raw_data <- get_data_from_db(hmrc_driver_name = credentials[1], 
                                 hmrc_dbname = credentials[2],
                                 hmrc_host = credentials[3],
                                 hmrc_port = credentials[4],
                                 hmrc_user = credentials[5],
                                 hmrc_password = credentials[6],
                                 sql_query = sql_query,
                                 user_selected_years = years_sql_str,
                                 user_selected_country = selected_country)
    if(!file.exists(path_cn_allcountries_agrigoods)){
      create_cn_allcountries_agrigoods(raw_data, selected_years, path_cn_allcountries_agrigoods)
    }
    
    if(!file.exists(path_hs4_allcountries_allgoods)){
      # TODO check if its all goods or agrigoods only 
      # if so rename allgoods to agrigoods even filename hadcoded inside func
      create_hs4_allcountries_allgoods(raw_data, selected_years, path_hs4_allcountries_allgoods)
    }
  }
}



create_cn_allcountries_allindustriesgoods <- function(fn_raw_manual_csv, fn_processed, selected_years, 
                                                      fn_data, fn_manual, fn_preproc, fn_world, ext=".RDS", raw_ext=".csv"){
  # Checks whether the RDS table existis in the system or it creates it.
  # creates one table with country names "cn" on index
  # this one reads data from a manually extracted csv file.  
  # This file named ..........  is extracted from HMRC website. 
  # https://www.uktradeinfo.com/trade-data/ots-custom-table/
  # and it differs from create_cn_allcountries_agrigoods 
  # because it contains not only agrigood data but all comodity
  # goods not available in defra database. 
  # Therefore this file needs to be extracted manually "for now". 
  # Returns true if no errors have arrised. 
  
  # This table is used in the word doc When looking at the most important HS4 product trade with specific country profile. 
  # We compute the relative percentage of the product traded with the specific country vs the overall world trade of the product. 
  # Eg: Product HS3029 is the UK's primary export to Country selected and accounts for 80% of the UK'S total exports of product XYZ. 

  output_fn <- paste0(c(fn_processed, selected_years, ext), collapse = "_")
  output_path <- file.path(getwd(), fn_data, fn_preproc, fn_world, output_fn)
  
  fn_raw_manual <- paste0(c(fn_raw_manual_csv, selected_years, raw_ext), collapse = "_")
  
  if(!file.exists(file.path(getwd(), fn_data, fn_manual, fn_raw_manual))){
    # TODO managing Error. 
    print(stringr::str_interp("Error: file name ${fn_raw_manual} does not exists."))
    print(file.path(getwd(), fn_data, fn_manual, fn_raw_manual))
    OK <- FALSE
  }
  else{
    if(!file.exists(output_path)){
      
      # where to read data from 
      input_path <- file.path(getwd(), fn_data, fn_manual, fn_raw_manual)
      
      data <- read.csv(input_path)
      # TODO document manual files column names! or will fail. 
      # pivot
      data <- data %>% dcast(Country ~ FlowType + Year, value.var = "Value", fun.aggregate = sum)
      data <- merge_columns(data, selected_years, source_data = "hmrc_web")
      data <- compute_average_ti_te(data, selected_years)
      data <- compute_rank_ti_te(data)
      saveRDS(data, output_path)
      OK <- TRUE
    }
  }
  return(OK)
}



create_hs4_selectedcountry_agrigooods <- function(credentials, sql_query, selected_years, selected_country, 
                                                  fn_processed, 
                                                  needed_file1, 
                                                  needed_file2, 
                                                  fn_data,
                                                  fn_preproc,
                                                  fn_world, 
                                                  fn_manual, 
                                                  ext = ".RDS"){
  # Checks whether the RDS table existis in the system or it creates it.
  # To create this table we need three different data sources.
  # One, extract data from the database for the country selected only.
  # Two, merge this table with world data for each specific hs4 code 
  # in index. In this we want to know how much is UK importing / exporting
  # with the partner country selected but also how much UK is imp / exp 
  # the same hs4 product from the rest of the world. 
  # Third, merge the info above with a manually created csv file 
  # conteining short sh4 descriptions used on tables rather than long
  # hs4 descriptions. This csv can be found in country profiles 
  # excel file short descriptions tab. 
  # Returns true if no errors have arrised. 
  
  country_folder_name <- str_replace(selected_country, " ", "_")
  print(c("Country folder name ", country_folder_name))
  output_fn <- paste0(c(fn_processed, selected_years, ext), collapse = "_")
  print(c("Ouput file name ", output_fn))
  output_path <- file.path(getwd(), fn_data, fn_preproc, country_folder_name, output_fn)
  print(c("output_path name ", output_path))
  does_file_exist <- file_exists(fn_data, fn_preproc, country_folder_name, output_fn)
  
  hs4_code_colname <- "hs4_code"
  
  avg_imp_colname <- "ti_avg"
  avg_exp_colname <- "te_avg"
  suffix_country <- "_C"
  suffix_world <- "_W"
  
  ti_avg_country_colname <- paste0(avg_imp_colname, suffix_country)
  te_avg_country_colname <- paste0(avg_exp_colname, suffix_country)
  ti_avg_world_colname <- paste0(avg_imp_colname, suffix_world)
  te_avg_world_colname <- paste0(avg_exp_colname, suffix_world)
  
  
  ti_avg_rank_colname <- "ti_ranking"
  te_avg_rank_colname <- "te_ranking"
  
  ti_avg_rank_country_colname <- paste0(ti_avg_rank_colname, suffix_country)
  te_avg_rank_country_colname <- paste0(te_avg_rank_colname, suffix_country)
    
  ti_avg_rank_world_colname <- paste0(ti_avg_rank_colname, suffix_world)
  te_avg_rank_world_colname <- paste0(te_avg_rank_colname, suffix_world)
  
  
  
  if (!does_file_exist){
    
    years_sql_str <- get_years_in_sql_format(selected_years)
    sql_selected_country <- get_country_sql_format(selected_country)
    
    print(years_sql_str)
    print(sql_selected_country)
    
    data <- get_data_from_db(hmrc_driver_name = credentials[1], 
                                 hmrc_dbname = credentials[2],
                                 hmrc_host = credentials[3],
                                 hmrc_port = credentials[4],
                                 hmrc_user = credentials[5],
                                 hmrc_password = credentials[6],
                                 sql_query = sql_query,
                                 user_selected_years = years_sql_str,
                                 user_selected_country = sql_selected_country)
    
    imp_exp_prefix <- unique(data[c("type")])
   
    exp_prefix <- paste0(imp_exp_prefix[1,], "_")
    imp_prefix <- paste0(imp_exp_prefix[2,], "_")

    #data <- compute_average_ti_te(data, selected_years, imp_colname = "E_", exp_colname = "I_",)
    #data <- compute_average_ti_te(data, selected_years, imp_colname = "A_", exp_colname = "D_",)
    
    data <- data %>% dcast(hs4_code ~ type + year, fun.aggregate = sum)
    data <- compute_average_ti_te(data, selected_years, imp_colname = imp_prefix, exp_colname = exp_prefix)

    # merge with all countries data source HMRC
    fn_hs4_allcountries_allgoods <- paste0(c(needed_file1, selected_years, ext), collapse = "_")
    path_hs4_allcountries_allgoods <- file.path(getwd(), fn_data, fn_preproc,  fn_world,  fn_hs4_allcountries_allgoods)
    hs4_allcountries_allgoods <- readRDS(path_hs4_allcountries_allgoods)
    
    data <- merge(data, hs4_allcountries_allgoods[, c(hs4_code_colname, avg_exp_colname, avg_imp_colname)], by=hs4_code_colname, all = TRUE, suffixes = c(suffix_country, suffix_world))

    hs4_codes_descriptions_path <- file.path(getwd(), fn_data, fn_manual, needed_file2)
    hs4_codes_descriptions <- read.csv(hs4_codes_descriptions_path)
    # Merge with short descriptions manual data extracted
    hs4_codes_descriptions <- na.omit(hs4_codes_descriptions)
    hs4_codes_descriptions$hs4_codes2 <- stringr::str_pad(hs4_codes_descriptions$hs4_code, 4, pad = "0")

    
    data <- merge(data, hs4_codes_descriptions, by.x = hs4_code_colname, by.y = "hs4_codes2", all.x = TRUE)   

    data <- data %>% filter(hs4_code < 2400)
    
    
    # compute rankings for both World and Country selected
    data <- compute_rank_ti_te(data, 
                               colname_imp_usedto_rank=ti_avg_country_colname, 
                               colname_exp_usedto_rank=te_avg_country_colname, 
                               rank_imp_colname = ti_avg_rank_country_colname, 
                               rank_exp_colname=te_avg_rank_country_colname)
    
    
    # compute ratio used in word reports
    ratio_imp_colname <- "ti_ratio"
    ratio_exp_colname <- "te_ratio"
    data[, ratio_imp_colname] <- data[,  ti_avg_country_colname] / data[, ti_avg_world_colname] * 100
    data[, ratio_exp_colname] <- data[,  te_avg_country_colname] / data[, te_avg_world_colname] * 100
    
    saveRDS(data, output_path)
    
  }
  
}



create_rds_files <- function(credentials, sql_query, selected_years, selected_country,
                             all_countries_sql = "geo_master_table.master_country_name", 
                             fn_cn_allcountries_agrigoods = "cn_allcountries_agrigoods",
                             fn_hs4_allcountries_allgoods = "hs4_allcountries_allgoods",
                             fn_cn_allcountries_allindustriesgoods = "cn_allcountries_allindgoods",
                             fn_hs4_selectedcountry_agrigoods = "hs4_selectedcountry_agrigoods", 

                             # compulsory csv file in sistem both manually extracted 
                             fn_raw_world_trade_csv = "world_data_all_trade",    # 2017_2018_2019  ### ERROORRR
                             condensed_hs4_fn = "condensed_hs4_code.csv",
                             fn_data="data", fn_manual="manual", fn_prepoc="processed", fn_world="world"){
  # This function orchestrates the creation of all files needed
  # to create the country profiles report. 
  # Note some files such as tables containing world data
  # are used across the creation of all country profiles. 
  # Hence they are only created ones for specific country report years.
  
  # This Funcion produces two RDS files inside "/data/processed/world" 
  # Those are: cn_allcountries_agrigoods and hs4_allcountries_agrigoods. 
  # One has country names "cn" on index the other hs4 codes. 
  # 
  # Data extracted only once for specific years. Does not change if only different country name selected. 
  # Data source from PosgreSQL "HMRC" data. 
  # Data containing agrigoods information 
  create_allcountries_agrigoods_tables(credentials, sql_query, selected_years, 
                                      selected_country = all_countries_sql,
                                      prefix_fname_cn = fn_cn_allcountries_agrigoods,
                                      prefix_fname_hs4 = fn_hs4_allcountries_allgoods, 
                                      fn_data = fn_data,
                                      fn_prepoc = fn_prepoc,
                                      fn_world = fn_world,
                                      ext = ".RDS")

  is_ok <- create_cn_allcountries_allindustriesgoods(fn_raw_manual_csv = fn_raw_world_trade_csv, 
                                            fn_processed = fn_cn_allcountries_allindustriesgoods,
                                            selected_years = selected_years, 
                                            fn_data = fn_data,
                                            fn_manual = fn_manual, 
                                            fn_preproc = fn_prepoc,
                                            fn_world = fn_world)
  

  
  create_hs4_selectedcountry_agrigooods(credentials, sql_query, selected_years, selected_country = selected_country, 
                                                                  fn_processed = fn_hs4_selectedcountry_agrigoods,
                                                                  needed_file1 = fn_hs4_allcountries_allgoods,
                                                                  needed_file2 = condensed_hs4_fn, 
                                                                  fn_data = fn_data,
                                                                  fn_preproc = fn_prepoc, 
                                                                  fn_world = fn_world,
                                                                  fn_manual = fn_manual)
  
  
  #files_created <- all(allcountries_ag_tables & cn_allcountries_allgoods & cn_allcountries_allgoods)
  return (is_ok) # CHANGE TO TRUE ONCE TO COMPLETE AL PROCESS
}

 