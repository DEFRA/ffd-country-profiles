# The word_doc.R provides all type of funcionality to create the Country profile word doc. 
# It provides funcionality to read preprocessed RDS files, write text, add tables, and others to word doc. 
# Also, provides funcionality for formating values, percentages, and currencies. 
library(flextable)

format_currency <- function (number, rounding=T, simbol="£"){

  lut <- c( 100, 1000, 1e+06, 1e+09, 1e+12)
  pre <- c( "hundred", "Thousand", "m", "bn", "tn")
  ix <- findInterval(number, lut)
  

  # if(is.na(number)){
  #   sistring <- as.character(0)
  #   print("inside nan")
  # }
  if (lut[ix]>=1e+06) {
    if (rounding==T) {
      sistring <- paste(simbol, round(number/lut[ix]), pre[ix], sep = "")
    }
    else {
      sistring <- paste(simbol, number/lut[ix], pre[ix], sep = "")
    } 
  }
  else {
    sistring <- as.character(number)
  }
  return(sistring)
}



format_table_currency <- function (number, rounding="m", simbol="", sep=" "){
  
  lut <- c( 100, 1000, 1e+06, 1e+09, 1e+12)
  pre <- c( "hundred", "Thousand", "m", "bn", "tn")
  ix <- findInterval(number, lut)

  
  if (rounding == "m") {
    if(number==0){
      sistring <- paste(simbol, 0, rounding, sep=sep)
    }
    else if (lut[ix] >= 1e+06) {
      sistring <- paste(simbol, format(round(number/1e+06, digits = 1 ),  nsmall=1, big.mark=",") ,rounding, sep = sep)
    } 
    else if(lut[ix] >= 1000){
      sistring <- paste(simbol, format( round(number/1e+06, digits = 1 ),  nsmall=1, big.mark=",") , rounding, sep = sep)
    } 
    else {
      sistring <-  paste(simbol, as.character(number), rounding, sep=sep)
    }
  }
  
  return(sistring)
}



format_percentages <- function(number, is_pct=TRUE, simbol = "%", sep=""){ 
 
  if(!is_pct){
    number <- number * 100
  }
  
  lut <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100)
  ix <- findInterval(number, lut)

  if(number==0){
    sistring <- paste(0, simbol, sep=sep)
  }
  else if (lut[ix]>0.01){
    sistring <- paste(round(number, digits = 1 ), simbol, sep = sep)
  } 
  else if(lut[ix]>0.001){
    sistring <- paste(round(number, digits = 2 ), simbol, sep = sep) 
  }
  else if(lut[ix]>0.0001){
    sistring <- paste(round(number, digits = 3 ), simbol, sep = sep)
  }
  else if(lut[ix]>0.00001){
    sistring <- paste(round(number, digits = 4 ), simbol, sep = sep)
  }
  return(sistring)
}




# ph1
value_agrigoods_trade <- function(hs4_agrigoods_sel_country){
  
  # According to HMRC the UK EXPORTS / IMPORTS % of agri-good TO country per annum;
  # Exports
  total_agrifood_from_uk_to_contry <- format_currency(sum(hs4_agrigoods_sel_country$te_avg_C, na.rm = TRUE))
  # Imports 
  total_agrifood_from_country_to_uk <- format_currency(sum(hs4_agrigoods_sel_country$ti_avg_C, na.rm = TRUE))
  
  return(list(total_agrifood_from_uk_to_contry, total_agrifood_from_country_to_uk)) 
}



# ph2
country_rank_pos <- function(selected_country, cn_agrifood_i_e){
  
  country_vs_all <- cn_agrifood_i_e %>%
    filter(master_country_name == selected_country)
  # Exports 
  e <- scales::ordinal_format()(country_vs_all$te_ranking)
  # Imports 
  i <- scales::ordinal_format()(country_vs_all$ti_ranking)
  return(list(e, i))
}


#ph3
pct_agrigoods_traded <-function(selected_country, hs4_agrigoods_sel_country, cn_all_trade){
  
  # Export
  denominator <- cn_all_trade %>%
    select(Country, te_avg) %>%
    filter(Country == selected_country)
  
  accounted_exp <- sum(hs4_agrigoods_sel_country$te_avg_C,  na.rm = TRUE) / denominator$te_avg * 100

  
  # Import
  denominator <- cn_all_trade %>%
    select(Country, ti_avg) %>%
    filter(Country == selected_country)
  
  accounted_imp <- sum(hs4_agrigoods_sel_country$ti_avg_C,  na.rm = TRUE) / denominator$ti_avg * 100
  
  return(list(accounted_exp, accounted_imp))
  
} 


#table
top_twenty_agrigood_products <- function(selected_country, hs4_agrigoods_sel_country){
  # Exports
  exp_table <- hs4_agrigoods_sel_country[order(hs4_agrigoods_sel_country$te_ranking_C, decreasing = FALSE), ][0:20, c("hs4_code", "condensed_description", "te_avg_C", "te_avg_W", "te_ratio")]
  exp_table$te_avg_C <- lapply(exp_table$te_avg_C, format_table_currency)
  exp_table$te_avg_W <- lapply(exp_table$te_avg_W, format_table_currency)
  exp_table$te_ratio <- lapply(exp_table$te_ratio, format_percentages)
  colnames(exp_table) <- c("HS4", "Description", stringr::str_interp("UK Exports to ${selected_country} (£)"), "UK Exports to World (£)", "Proportion of UK Exports")

  # Imports
  imp_table <- hs4_agrigoods_sel_country[order(hs4_agrigoods_sel_country$ti_ranking_C, decreasing = FALSE), ][0:20, c("hs4_code", "condensed_description", "ti_avg_C", "ti_avg_W", "ti_ratio")]
  imp_table$ti_avg_C <- lapply(imp_table$ti_avg_C, format_table_currency)
  imp_table$ti_avg_W <- lapply(imp_table$ti_avg_W, format_table_currency)
  imp_table$ti_ratio <- lapply(imp_table$ti_ratio, format_percentages)
  colnames(imp_table) <- c("HS4", "Description", stringr::str_interp("UK Imports from ${selected_country} (£)"), "UK Imports W", "Proportion of UK Imports")
  return(list(exp_table, imp_table))
}



create_first_page <- function(doc, country){
  doc %>%
  body_add_par(stringr::str_interp("AGRI-GOOD TRADE PROFILE"), style = "doc_title") %>%
  body_add_par(stringr::str_interp("United Kingdom  –  ${country}"), style = "doc_subtitle") %>%
  body_add_docx(src = "data/word_templates/notes_to_reader.docx") 
  return(doc)
}


create_uk_perspective <- function(doc, country, country_folder, hs4_agrigoods_sel_country, cn_agrifood_i_e, cn_all_trade){
  # uk exports 
  ph_1 <- value_agrigoods_trade(hs4_agrigoods_sel_country)
  ph_2 <- country_rank_pos(country, cn_agrifood_i_e)
  ph_3 <- pct_agrigoods_traded(country, hs4_agrigoods_sel_country, cn_all_trade) # Need World Data
 
  
  table <- top_twenty_agrigood_products(country, hs4_agrigoods_sel_country)
  
  
  # Parsing matrix to dataframe so it can be pasted to word doc
  export_table <- do.call(rbind.data.frame, table[1])
  import_table <- do.call(rbind.data.frame, table[2])
  
  doc %>%
    body_add_par(stringr::str_interp("United Kingdom's Perspective"), style = "H1") %>%
    body_add_par(stringr::str_interp("UK Exports to ${country}"), style = "H2_exports") %>%
    body_add_par(stringr::str_interp(c("According to HMRC the UK exports ${ph_1[1]} of agri-good products to ${country} per annum; ",
                                       "thus, resulting in ${country} being the UK’s ${ph_2[1]} most important market of agri-good products in term of value. ",
                                       "Agri-good products account for ${format_percentages(ph_3[[1]])} of the UK’s total export of goods to ${country}. ")), style = "bullet_point") %>%
    body_add_par(stringr::str_interp("Table 3 presents the UK’s top twenty agri-good exports to ${country}."), style = "bullet_point") %>%
    body_add_par(stringr::str_interp("${export_table$Description[1]} (HS${export_table[1, 'HS4']}) is the UK’s primary export to ${country} and
                                     accounts for ${export_table[1, 'Proportion of UK Exports']} of the UK’s total exports of ${export_table$Description[1]}."), style = "bullet_point") %>%
    body_add_par(stringr::str_interp("Table 3: UK’s top twenty exports of Agri-good products to ${country}."), style="caption") %>%
    body_add_table(export_table, style = "List Table 4 Accent 1") %>%
    body_add_par("Source: HMRC Overseas Trade Statistics 2017-2019 average", style="caption") %>%
    
    body_add_break() %>%

    body_add_par(stringr::str_interp("UK Imports from ${country}"), style = "H2_imports") %>%
    body_add_par(stringr::str_interp(c("According to HMRC the UK imports ${ph_1[2]} of agri-good products from ${country} per annum; ",
                                       "thus, resulting in ${country} being the UK’s ${ph_2[2]} most important source of agri-good products in terms of value. ",
                                       "Agri-good imports account for ${format_percentages(ph_3[[2]])} of the UK’s total goods imports from ${country}.")), style = "bullet_point") %>%
    body_add_par(stringr::str_interp("The UK’s top twenty imports from ${country} are presented in Table 4, with relevant product groups if possible dominating the list."), style = "bullet_point") %>%
    body_add_par(stringr::str_interp("${import_table$Description[1]} (HS${import_table[1, 'HS4']}) is the UK’s top import from ${country} and accounts for ${import_table[1, 'Proportion of UK Imports']} of the UK’s imports of ${import_table$Description[1]}."), style = "bullet_point") %>%
    body_add_par(stringr::str_interp("Table 4: UK’s top twenty imports of Agri-good products from ${country}."), style="caption") %>%
    body_add_table(import_table, style = "List Table 4 Accent 3") %>%
    body_add_par("Source: HMRC Overseas Trade Statistics 2017-2019 average", style="caption") %>%
    body_add_break() 
  
    ft <- flextable(import_table) 
    ft <- add_footer(ft, HS4 = "This is a note in footer" ) 
    ft <- merge_at(ft, j = 1:5, part = "footer")
    body_add_flextable(doc, ft)  

    return(doc)
}



create_report <- function(selected_country, selected_years, world_folder_name="world"){
  
  country_folder_name <- str_replace(selected_country, " ", "_")
  str_years <- paste0(selected_years, collapse = "_")
  
  hs4_agrigoods_sel_country <- readRDS(stringr::str_interp("data/processed/${country_folder_name}/hs4_selectedcountry_agrigoods_${str_years}_.RDS"))
  cn_agrifood_i_e <- readRDS(stringr::str_interp("data/processed/${world_folder_name}/cn_allcountries_agrigoods_${str_years}_.RDS"))
  cn_all_trade <- readRDS(stringr::str_interp("data/processed/${world_folder_name}/cn_allcountries_allindgoods_${str_years}_.RDS"))
    
  doc <- read_docx("data/word_templates/empty_word.docx")
  doc <- create_first_page(doc, selected_country) 
  doc <- create_uk_perspective(doc, selected_country, country_folder_name, hs4_agrigoods_sel_country, cn_agrifood_i_e, cn_all_trade)
  
  return(doc)
}




