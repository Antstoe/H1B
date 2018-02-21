library(dplyr)
library(ggplot2)
library(readxl)
library(hashmap)

#Empty data frame
h1b_df = data.frame()

for(year in seq(2018,2016)) {
  
  print(paste0("Reading ", year, " records .."))
  raw_data_path = paste0("./data/",year,"_raw_data.xlsx")
  new_df = read_excel(raw_data_path)
  
  print(paste0("Raw data size: ", as.character(dim(new_df))))  
  
  # Adding Year column to dataframe
  print("Mutating year ..")
  new_df = new_df %>% 
    mutate(YEAR = as.character(year))
  
  print(paste0("Mutated data size: ", as.character(dim(new_df))))  
  
  # Selecting only the relevant columns
  new_df = new_df %>% 
    select(CASE_NUMBER, 
           CASE_STATUS,
           EMPLOYER_NAME,
           SOC_NAME,
           SOC_CODE,
           JOB_TITLE,
           FULL_TIME_POSITION,
           PREVAILING_WAGE,
           PW_UNIT_OF_PAY,
           WORKSITE_CITY,
           WORKSITE_STATE,
           YEAR)
  
  # Merging data with already transformed data
  print("Merging data ..")
  h1b_df = rbind(h1b_df, new_df)
  
  print(paste0("Merged data size: ",as.character(dim(h1b_df))))
  }


# Saving read data frame 
saveRDS(h1b_df,"h1b_df_no_transform.rds")


