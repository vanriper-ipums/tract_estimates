# create_final_1990_2010_tract_estimate_data_files.r
# David Van Riper
# 2019-04-18
# 
# This script reads in a 2010 census tract extract from NHGIS, keeps the state name, county name, state 
# fips code, and county fips code to join with tract estimates. 
# 
# The script also breaks apart data frames by state so that users don't have to wade through so much 
# data.

library(tidyverse)
library(lubridate)

#constants
outdata_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/outdata/"

################
# Create a vector of state names from the datasets::state builtin 
names <- datasets::state.name

# append DC to the names dataset
names <- append(names, "District Of Columbia")

# Create a vector of state abbreviations from the datasets::state builtin
state_abb <- datasets::state.abb 

# append DC to list
state_abb <- append(state_abb, "DC")

# create a dictionary like data structure from state_Abb
names(state_abb) <- c("01", "02", "04", "05","06", "08", "09", "10", "12", "13", "15", "16", "17", "18",
                      "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32",
                      "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47",
                      "48", "49", "50", "51", "53", "54", "55", "56", "11")

names(names) <- c("01", "02", "04", "05","06", "08", "09", "10", "12", "13", "15", "16", "17", "18",
                  "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32",
                  "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47",
                  "48", "49", "50", "51", "53", "54", "55", "56", "11")

################
# CENSUS TRACT PREP
# 
# Read in 2010 tract extract
t10 <- read_csv("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/nhgis/nhgis1145_ds172_2010_tract.csv")

###
# NHGIS 2010 Tract df - preparing the fields
# 
# Keep required fields from 2010 tract extract
t10 <- t10 %>%
  select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA)

###
# Create the GEOID10 field
t10 <- t10 %>%
  mutate(GEOID10 = paste0(STATEA, COUNTYA, TRACTA))

###
# Arrange the df to pull geoid10 by GISJOIN
t10 <- t10 %>%
  select(GISJOIN, GEOID10, STATE, STATEA, COUNTY, COUNTYA, TRACTA)

###
# Read in Female by age CSVs

# Females 
female_90_00 <- read_csv("/pkg/popgis/labpcs/data_projects/tract_estimates/1990_2000/data/outdata/tct90_00_female.csv", guess_max = 200000)
female_00_10 <- read_csv("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/outdata/tct00_10_female.csv", guess_max = 200000)

# row bind df together into a single df
female <- bind_rows(female_90_00, female_00_10)%>%
  select(-STATE, -cum_days_interval, -n_diff, -n.x, -n.y, -var_ratio, -n) %>%
  rename(DATE = date1, VALUE = n_est, VAR_CODE = var_code)

# delete extra dfs for memory management
rm(female_90_00)
rm(female_00_10)

# garbage cleanup to free up memory
gc()

# join the t10 attributes onto the female df, then drop certain fields, then filter out
# the decennial values
female <- left_join(female, t10, by="GISJOIN") %>%
  select(GISJOIN, GEOID10, STATE, STATEA, COUNTY, COUNTYA, TRACTA, DATE, VAR_CODE, VALUE) %>%
  arrange(VAR_CODE, GISJOIN, DATE) %>%
  filter(DATE != "1990-04-01" & DATE != "2000-04-01" & DATE != "2010-04-01")

###
# Read in Male by age CSVs

# Males
male_90_00 <- read_csv("/pkg/popgis/labpcs/data_projects/tract_estimates/1990_2000/data/outdata/tct90_00_male.csv", guess_max = 200000)
male_00_10 <- read_csv("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/outdata/tct00_10_male.csv", guess_max = 200000)

# row bind df together into a single df
male <- bind_rows(male_90_00, male_00_10)%>%
  select(-STATE, -cum_days_interval, -n_diff, -n.x, -n.y, -var_ratio, -n) %>%
  rename(DATE = date1, VALUE = n_est, VAR_CODE = var_code)

# delete extra dfs for memory management
rm(male_90_00)
rm(male_00_10)

# garbage cleanup to free up memory
gc()

# join the t10 attributes onto the male df, then drop certain fields, then filter out
# the decennial values
male <- left_join(male, t10, by="GISJOIN") %>%
  select(GISJOIN, GEOID10, STATE, STATEA, COUNTY, COUNTYA, TRACTA, DATE, VAR_CODE, VALUE) %>%
  arrange(VAR_CODE, GISJOIN, DATE) %>%
  filter(DATE != "1990-04-01" & DATE != "2000-04-01" & DATE != "2010-04-01")

##################
# Prepare age subtotals, sex subtotals, and total pop values for each tract 
# from the male and female tbls 
# 
# 0. Split apart var_code into sex and age 
# 1. Merge male and female to single tbl 
# 2. Summarize tbl by sex, age, and GISJOIN/date to generate subtotals 

# 0. Split apart var_code in male and female into sex and age 
female <- female %>%
  mutate(SEX = substr(VAR_CODE, 1, 6),
         AGE = substr(VAR_CODE, 8, 15))

male <- male %>%
  mutate(SEX = substr(VAR_CODE, 1, 4),
         AGE = substr(VAR_CODE, 6, 13))

# 1. Merge male and female to single tbl 
mf <- bind_rows(female, male) 

# 1. Summarize tbl by sex, age, and GISJOIN/date to generate subtotals
sex <- mf %>%
  group_by(GISJOIN, GEOID10, STATE, STATEA, COUNTY, COUNTYA, TRACTA, DATE, SEX) %>%
  summarise(VALUE = sum(VALUE)) %>%
  rename(VAR_CODE = SEX)

age <- mf %>%
  group_by(GISJOIN, GEOID10, STATE, STATEA, COUNTY, COUNTYA, TRACTA, DATE, AGE) %>%
  summarise(VALUE = sum(VALUE)) %>%
  rename(VAR_CODE = AGE)

total <- mf %>%
  group_by(GISJOIN, GEOID10, STATE, STATEA, COUNTY, COUNTYA, TRACTA, DATE) %>%
  summarise(VALUE = sum(VALUE)) %>%
  mutate(VAR_CODE = "total_pop") %>%
  select(GISJOIN, GEOID10, STATE, STATEA, COUNTY, COUNTYA, TRACTA, DATE, VAR_CODE, VALUE)

# 2. Round the male, female, age, and total tbls to four decimal places
female <- female %>%
  mutate(VALUE = round(VALUE,4))

male <- male %>%
  mutate(VALUE = round(VALUE,4))

sex <- sex %>%
  mutate(VALUE = round(VALUE,4))

age <- age %>%
  mutate(VALUE = round(VALUE,4))

total <- total %>%
  mutate(VALUE = round(VALUE,4))

# 3. ungroup the sex, age, and total dataframes to speed up queries 
sex <- ungroup(sex)
age <- ungroup(age)
total <- ungroup(total)

##################
# Create final output CSVs from various input tbls 

# Unique IDs for each state - for splitting tbls by state
state <- female %>%
  distinct(STATEA)

# 1. Females by age 
# for the age and sex by age files, we don't need to loop over the var_code_uniq values because each 
# df only contains needed var_codes
for(i in state$STATEA){
  # get state abbreviation for i
  j <- state_abb[[i]]
  
  # path to output file
  temp_file <- paste0(outdata_dir, "female_age_long_", j, ".csv")
  
  # create a temp_df to hold a state's worth of data
  temp_df <- female %>%
    filter(STATEA == i) %>%
    select(-SEX, -AGE) 
  
  # tidy up the df by moving the VAR_CODE to columns 
  # reorder the fields to that age5_9 are in correct place
  temp_df_tidy <- temp_df %>%
    spread(VAR_CODE, VALUE) %>%
    select(GISJOIN:DATE, female_age0, female_age1_4, female_age5_9, female_age10_14:female_age85)

  # write out text file using write.csv - this ensures that strings are quoted correctly 
  # get a warning message when reading in CSV to R, but it just names the missing column X1
  write.csv(file = temp_file, x = temp_df_tidy, quote = TRUE)
  
  # create a wide temp_df to hold a state's worth of data
  temp_file_wide <- paste0(outdata_dir, "female_age_wide_", j, ".csv")
  temp_df_wide <- temp_df %>%
    mutate(YEAR = year(DATE)) %>%
    unite("UNIQ_ID", VAR_CODE, YEAR) %>%
    select(-DATE) %>%
    spread(UNIQ_ID, VALUE) %>%
    select(GISJOIN:TRACTA, female_age0_1990:female_age1_4_2009, female_age5_9_1990:female_age5_9_2009, female_age10_14_1990:female_age85_2009)
  
  
  # write out text file using write.csv - this ensures that strings are quoted correctly 
  # get a warning message when reading in CSV to R, but it just names the missing column X1
  write.csv(file = temp_file_wide, x = temp_df_wide, quote = TRUE)
  
  #  }
  
}

# 2. Males by age 
# for the age and sex by age files, we don't need to loop over the var_code_uniq values because each 
# df only contains needed var_codes
for(i in state$STATEA){
  # get state abbreviation for i
  j <- state_abb[[i]]
  
  # path to output file
  temp_file <- paste0(outdata_dir, "male_age_long_", j, ".csv")
  
  # create a temp_df to hold a state's worth of data
  temp_df <- male %>%
    filter(STATEA == i) %>%
    select(-SEX, -AGE)

    # tidy up the df by moving the VAR_CODE to columns 
  # reorder the fields to that age5_9 are in correct place
  temp_df_tidy <- temp_df %>%
    spread(VAR_CODE, VALUE) %>%
    select(GISJOIN:DATE, male_age0, male_age1_4, male_age5_9, male_age10_14:male_age85)
  
  # write out text file using write.csv - this ensures that strings are quoted correctly 
  # get a warning message when reading in CSV to R, but it just names the missing column X1
  write.csv(file = temp_file, x = temp_df_tidy, quote = TRUE)
  
  # create a wide temp_df to hold a state's worth of data
  temp_file_wide <- paste0(outdata_dir, "male_age_wide_", j, ".csv")
  temp_df_wide <- temp_df %>%
    mutate(YEAR = year(DATE)) %>%
    unite("UNIQ_ID", VAR_CODE, YEAR) %>%
    select(-DATE) %>%
    spread(UNIQ_ID, VALUE) %>%
    select(GISJOIN:TRACTA, male_age0_1990:male_age1_4_2009, male_age5_9_1990:male_age5_9_2009, male_age10_14_1990:male_age85_2009)
  
  
  # write out text file using write.csv - this ensures that strings are quoted correctly 
  # get a warning message when reading in CSV to R, but it just names the missing column X1
  write.csv(file = temp_file_wide, x = temp_df_wide, quote = TRUE)
  
  #  }
  
}

# 3. Sex 
for(i in state$STATEA){
  # get state abbreviation for i
  j <- state_abb[[i]]
  
  # path to output file
  temp_file <- paste0(outdata_dir, "sex_long_", j, ".csv")
  
  # create a temp_df to hold a state's worth of data
  temp_df <- sex %>%
    filter(STATEA == i)
  
  # tidy up the df by moving the VAR_CODE to columns 
  # reorder the fields to that age5_9 are in correct place
  temp_df_tidy <- temp_df %>%
    spread(VAR_CODE, VALUE)
  
  # write out text file using write.csv - this ensures that strings are quoted correctly 
  # get a warning message when reading in CSV to R, but it just names the missing column X1
  write.csv(file = temp_file, x = temp_df_tidy, quote = TRUE)
  
  # create a wide temp_df to hold a state's worth of data
  temp_file_wide <- paste0(outdata_dir, "sex_wide_", j, ".csv")
  temp_df_wide <- temp_df %>%
    ungroup() %>%
    mutate(YEAR = year(DATE)) %>%
    unite("UNIQ_ID", VAR_CODE, YEAR) %>%
    select(-DATE) %>%
    spread(UNIQ_ID, VALUE)
  
  # write out text file using write.csv - this ensures that strings are quoted correctly 
  # get a warning message when reading in CSV to R, but it just names the missing column X1
  write.csv(file = temp_file_wide, x = temp_df_wide, quote = TRUE)
  
  #  }
  
}


# 4. Age 
for(i in state$STATEA){
  # get state abbreviation for i
  j <- state_abb[[i]]
  
  # path to output file
  temp_file <- paste0(outdata_dir, "age_long_", j, ".csv")
  
  # create a temp_df to hold a state's worth of data
  temp_df <- age %>%
    filter(STATEA == i)
  
  # tidy up the df by moving the VAR_CODE to columns 
  # reorder the fields to that age5_9 are in correct place
  temp_df_tidy <- temp_df %>%
    spread(VAR_CODE, VALUE) %>%
    select(GISJOIN:DATE, age0, age1_4, age5_9, age10_14:age85)
  
  # write out text file using write.csv - this ensures that strings are quoted correctly 
  # get a warning message when reading in CSV to R, but it just names the missing column X1
  write.csv(file = temp_file, x = temp_df_tidy, quote = TRUE)
  
  # create a wide temp_df to hold a state's worth of data
  temp_file_wide <- paste0(outdata_dir, "age_wide_", j, ".csv")
  temp_df_wide <- temp_df %>%
    ungroup() %>%
    mutate(YEAR = year(DATE)) %>%
    unite("UNIQ_ID", VAR_CODE, YEAR) %>%
    select(-DATE) %>%
    spread(UNIQ_ID, VALUE) %>%
    select(GISJOIN:TRACTA, age0_1990:age1_4_2009, age5_9_1990:age5_9_2009, age10_14_1990:age85_2009)
  
  # write out text file using write.csv - this ensures that strings are quoted correctly 
  # get a warning message when reading in CSV to R, but it just names the missing column X1
  write.csv(file = temp_file_wide, x = temp_df_wide, quote = TRUE)
  
  #  }
  
}


# 5. Total pop
for(i in state$STATEA){
  # get state abbreviation for i
  j <- state_abb[[i]]
  
  # path to output file
  temp_file <- paste0(outdata_dir, "total_long_", j, ".csv")
  
  # create a temp_df to hold a state's worth of data
  temp_df <- total %>%
    filter(STATEA == i)
  
  # tidy up the df by moving the VAR_CODE to columns 
  # reorder the fields to that age5_9 are in correct place
  temp_df_tidy <- temp_df %>%
    spread(VAR_CODE, VALUE) 
  
  # write out text file using write.csv - this ensures that strings are quoted correctly 
  # get a warning message when reading in CSV to R, but it just names the missing column X1
  write.csv(file = temp_file, x = temp_df_tidy, quote = TRUE)
  
  # create a wide temp_df to hold a state's worth of data
  temp_file_wide <- paste0(outdata_dir, "total_wide_", j, ".csv")
  temp_df_wide <- temp_df %>%
    ungroup() %>%
    mutate(YEAR = year(DATE)) %>%
    unite("UNIQ_ID", VAR_CODE, YEAR) %>%
    select(-DATE) %>%
    spread(UNIQ_ID, VALUE) 
  
  # write out text file using write.csv - this ensures that strings are quoted correctly 
  # get a warning message when reading in CSV to R, but it just names the missing column X1
  write.csv(file = temp_file_wide, x = temp_df_wide, quote = TRUE)
  
  #  }
  
}

############
# Packaging data into ZIP files 
# 
# Can use the state.abb information from the built-in state dataset 
# Then just need to match up the codes with the abbreviations

# use a for loop to create ZIP files from CSVs
#  - using R's builtin ZIP utility 
#  - packaging plan 
#     - one ZIP file for long CSVs
#     - one ZIP file for wide CSVs
#     - long_<state.abb>.zip
#     - wide_<state.abb>.zip

setwd(outdata_dir)
for(i in state$STATEA){
  
  #get the state.abb from i
  j <- state_abb[[i]]
  
  # vector of long CSV files per state
  long_csv_file_names <- c(paste0("total_long_", j, ".csv"),
                       paste0("age_long_", j, ".csv"),
                       paste0("sex_long_", j, ".csv"),
                       paste0("male_age_long_", j, ".csv"),
                       paste0("female_age_long_", j, ".csv"),
                       paste0("total_long_codebook.txt"),
                       paste0("age_long_codebook.txt"),
                       paste0("sex_long_codebook.txt"),
                       paste0("male_age_long_codebook.txt"),
                       paste0("female_age_long_codebook.txt"))
  
  
  # vector of wide CSV file names per state
  wide_csv_file_names <- c(paste0("total_wide_", j, ".csv"),
                             paste0("age_wide_", j, ".csv"),
                             paste0("sex_wide_", j, ".csv"),
                             paste0("male_age_wide_", j, ".csv"),
                             paste0("female_age_wide_", j, ".csv"),
                             paste0("total_wide_codebook.txt"),
                             paste0("age_wide_codebook.txt"),
                             paste0("sex_wide_codebook.txt"),
                             paste0("male_age_wide_codebook.txt"),
                             paste0("female_age_wide_codebook.txt"))  
  
  # path to TOTAL long output zip
  long_temp_file <- paste0(outdata_dir, "long_", j, ".zip")
  wide_temp_file <- paste0(outdata_dir, "wide_", j, ".zip")

  # create two zip files per state - one containing long CSVs and one containing wide CSVs
  zip(zipfile = long_temp_file, files = long_csv_file_names)
  zip(zipfile = wide_temp_file, files = wide_csv_file_names)
  
}

# Writing out HTML code for the download links

# 0. Create an empty vector to hold the HTML 
temp_html_table <- vector(mode="character")

# 1. Loop over all states creating correct HTML code for the table
for(i in state$STATEA){
  j <- state_abb[[i]]
  k <- names[[i]]
  
  state_file_paths <- c(paste0("<tr><td><a href = ", "\"", "http://assets.nhgis.org/tract-estimates/wide_", j, ".zip>", "\"", k, "</a></td>"),
                        paste0("<td><a href = ", "\"", "http://assets.nhgis.org/tract-estimates/long_", j, ".zip>", "\"", k, "</a></td></tr>"))
  
  temp_html_table <- c(temp_html_table, state_file_paths)
}

# 2. Write out contents of temp_html_table to a text file
write_lines(temp_html_table, paste0(outdata_dir, "download_table.txt"))
# creating CSVs of attribute names for documentation 

# total 
# write_csv(age_attributes, "/Users/vanriper/Documents/scratch/tract_estimates/outdata/age_attributes.txt")
# write_csv(age_wide_attributes, "/Users/vanriper/Documents/scratch/tract_estimates/outdata/age_wide_attributes.txt")
# write_csv(sex_attributes, "/Users/vanriper/Documents/scratch/tract_estimates/outdata/sex_attributes.txt")
# write_csv(sex_wide_attributes, "/Users/vanriper/Documents/scratch/tract_estimates/outdata/sex_wide_attributes.txt")
# write_csv(female_attributes, "/Users/vanriper/Documents/scratch/tract_estimates/outdata/female_attributes.txt")
# write_csv(female_wide_attributes, "/Users/vanriper/Documents/scratch/tract_estimates/outdata/female_wide_attributes.txt")
# write_csv(total_wide_attributes,"/Users/vanriper/Documents/scratch/tract_estimates/outdata/total_wide_attributes.txt")
