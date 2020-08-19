# annual_tract_estimates_temp.r
# David Van Riper
# 2018-11-20
# 
# This script creates annual tract estimates from 2000-2010. I'm mostly using this script to 
# practice and develop the code. 
# 
# 2018-11-20
# The script does the linearish interpolation from 2000-04-01 to 2010-04-01, but the intervening
# dates are all on YYYY-07-01, which is the date for the county population estimates. 
# 
# I computed the cumulative number of days from 2000-04-01 to YYYY-07-01, divide that cumsum by 
# 3652 (the number of days betwen 2000-04-01 and 2010-04-01). I then multiply that fraction by 
# the difference between 2010 population and 2000 population and add it to the 2000 population. 
# 
# Next, I need to determine how to determine the county adjustment factor using the annual county 
# estimates. I need to compute the same linearish estimate for counties based on their decennial 
# value and the intervening years. Then, you compute the ratio of between the linearish estimate 
# and the CB's annual pop estimate. Finally, you adjust the tract values by that ratio so that 
# tract pops sum to county estimate. 
# 
# 2018-11-21
# This update adds in a number of function to capture the overall process and make it repeatable
# 
# 2018-11-22 
# I finished converting all of the code to functions, and the results match what I wrote previously!
# 
# 2018-11-24
# I tested the code on the county standardized data (2000-2010) for Alabama and it ran successfully.
# 
# 2018-11-25
# Next steps for this project:
# 
# 1. Extend computation to other variables in dataset
#   a. Extend code to loop over a set of variables 
# 2. Add code to download data from Census Bureau (using curl command)
# 

library(tidyverse)
library(lubridate)
library(padr)

setwd("/Users/vanriper/Documents/scratch/tract_estimates/2000_2010/data/nhgis/")

select_years <- function(x, year1, year2) {
  x <- filter(x, DATAYEAR == year1 | DATAYEAR == year2)
  return(x)
}

select_state <- function(x, state) {
  x <- filter(x, STATE == state)
  return(x)
}

compute_diff <- function(x, diff_var, var1, var2, group_var){
  diff_var <- enquo(diff_var)
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  group_var <- enquo(group_var)
  
  diff_var_name <- paste0(quo_name(diff_var))
  var1_name <- paste0(quo_name(var1))

  x <- x %>%
       group_by(!!group_var) %>%  # group by GISJOIN
       mutate(!!var1_name := lead(!!var2)) %>%
       mutate(!!diff_var_name := !!var1 - !!var2) # compute difference between YYYY1 and YYYY2
  
  return(x)
 
}

pad_months <- function(x, group_var, date_var, start_date, end_date){
  group_var <- enquo(group_var)
  date_var <- enquo(date_var)
  start_date <- enquo(start_date)
  end_date <- enquo(end_date)
  
  start_date_str <- quo_name(start_date)
  end_date_str <- quo_name(end_date)
  
  x <- x %>% 
       group_by(!!group_var) %>%
       pad('month')  %>% 
       filter(!!date_var == start_date_str | !!date_var == end_date_str | month(!!date_var) == 7)

  return(x)
}

fill_down <- function(x, group_var, fill_var1, fill_var2){
  group_var <- enquo(group_var)
  fill_var1 <- enquo(fill_var1)
  fill_var2 <- enquo(fill_var2)
  
  x <- x %>%
       group_by(!!group_var) %>%
       fill(!!fill_var1, !!fill_var2)
  
  return(x)
}

#keep_linear_int <- function(x, var1){
#  var1 <- enquo(var1)
#  
#  var1_name <- paste0(quo_name(var1),"_orig")
#  
#  x <- mutate(x, !!var1_name := !!var1)
#  
#  return(x)
#}

compute_day_interval <- function(x, group_var, day_interval, date_var ){
  group_var <- enquo(group_var)
  day_interval <- enquo(day_interval)
  date_var <- enquo(date_var)
  
  day_interval_name <- paste0(quo_name(day_interval))
  
  x <- x %>%
       group_by(!!group_var) %>%
       mutate(!!day_interval_name := !!date_var - lag(!!date_var, default = first(!!date_var)))
#    t <- mutate(t, day_interval = date1 - lag(date1, default = first(date1)))
  
  return(x)
}

compute_cum_sum <- function(x, group_var, cum_var, interval_var){
  group_var = enquo(group_var)
  cum_var = enquo(cum_var)
  interval_var = enquo(interval_var)
  
  cum_var_name <- paste0(quo_name(cum_var))
  
  x <- x %>%
       group_by(!!group_var) %>%
       mutate(!!cum_var_name := cumsum(as.numeric(!!interval_var)))
  
 return(x) 
}

reset_cum_days_interval_zero <- function(x, interval_var, year){
  interval_var = enquo(interval_var)
  year = enquo(year)
  
  interval_var_name <- paste0(quo_name(interval_var))
  
  x <- x %>% 
       mutate(!!interval_var_name := replace(!!interval_var, DATAYEAR == !!year, 0))
  
  return(x)
}

linear_interpolation <- function(x, var1, interval_var, diff_var){
  var1 = enquo(var1)
  interval_var = enquo(interval_var)
  diff_var = enquo(diff_var)
  
  var1_name = paste0(quo_name(var1))
  
  x <- x %>%
       mutate(!!var1_name := !!var1 + ((!!interval_var / days) * !!diff_var))
  
#  t <- mutate(t, CL8AA = CL8AA + ((cum_days_interval / days) * pdiff))
  return(x)
}

# 0. set begin and end vals, convert to dates and find number of days between them
begin_date <- ymd("2000-04-01")
end_date <- ymd("2010-04-01")

# a. set the decade interval
decade_interval <- interval(begin_date, end_date)

# b. find number of days in interval 
days <- decade_interval / days(1)

# 1. Import NHGIS standardized TST - time varies by row - 1990-2010
# Based on my work with lead(), I should just use TVC because it will be easier
t <- read_csv("nhgis1052_ts_geog2010_tract.csv")

# 2. Subset t to only keep two years of data
t <- select_years(t, 2000, 2010)

# a. Subset t for one state (to speed up testing)
t <- select_state(t, "Alabama")

# 3. Compute difference between 2000 and 2010 data 
t <- compute_diff(t, CL8AA_diff, d2010, CL8AA, GISJOIN)

# 4. Add a date field for 1990, 2000 and 2010 records 
t <- mutate(t, date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
                                    (DATAYEAR == 2000) ~ "2000-04-01",
                                    (DATAYEAR == 2010) ~ "2010-04-01")) 

# a. convert data_str to a date type
t <- mutate(t, date1 = ymd(date_str))

# 5. Add records to hold the intercensal values
t <- pad_months(t, GISJOIN, date1, "2000-04-01", "2010-04-01")

# d. fill diff and CL8AA down by group 
t <- fill_down(t, GISJOIN, CL8AA_diff, CL8AA)

# e. save original linear interpolation value in new variable for future comparisons 
#t <- keep_linear_int(t, CL8AA)

# 6. Compute number of days between each record's date value 
t <- compute_day_interval(t, GISJOIN, days_interval, date1)

# 7. Compute cumulative sum of days_interval for each group
# Wrap days_interval in the as.numeric because days_interval is a difftime object
# We will use this cumulative sum when computing the annual tract value for the linearish interpolation
t <- compute_cum_sum(t, GISJOIN, cum_days_interval, days_interval)

# a. re-set DATAYEAR == 2010 cum_days_interval to 0 - replace() function worked
t <- reset_cum_days_interval_zero(t, cum_days_interval, 2010)

# 8. Compute the CL8AA value using the following:
t <- linear_interpolation(t, CL8AA, cum_days_interval, CL8AA_diff)
