# funs_tract_county_estimates.r
# David Van Riper
# 2018-11-26
# 
# This script stores the functions used to create annual tract estimates for NHGIS. I am pushing
# off the funs to another script to simplify the rest of the process.

library(tidyverse)
library(lubridate)
library(padr)

# fun select two years of data from an NHGIS standardized extract. 
select_years <- function(x, year1, year2) {
  x <- filter(x, DATAYEAR == year1 | DATAYEAR == year2)
  return(x)
}

# fun selects out a state's worth of county or tract data
# used for testing purposes because processing whole country can be slow
# - expects a fully spelled out state name (e.g., "Minnesota") as a parameter
select_state <- function(x, state) {
  x <- filter(x, STATE == state)
  return(x)
}

# fun computes difference between 2000 and 2010 values of a single variable for a given GISJOIN value
# - stores the difference in diff_var 
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

# fun uses padr::pad function to insert records for each month between start_date and end_date 
# - then filters for only records that equal start_date, end_date and all July 1sts (which is the 
# day the pop estimates are for)
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

# fun fills down values from April 1, YYYY1 to April 1, YYYY2 for two variables
fill_down <- function(x, group_var, fill_var1, fill_var2){
  group_var <- enquo(group_var)
  fill_var1 <- enquo(fill_var1)
  fill_var2 <- enquo(fill_var2)
  
  x <- x %>%
    group_by(!!group_var) %>%
    fill(!!fill_var1, !!fill_var2)
  
  return(x)
}

# fun computes number of days between each record for a given GISJOIN
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

# fun computes the cumulative sum of days for each record for a given GISJOIN
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

# fun resets the cum_days_interval for April 1, YYYY2 to zero
# - this way the linear_interpolation function will just return the actual variable value for the
# April 1, YYYY2 decennial census
reset_cum_days_interval_zero <- function(x, interval_var, year){
  interval_var = enquo(interval_var)
  year = enquo(year)
  
  interval_var_name <- paste0(quo_name(interval_var))
  
  x <- x %>% 
    mutate(!!interval_var_name := replace(!!interval_var, DATAYEAR == !!year, 0))
  
  return(x)
}

# fun computes the linear interpolation for var1
# - computes the proportion of the cumulative sum of days that have passed between April 1, YYYY1 -
# date of record and the number of days in the full interval (10 years)
# - multiplies the proportion by the value of the diff_var (difference between YYYY1 and YYYY2 value 
# of variable)
# - adds that value to the var1 value for the record
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
