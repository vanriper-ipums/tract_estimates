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

# 0. set begin and end vals, convert to dates and find number of days between them
begin_date <- ymd("2000-04-01")
end_date <- ymd("2010-04-01")

# a. set the decade interval
decade_interval <- interval(begin_date, end_date)

# b. find number of days in interval 
days <- decade_interval / days(1)

# 1. Import NHGIS standardized TST - time varies by row - 1990-2010
# Based on my work with lead(), I should just use TVC because it will be easier
t <- read_csv("nhgis1051_ts_geog2010_tract.csv")

# 2. Subset t to only keep two years of data
t <- select_years(t, 2000, 2010)

# a. Subset t for one state (to speed up testing)
t <- select_state(t, "Alabama")

# 3. Compute difference between 2000 and 2010 data 
t <- group_by(t, GISJOIN)

# a. put the 2010 values on each 2000 datayear row
t2 <- mutate(t, d2010 = lead(CL8AA))

# b. subtract 2000 from 2010 to get difference
t2 <- mutate(t2, diff = d2010 - CL8AA)

# 4. Add a date field to the 2000 and 2010 records 
t2 <- mutate(t2, date_str = case_when((DATAYEAR == 2000) ~ "2000-04-01",
                                      (DATAYEAR == 2010) ~ "2010-04-01")) 

# a. convert data_str to a date type
t2 <- mutate(t2, date1 = ymd(date_str))

# 5. Add records to hold the intercensal values
# 
# a. group by GISJOIN 
t2 <- group_by(t2, GISJOIN)

# b. pad out df by months, which creates a df that's way to big
t2 <- pad(t2, 'month')

# c. keep only the months needed for "linear" values 
t2 <- filter(t2, date1 == "2000-04-01" | date1 == "2010-04-01" | month(date1) == 7)

# d. fill diff and CL8AA down by group 
t2 <- fill(t2, diff, CL8AA)

# 6. Compute number of days between each record's date value 
t2 <- mutate(t2, days_interval = date1 - lag(date1, default = first(date1)))

# 7. Compute cumulative sum of days_interval for each group
# Wrap days_interval in the as.numeric because days_interval is a difftime object
# We will use this cumulative sum when computing the annual tract value for the linearish interpolation
t2 <- mutate(t2, cum_days_interval = cumsum(as.numeric(days_interval)))

# a. re-set DATAYEAR == 2010 cum_days_interval to 0 - replace() function worked
# I reset the cum_days_interval to 0 because we don't actually want to change the 
# CL8AA value for 2010. It is the known value for that date. 
t2 <- mutate(t2, cum_days_interval = replace(cum_days_interval, DATAYEAR==2010, 0))

# 8. Compute the CL8AA value using the following:
# CL8AA = CL8AA + ((cum_days_interval / days) * diff)
t2 <- mutate(t2, CL8AA = CL8AA + ((cum_days_interval / days) * diff))
