# testing out functions stored in separate script
# 
# Steps 4, 7 and 8 are the ones that we'd want to run again, once for each specified variable.
# All other steps really only need to be run once per data frame.

# loads functions from script
source("/Users/vanriper/Documents/scratch/tract_estimates/2000_2010/scripts/funs_tract_county_estimates.r")

setwd("/Users/vanriper/Documents/scratch/tract_estimates/2000_2010/data/nhgis/")

# 0. set begin and end values, convert to dates and find number of days between them
begin_date <- ymd("2000-04-01")
end_date <- ymd("2010-04-01")

# a. set the decade interval
decade_interval <- interval(begin_date, end_date)

# b. find number of days in interval 
days <- decade_interval / days(1)

# 1. Import NHGIS standardized TST - time varies by row - 1990-2010
t <- read_csv("nhgis1052_ts_geog2010_tract.csv")
c <- read_csv("nhgis1052_ts_geog2010_county.csv")

# 2. Subset t to only keep two years of data
t <- select_years(t, 2000, 2010)
c <- select_years(c, 2000, 2010)

# a. Subset t for one state (to speed up testing)
t <- select_state(t, "Minnesota")
c <- select_state(c, "Minnesota")

# 3. Add a date field for 1990, 2000 and 2010 records 
t <- mutate(t, date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
                                    (DATAYEAR == 2000) ~ "2000-04-01",
                                    (DATAYEAR == 2010) ~ "2010-04-01")) 

c <- mutate(c, date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
                                    (DATAYEAR == 2000) ~ "2000-04-01",
                                    (DATAYEAR == 2010) ~ "2010-04-01")) 

# a. convert data_str to a date type
t <- mutate(t, date1 = ymd(date_str))
c <- mutate(c, date1 = ymd(date_str))

# 4. Compute difference between 2000 and 2010 data 
t <- compute_diff(t, CL8AA_diff, d2010, CL8AA, GISJOIN)
c <- compute_diff(c, CL8AA_diff, d2010, CL8AA, GISJOIN)

# 4. Add records to hold the intercensal values
t <- pad_months(t, GISJOIN, date1, "2000-04-01", "2010-04-01")
c <- pad_months(c, GISJOIN, date1, "2000-04-01", "2010-04-01")

# 5. Compute number of days between each record's date value 
t <- compute_day_interval(t, GISJOIN, days_interval, date1)
c <- compute_day_interval(c, GISJOIN, days_interval, date1)

# 6. Compute cumulative sum of days_interval for each group
t <- compute_cum_sum(t, GISJOIN, cum_days_interval, days_interval)
c <- compute_cum_sum(c, GISJOIN, cum_days_interval, days_interval)

# a. re-set DATAYEAR == 2010 cum_days_interval to 0 - replace() function worked
t <- reset_cum_days_interval_zero(t, cum_days_interval, 2010)
c <- reset_cum_days_interval_zero(c, cum_days_interval, 2010)

# 7. fill diff and CL8AA down by group 
t <- fill_down(t, GISJOIN, CL8AA_diff, CL8AA)
c <- fill_down(c, GISJOIN, CL8AA_diff, CL8AA)

# 8. Compute the CL8AA value using the following:
t <- linear_interpolation(t, CL8AA, cum_days_interval, CL8AA_diff)
c <- linear_interpolation(c, CL8AA, cum_days_interval, CL8AA_diff)
