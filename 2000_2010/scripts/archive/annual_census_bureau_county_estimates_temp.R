# annual_census_bureau_county_estimates_temp.r
# David Van Riper
# 2018-11-20
# 
# This script reads in the Census Bureau's 2000-2010 annual county estimates and preps it.
# I keep only the necessary columns, AGEGRPs and YEARs. For testing purposes, I only keep
# total pop (AGEGRP == 99) and YEAR < 13 (this gets rid of July 1, 2010 county estimate)

library(tidyverse)
library(lubridate)
library(padr)

setwd("/Users/vanriper/Documents/scratch/tract_estimates/2000_2010/data/popest/")

# 1. Import CB county estimates 2000-2010
# AGEGRP = 99 (total)
# 1 = 4/1/2000 resident population estimates base
# 2 = 7/1/2000 resident population estimate
# 3 = 7/1/2001 resident population estimate
# 4 = 7/1/2002 resident population estimate
# 5 = 7/1/2003 resident population estimate
# 6 = 7/1/2004 resident population estimate
# 7 = 7/1/2005 resident population estimate
# 8 = 7/1/2006 resident population estimate
# 9 = 7/1/2007 resident population estimate
# 10 = 7/1/2008 resident population estimate
# 11 = 7/1/2009 resident population estimate
# 12 = 4/1/2010 resident 2010 Census population 
# 13 = 7/1/2010 resident population estimate
c_est<- read_csv("co-est00int-alldata-01.csv")

# 2. filter out unnecessary AGEGRPs and YEARs
# keep only AGEGRP == 99 (for now) and YEAR < 13 (don't need July 2010 for right now)
c_est<- filter(c_est, AGEGRP == 99 & YEAR < 13)

# 3. select out necessary columns
# select out columns from STATE to TOT_POP because that's all I need for now
c_est<- select(c_est, STATE:TOT_POP)

# 4. create a GISJOIN 
c_est<- mutate(c_est, GISJOIN = paste0("G",STATE,"0",COUNTY,"0"))

# 5. Create a new date column to convert YEAR to a date 
c_est<- mutate(c_est, date_str = case_when((YEAR == 1) ~ "2000-04-01",
                                   (YEAR == 2) ~ "2000-07-01",
                                   (YEAR == 3) ~ "2001-07-01",
                                   (YEAR == 4) ~ "2002-07-01",
                                   (YEAR == 5) ~ "2003-07-01",
                                   (YEAR == 6) ~ "2004-07-01",
                                   (YEAR == 7) ~ "2005-07-01",
                                   (YEAR == 8) ~ "2006-07-01",
                                   (YEAR == 9) ~ "2007-07-01",
                                   (YEAR == 10) ~ "2008-07-01",
                                   (YEAR == 11) ~ "2009-07-01",
                                   (YEAR == 12) ~ "2010-04-01"))

# 6. Convert date_str to date type 
c_est<- mutate(c_est, date1 = ymd(date_str))