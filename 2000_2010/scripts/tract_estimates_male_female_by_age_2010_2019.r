# tract_estimates_male_female_by_age_2010_2019.r
# Author: David Van Riper
# Created: 2020-08-10
# 
# This script prepares the tract estimates for 2010-2019. It reads in the vintage 2019 county estimates
# using tidycensus.
# 
# Notes to add to scripts:
# 1. We must make a few decisions that will impact the estimates:
#   - CCR caps matter a ton
#   - CTW values for cohorts with Inf values (e.g. no 0-4 or 5-9 in 20000 and greater than zero 0-4 and 
#   5-9 year olds in 2010)
#   - CTW denominators are females of child-bearing age, and we must choose what we mean by "child-bearing age"
#     - Strate et al. use females 20-44 for children 0-4 and females 30-49 for children 5-9, but those 
#       age ranges were based on Massachusetts-specific birth data

require(tidyverse)
require(lubridate)
require(padr)
require(tidycensus)

#### Source the tidy functions #### 
source("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/scripts/tidy_funs_tract_county_estimates.r")

# constants 
nhgis_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/nhgis/"
outdata_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/outdata/"


#### 1. Load in the NHGIS standardized sex/age by tract/county data ####
t <- read_csv(paste0(nhgis_dir, "nhgis1133_ts_geog2010_tract.csv"), guess_max = 200000)
c <- read_csv(paste0(nhgis_dir, "nhgis1133_ts_geog2010_county.csv"), guess_max = 200000)

#### 2. Keep only 2000 and 2010 tract and county estimates #### 
t <- t %>%
  filter(DATAYEAR == 2000 | DATAYEAR == 2010)

c <- c %>%
  filter(DATAYEAR == 2000 | DATAYEAR == 2010)

#### 3. Creates age groups required for CCR calculations ####
t <- compute_age_sex_cohorts(t)
c <- compute_age_sex_cohorts(c)

#### 4. Create wide df where each row is a tract ####
t_wide <- create_wide_age_cohort_df(t)
c_wide <- create_wide_age_cohort_df(c)

#### 5. Compute Cohort Change Ratios for 2000-2010 by sex/age/tract #### 
# The formula for a sex-specific CCR is:
# 
# ccr_age = Population_age_2010 / Popualtion_age-10_2000
# 
# where age is the sex-specific age group in question. We do not compute CCRs for
# ages 0-4 or 5-9; instead, we will use child-to-women ratios for those age groups.
# For the top age category (85+), the denominator is the sum of the Census 2000 counts 
# of ages 75-70, 80-84, and 85+.

t_wide_ccr <- compute_ccr(t_wide)
c_wide_ccr <- compute_ccr(c_wide)

#### 6. Generate cohort size flags for each CCR cohort #### 
# we will use the cohort size flags when we set the CCR caps. If a cohort is < 25, the
# CCR for that cohort is capped to 1.0. If a cohort is >= 25 and < 100, the CCR is capped
# at 2.0. 
t_cohort_size <- compute_cohort_size_flag(t)
c_cohort_size <- compute_cohort_size_flag(c)

#### 7. Create a usable data frame from CCRs #### 
# create a long df of CCRs by cohort
# compute a new age_code for merging with the t_cohort_size df
t_long_ccr <- create_long_ccr_df(t_wide_ccr)
c_long_ccr <- create_long_ccr_df(c_wide_ccr)

#### 8. Merge cohort_size and ccr dfs and set ccr caps in place #### 
t_long_ccr <- t_long_ccr %>%
  left_join(t_cohort_size, by = c("GISJOIN" = "GISJOIN", "age_code" = "var_code")) %>%
  select(-DATAYEAR, -age_code)

c_long_ccr <- c_long_ccr %>%
  left_join(c_cohort_size, by = c("GISJOIN" = "GISJOIN", "age_code" = "var_code")) %>%
  select(-DATAYEAR, -age_code)

# set ccr caps to 1 if ccr_cap == 1 & ccr > 1.0
# set ccr caps to 2 if ccr_cap == 2 (cohort is between 25 and 100) & ccr > 2.0
# set ccr caps to 0 if is.na(ccr) - these are cohorts that were 0 in 2000 and 2010
# set ccr caps to X if is.inf(ccr) - these are cohorts that were 0 in 2000 and > 0 in 2010 ?????? - propose 2
# set ccr caps to X if ccr > 2 regardless of population size - propose 2

t_long_ccr_final <- set_ccr_caps(t_long_ccr)
c_long_ccr_final <- set_ccr_caps(c_long_ccr)

#### 9. Need to do basic analysis of CCRs #### 
# # Number of inf values in total - 2368 out of 2,337,824 (0.01% of all tract-cohort combos)
# t_long_ccr_final %>%
#   summarise(inf_count = sum(is.infinite(ccr_final)))
# 
# # Number of inf values by cohort 
# t_long_ccr_final %>%
#   group_by(var_code) %>%
#   summarise(inf_count = sum(is.infinite(ccr_final))) %>%
#   print(n = Inf)
# 
# # Combos with CCRs greater than 2.0 - 82,002 out of 2,337,824 (3% of all tract-cohort combos)
# t_long_ccr_final %>%
#   summarise(ccr_final_count = sum(ccr_final > 2.0))
# 
# # Tract-cohort combos with CCRs greater than 2.0 
# # There aren't that many in the older age cohorts with CCRs greater than 2.0
# t_long_ccr_final %>%
#   group_by(var_code) %>%
#   summarise(ccr_final_count = sum(ccr_final > 2.0)) %>%
#   print(n = Inf)
# 
# # What are typical pop counts for tract-cohorts with CCR > 2.0
# t_long_ccr_final %>%
#   filter(ccr > 2.0) %>%
#   filter(!is.infinite(ccr)) %>%
#   group_by(var_code) %>%
#   summarise(mean_n = mean(n),
#             mean_ccr = mean(ccr, na.rm = TRUE),
# #            sd_n = sd(n),
#             median_n = median(n),
#             median_ccr = median(ccr, na.rm = TRUE),
# #            min_n = min(n),
# #            max_n = max(n),
#             gt_100 = sum(n > 100)) %>%
#   print(n = Inf)
# 
# # Tract-cohort combos counts in different CCR ranges 
# cohort_ccr_counts <- t_long_ccr_final %>%
#   mutate(ccr_range = case_when(ccr_final < 1.0 ~ 1,
#                                ccr_final >= 1.0 & ccr_final < 2 ~ 2,
#                                ccr_final >= 2.0 & ccr_final < 10 ~ 3,
#                                ccr_final >= 10 & ccr_final < 100 ~ 4,
#                                ccr_final >= 100 ~ 5)) %>%
#   group_by(var_code, ccr_range) %>%
#   summarise(ccr_count = n())
# 
# # factorize ccr_ranges for plotting
# cohort_ccr_counts$ccr_range <- factor(cohort_ccr_counts$ccr_range, labels = c("Less than 1.0", "1.0-1.9", "2.0-9.9", "10.0-99.9", "Greater than 100"))
# 
# # Bar chart of counts by ccr_range category
# ggplot(data = cohort_ccr_counts, aes(x = ccr_range, y = ccr_count)) +
#   geom_bar(stat="identity") + 
#   coord_flip() +
#   facet_wrap(vars(var_code))
# 
# # Histograms of ccr2 by cohort
# ggplot(data = t_long_ccr_final, x = ccr_final) + 
#   geom_histogram() + 
#   facet_wrap(vars(var_code))


#### 10. Apply the CCRs to the 2010 counts to create 2020 estimates ####
# Create a match code in the t_long_ccr_final to join CCRs to 2010 counts
t_long_ccr_final <- t_long_ccr_final %>%
  mutate(var_code = str_remove(var_code, "ccr_")) %>%
  select(-n)

c_long_ccr_final <- c_long_ccr_final %>%
  mutate(var_code = str_remove(var_code, "ccr_")) %>%
  select(-n)

# Keep only 2010 DATAYEAR from T
t_2010 <- t %>%
  filter(DATAYEAR == 2010)

c_2010 <- c %>%
  filter(DATAYEAR == 2010)

# Join t_long_ccr_final to t_2010 on var_code and REMOVE YOUNGER AGES FOR NOW
t_2010 <- t_2010 %>%
  left_join(t_long_ccr_final, by = c("GISJOIN", "var_code")) %>%
  filter(!is.na(ccr_final))

c_2010 <- c_2010 %>%
  left_join(c_long_ccr_final, by = c("GISJOIN", "var_code")) %>%
  filter(!is.na(ccr_final))

# Compute 2020 estimates- multiply n by ccr_final and then create df with fewer vars
t_2010 <- t_2010 %>%
  mutate(n2020 = n * ccr_final) %>%
  select(GISJOIN, var_code, n2010 = n, n2020, -ccr, -ccr_final, -ccr_cap)

c_2010 <- c_2010 %>%
  mutate(n2020 = n * ccr_final) %>%
  select(GISJOIN, var_code, n2010 = n, n2020, -ccr, -ccr_final, -ccr_cap)

# Create long t_2010 df 
t_long_2010 <- t_2010 %>%
  pivot_longer(n2010:n2020, names_to = "year", values_to = "n") %>%
  mutate(DATAYEAR = case_when(year == "n2010" ~ 2010,
                              year == "n2020" ~ 2020)) %>%
  select(-year)

c_long_2010 <- c_2010 %>%
  pivot_longer(n2010:n2020, names_to = "year", values_to = "n") %>%
  mutate(DATAYEAR = case_when(year == "n2010" ~ 2010,
                              year == "n2020" ~ 2020)) %>%
  select(-year)

#### 11. Compute Child-to-Women Ratios 2000-2010 by age/tract #### 
# Child-to-women ratios are computed as follows:
# 
# CTW_male0_4 = Population_2010_male_age0_4 / Population_2010_female_age_20_44
# CTW_female0_4 = Population_2010_female_age0_4 / Population_2010_female_age_20_44
# CTW_male5_9 = Population_2010_male_age5_9 / Population_2010_female_age_30_49
# CTW_female5_9 = Population_2010_female_age5_9 / Population_2010_female_age_30_49
# 
# CTW_age0_4 = Population_2010_age0_4 / Population_2010_female_age_20_44
# CTW_age5_9 = Population_2010_age5_9 / Population_2010_female_age_30_49
# 
# where the numerator is the number of males and females in each age group. We will
# apply the overall CTW to each sex.

t_wide_ctw <- compute_ctw(t_wide_ccr)
c_wide_ctw <- compute_ctw(c_wide_ccr)

#### 12. Create a usable long data frame from the CTWs #### 
t_long_ctw <- create_long_ctw_df(t_wide_ctw)
c_long_ctw <- create_long_ctw_df(c_wide_ctw)

#### 13. Analysis of the CTWs ####
# Number of tract-cohort INF values - 12 out of 146,114
# t_long_ctw %>%
#   summarise(inf_count = sum(is.infinite(ctw)))
# 
# # Number of tract-cohort NaN values - 1222 out of 146,114
# t_long_ctw %>%
#   summarise(nan_count = sum(is.nan(ctw)))

#### 14. Set NaN and Inf values for tract-level CTWs
t_long_ctw <- replace_nan_inf_ctw(t_long_ctw)
c_long_ctw <- replace_nan_inf_ctw(c_long_ctw)

#### 15. Create a match code in the t_long_ctw and c_long_ctw ####
t_long_ctw <- t_long_ctw %>%
   mutate(var_code = str_remove(var_code, "ctw_"))
 
c_long_ctw <- c_long_ctw %>%
   mutate(var_code = str_remove(var_code, "ctw_"))

#### 15. Pull out the female age groups from t_long_2010 and c_long_2010 required for CTW application #### 
# Keep females age 20-44 for age 0-4
# Keep females age 30-49 for ages 5-9
t_females <- t_long_2010 %>%
  filter(DATAYEAR == 2020) %>%
  filter(var_code %in% c("female_age20_24", "female_age25_29", "female_age30_34", "female_age35_39", "female_age40_44", "female_age45_49"))

c_females <- c_long_2010 %>%
  filter(DATAYEAR == 2020) %>%
  filter(var_code %in% c("female_age20_24", "female_age25_29", "female_age30_34", "female_age35_39", "female_age40_44", "female_age45_49"))

# Recode age categories into two groups - 20-44 and 30-49
t_females_20_44 <- create_females_20_44(t_females)
t_females_30_49 <- create_females_30_49(t_females)
c_females_20_44 <- create_females_20_44(c_females)
c_females_30_49 <- create_females_30_49(c_females)

#### 16. Apply CTWs to counts in t_females* and c_females* to get 2020 estimates #### 
# a. Split apart t_long_ctw and c_long_ctw into two dfs each (one for 0-4, one for 5-9)
t_long_ctw_age0_4 <- t_long_ctw %>%
  filter(var_code == "male_age0_4" | var_code == "female_age0_4")

t_long_ctw_age5_9 <- t_long_ctw %>%
  filter(var_code == "male_age5_9" | var_code == "female_age5_9")

c_long_ctw_age0_4 <- c_long_ctw %>%
  filter(var_code == "male_age0_4" | var_code == "female_age0_4")

c_long_ctw_age5_9 <- c_long_ctw %>%
  filter(var_code == "male_age5_9" | var_code == "female_age5_9")

# b. Join the t_females_XX_YY onto CTW rates and multiply ctw * n
# - this yields the 2020 estimate for male_age0_4, male_age5_9, female_age0_4, and female_age5_9
t_long_ctw_age0_4_females_age20_44 <- t_long_ctw_age0_4 %>%
  left_join(t_females_20_44, by = "GISJOIN") %>%
  mutate(n = n * ctw_final,
         DATAYEAR = 2020) %>%
  select(-ctw, -ctw_final)

t_long_ctw_age5_9_females_age30_49 <- t_long_ctw_age5_9 %>%
  left_join(t_females_30_49, by = "GISJOIN") %>%
  mutate(n = n * ctw_final,
         DATAYEAR = 2020) %>%
  select(-ctw, -ctw_final)

c_long_ctw_age0_4_females_age20_44 <- c_long_ctw_age0_4 %>%
  left_join(c_females_20_44, by = "GISJOIN") %>%
  mutate(n = n * ctw_final,
         DATAYEAR = 2020) %>%
  select(-ctw, -ctw_final)

c_long_ctw_age5_9_females_age30_49 <- c_long_ctw_age5_9 %>%
  left_join(c_females_30_49, by = "GISJOIN") %>%
  mutate(n = n * ctw_final,
         DATAYEAR = 2020) %>%
  select(-ctw, -ctw_final)

#### 17. Extract male_age0_4, male_age5_9, female_age0_4, and female_age5_9 for 2010 from t and c dfs ####
# This step extracts 2010 counts of male_age0_4, male_age5_9, female_age0_4, and female_age5_9 from the 
# t and c dfs

t_2010_age04_59 <- t %>%
  filter(DATAYEAR == 2010) %>%
  filter(var_code %in% c("male_age0_4", "male_age5_9", "female_age0_4", "female_age5_9"))

c_2010_age04_59 <- c %>%
  filter(DATAYEAR == 2010) %>%
  filter(var_code %in% c("male_age0_4", "male_age5_9", "female_age0_4", "female_age5_9"))

#### 18. Bind t_2010_long, t_2010_age04_59, t_long_ctw_age0_4_females_age20_44, and t_long_ctw_age5_9_females_age30_49 ####
# This step generates the df with 2010 and 2020 values for each tract-sex-age combination

t_long_2010 <- bind_rows(t_long_2010, t_2010_age04_59)
t_long_2010 <- bind_rows(t_long_2010, t_long_ctw_age0_4_females_age20_44)
t_long_2010 <- bind_rows(t_long_2010, t_long_ctw_age5_9_females_age30_49)

c_long_2010 <- bind_rows(c_long_2010, c_2010_age04_59)
c_long_2010 <- bind_rows(c_long_2010, c_long_ctw_age0_4_females_age20_44)
c_long_2010 <- bind_rows(c_long_2010, c_long_ctw_age5_9_females_age30_49)


#### 19. Run the 2010 data through the prep steps for interpolation #### 
# a. set begin and end values, convert to dates and find number of days between them
begin_date <- ymd("2010-04-01")
end_date <- ymd("2020-04-01")

# b. set the decade interval
decade_interval <- interval(begin_date, end_date)

# c. find number of days in interval 
days <- decade_interval / days(1)

# d. Create appropriate date fields in t
t_long_2010 <- t_long_2010 %>%
  mutate(date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
                              (DATAYEAR == 2000) ~ "2000-04-01",
                              (DATAYEAR == 2010) ~ "2010-04-01",
                              (DATAYEAR == 2020) ~ "2020-04-01")) %>%
  mutate(date1 = ymd(date_str)) 

c_long_2010 <- c_long_2010 %>%
  mutate(date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
                              (DATAYEAR == 2000) ~ "2000-04-01",
                              (DATAYEAR == 2010) ~ "2010-04-01",
                              (DATAYEAR == 2020) ~ "2020-04-01")) %>%
  mutate(date1 = ymd(date_str)) 

# e. Create df containing males by age 
t_male <- t_long_2010 %>%
  filter(str_detect(var_code, "^male"))

# f. Create df containing females by age 
t_female <- t_long_2010 %>%
  filter(str_detect(var_code, "^female"))

# g. Compute difference between 2000 and 2010 data 
t_female <- compute_diff(t_female, d2010, n, var_code, GISJOIN)
t_male <- compute_diff(t_male, d2020, n, var_code, GISJOIN)
c <- compute_diff(c_long_2010, d2020, n, var_code, GISJOIN)

# g. Create a custom span option for adding a specific set of months to df (all April and July months)
time_span <- 
  span_around(begin_date, interval = "month", end_shift = "10 year") %>%
  subset_span(list(mon = c(4, 7)))

# h. Use the pad_cust to add records to t_male and t_female using time_span
t_male <- t_male %>%
  pad_cust(time_span, group = c("GISJOIN", "var_code")) %>%
  filter(date1 == "2010-04-01" | date1 == "2020-04-01" | month(date1) == 7 )

t_female <- t_female %>%
  pad_cust(time_span, group = c("GISJOIN", "var_code")) %>%
  filter(date1 == "2010-04-01" |date1 == "2020-04-01" | month(date1) == 7)

c <- c %>%
  pad_cust(time_span, group = c("GISJOIN", "var_code")) %>%
  filter(date1 == "2010-04-01" |date1 == "2020-04-01" | month(date1) == 7)

# i. compute cumulative days interval
t_female <- compute_cum_days_interval_2010_2019(t_female, date1, cum_days_interval)
t_male <- compute_cum_days_interval_2010_2019(t_male, date1, cum_days_interval)
c <- compute_cum_days_interval_2010_2019(c, date1, cum_days_interval)

# j. fill diff and CL8AA down by group 
t_female <- fill_down(t_female, GISJOIN, var_code, n)
t_male <- fill_down(t_male, GISJOIN, var_code, n)
c <- fill_down(c, GISJOIN, var_code, n)

# k. Compute a linear interpolation of for each intercensal year value. Essentially, this function
# spreads the n_diff out over each year. 
t_female <- linear_interpolation(t_female, n, n_diff, cum_days_interval)
t_male <- linear_interpolation(t_male, n, n_diff, cum_days_interval)
c <- linear_interpolation(c, n, n_diff, cum_days_interval)



