# tract-estimates
Code that generates annual tract-level population estimates from 1990-2010

### Overview
This repo contains the code required to compute annual tract-level population estimates from 1990-2010. The estimates are for 2010 census tracts, and are based on a variety of input data sources.

Output from these scripts are CSV files containing annual estimates of total population, population by sex, population by age (19 bins - under 1, 1-4, 5-9....80-84, 85+), and population by sex by age (same 19 age bins as the population by age). We adjust the annual tract estimates so they sum to the county population estimates published by the Census Bureau.

### Scripts
There are four scripts required to generate annual tract-level population estimates from 1990-2000. The four scripts include:

* _tract_estimates_male_female_by_age_1990_2000.r_
  - this script creates the annual tract estimates from 1990-2000
* _tract_estimates_male_female_by_age_2000_2010.r_
  - this script creates the annual tract estimates from 2000-2010
* _create_final_1990_2010_tract_estimate_data_files.r_
  - this script combines the output from _tract_estimates_male_female_by_age_1990_2000.r_ and _tract_estimates_male_female_by_age_2000_2010.r_, creates the age, sex, and total population data, and writes final data out to CSVs for dissemination
* tidy_funs_tract_county_estimates.r
  - this script contains functions used by _tract_estimates_male_female_by_age_1990_2000.r_ and _tract_estimates_male_female_by_age_2000_2010.r_ to create the annual estimates

### Software requirements
I wrote the code using RStudio 1.2.5019 ("Elderflower") and R version 3.6.1 (2019-07-05). I make use of the following R packages

* tidyverse
* padr 
* lubridate
* here
  - although the code doesn't actually make use of here yet
