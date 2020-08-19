# 03_annual_tract_estimates_interpolation_step.r
# David Van Riper
# 2018-11-26
# 
# This is the third script that actually creates the annual estimate for each tract. It first 
# processes the county data (county estimates from Census Bureau and linearly interpolated county 
# estimates). It joins the two data frames together on GISJOIN and date1, and then computes the 
# ratio between variables from each df. 
# 
# It then reduces the final county df down to the needed variables. 
# 
# Next, the script adds the county GISJOIN to each record in the tract df. It then joins the 
# county df to the tract df on the county GISJOIN and date1. 
# 
# FINALLY, the script multiplies the linearly interpolated variable by the county ratio to yield 
# the adjusted tract value. This final step would be run multiple times if we were processing 
# multiple variables. 


# join CB population estimates to county linear interpolation (t) on GISJOIN and date1 
c <- left_join(c, c_est, by=c("GISJOIN","date1"))

# compute ratio of county estimate to linear interpolation value 
# this step would need to be run more times if we had more than 1 variable we were estimating
c <- mutate(c, tot_pop_ratio = TOT_POP / CL8AA)

# keep only needed vars for tract interpolation
# this step would need to keep more variables if we're estimating more than 1 variable
c <- select(c, GISJOIN, CL8AA, date1, TOT_POP, tot_pop_ratio)


#create a GISJOIN_CTY on tract data
t <- mutate(t, GISJOIN_CTY = substr(GISJOIN, 1, 8))

# join county data to tract for final processing 
t <- left_join(t, c, by=c("GISJOIN_CTY" = "GISJOIN", "date1"="date1"))

# multiply tract linear interpolation estimate by value of county ratio 
# - this gives me a tract estimate adjusted by the ratio of the county estimate to the linearly
# interpolated county value
t <- mutate(t, CL8AA_est = CL8AA.x * tot_pop_ratio)
 
# this line of code compute county totals to see whether we lost or gained any persons during
# the interpolation process
#county_sum <- fun_t %>% group_by(GISJOIN_CTY, date1) %>% summarise(sum(CL8AA_v2))