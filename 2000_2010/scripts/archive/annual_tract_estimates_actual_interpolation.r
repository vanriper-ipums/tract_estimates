# county ratios testing
# 

# join CB population estimates to county linear interpolation (t) on GISJOIN and date1 
c <- left_join(c, c_est, by=c("GISJOIN","date1"))

# compute ratio of county estimate to linear interpolation value 
c <- mutate(c, tot_pop_ratio = TOT_POP / CL8AA)

# keep only needed vars for tract interpolation
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