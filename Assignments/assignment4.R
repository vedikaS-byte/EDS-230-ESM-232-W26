##AUTHORS: Melannie Moreno and Vedika Shirtekar
# Assignment 4 - Almond Profit Model - Informal Sensitivity Analysis
# In your same groups -
#   
#   Develop a profit model for your almond yield (you can make this up - think about what the parameters would be)
# 
# you might assume a baseline profit and then adjust according to the anomaly
# 
# there are many ways to combine the almond yield and profit functions; you can have the profit function "call"/use the almond yield function; or create a wrapper function that calls them in sequence (first the almond yield and then the profit function)
# 
# Do a simple informal sensitivity analysis of almond yield profit using at least 2 parameters
# 
# Create a single graph of the results - you can decide what is the most meaningful graph
# 
# Submit as a group: pdf of rendered quarto including graph, your R files for almond yield and profit model,
# 
# 
# 
# 


### Read in data
library(tidyverse)
climate <- read.csv(here::here("Data", "clim.txt"), header = T, sep = "", stringsAsFactors = F)


### Data Processing 
# Create two new time series datasets to store the average minimum temperatures and summed precipitation over time
# Group by year prior to summarizing
# Summarize the tmin_c and precip columns as variables
tmin_c <- climate %>% filter(month == 2) %>% group_by(year) %>% summarize(mean_temp = mean(tmin_c, na.rm = T))
precip <- climate %>% filter(month == 1) %>% group_by(year) %>% summarize(sum_precip = sum(precip, na.rm = T))


## Construct the function
# This function takes two kinds of inputs: 
#       1. Two climate time series
#      2. A list of the specific values for each climate paramater (Lobell et al., 2006)
almond_yield <- function(tmin_c, precip, tmin_param = c(.015, .0046), precip_param = c(.07, .0043)){
  
  # Create a merged data frame such that the minimum temperature and summed precip are together
  yields <- merge(tmin_c, precip, by = "year")    
  
  # Create a new column called "yield" that takes the subset of each parameter based on Lobell's equation and
  # the subsetted climate variable in the data frame "yeilds"
  yields$yield <- (-tmin_param[1] * yields$mean_temp) - 
    (tmin_param[2] * (yields$mean_temp)^2) - 
    (precip_param[1] * yields$sum_precip) + 
    (precip_param[2] * (yields$sum_precip)^2) + 0.28
  
  # Print results
  print(paste("The maximum almond yield  is", round(max(yeilds$yield),2), "ton/acre."))
  print(paste("The minimum almond yield is", round(min(yeilds$yield),2), "ton/acre."))
  print(paste("The mean almond yield is", round(mean(yeilds$yield),2), "ton/acre."))
  
}

### Call the function based on assigned arguments for the climate time series (parameters are auto-assigned)
almond_yield(tmin_c = tmin_c, precip = precip)

## RESULTS: 
# The maximum almond yield  is 1919.98 ton/acre.
# The minimum almond yield is -0.36 ton/acre.
# The mean almond yield is 181.45 ton/acre.
