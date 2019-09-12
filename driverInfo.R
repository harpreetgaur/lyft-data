# importing libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(scales)

# plot settings
theme_set(theme_bw())
options(repr.plot.width=4, repr.plot.height=3)

# loading in datasets
driver <- read.csv("driver_ids.csv")
ride <- read.csv("ride_ids.csv")
ride_stamps <- read.csv("ride_timestamps.csv")

# -----------------------------------------------------------
#           D R I V E R   L I F E T I M E   V A L U E   
#                   C A L C U L A T I O N S
# -----------------------------------------------------------

# joining driver and ride datasets by driver_id since we're
# interested in Driver's Lifetime Value
ride_info <- left_join(driver,ride, by = "driver_id")

base_fare <- 2
cost_per_mile <- 1.15
cost_per_min <- 0.22
service_fee <- 1.75


# calculating the revenue gained per trip with the equation:
# ((base_fare + (cost_per_mile * ride_distance_miles) + (cost_per_min * ride_duration_min))) * (1 + prime_decimal) + service_fee)
ride_info_enhanced <- ride_info %>% 
  mutate(ride_duration_min = ride_duration / 60.0) %>%
  mutate(ride_distance_miles = ride_distance / 1609.34) %>%
  mutate(prime_decimal = ride_prime_time / 100) %>%
  mutate(price_per_trip = ((base_fare + (cost_per_mile * ride_distance_miles) + (cost_per_min * ride_duration_min))) * (1 + prime_decimal) + service_fee)

# rides are always at least 5 dollars and are
# capped at 400 dollars
ride_info_enhanced <- ride_info_enhanced %>%
  mutate(price_per_trip_adj = case_when(
    (price_per_trip > 400) ~ 400,
    (price_per_trip < 5) ~ 5,
    TRUE ~ price_per_trip
  ))

# price per trip info
summary(ride_info_enhanced$price_per_trip_adj)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 3.761   8.038  10.568  13.527  15.101 625.005      83 

# grouped by driver and summed up ride values to get
# each rider's revenue
ride_info_drivers <- ride_info_enhanced %>% 
  group_by(driver_id) %>%
  mutate(driver_revenue = sum(price_per_trip_adj)) %>%
  filter(!(is.na(driver_revenue)))
# NOTE: not sure if this is specifically revenue generated
#       for the drivers or Lyft itself...
#       but even if it's just revenue generated for the drivers
#       it should still be proportional to Lyft's revenue

# driver revenue info
# there seems to be a lot of drivers that don't generate much
# revenue at all...
summary(ride_info_drivers$driver_revenue)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 25.69  3334.33  4634.24  4877.33  6339.61 12350.29 
driver_revenue_median = median(ride_info_drivers$driver_revenue)
ride_info_drivers %>%
  summarize(driver_revenue = max(driver_revenue))%>%
  ggplot() +
  geom_histogram(aes(x = driver_revenue),
                 color = "white", 
                 fill = "indianred1",
                 bins = 20) + 
  geom_vline(xintercept = driver_revenue_median, lwd = 1.25,
             linetype = "dashed", color = "blue") +
  labs(title = "Distribution of Driver Lifetime Values") +
  xlab("Driver Lifetime Value (Dollars)") +
  ylab("Count") 

# -----------------------------------------------------------
#           D R I V E R   L I F E T I M E   V A L U E   
#                         F A C T O R S
# -----------------------------------------------------------

# To determin the most important driver lifetime value factors
# a linear regression is being used with the driver lifetime value
# as the response variable and the factors as the response

# NOTE: Due to the nature of some of the factors, data will be
#       grouped by ride, not driver

# factor generation:
  # time driver has been with Lyft
  # requested - accepted time lapse
  # accepted - arrived time lapse
  # picked up - dropped off time lapse
  # requested - dropped off time lapse
  # day/night
  # peak hours (scatterplot with x = time, y = prime rate to determine)
  # weekends
  # holidays
  # seasons
  # weather
    # rain
    # snow
    # temperature

# simple correlation tests in order to eliminate some factors
# with correlation plots

# exponential pdfs for time lapses

# model creation

# k-fold validation to check for RMSE and adjusted RSq
