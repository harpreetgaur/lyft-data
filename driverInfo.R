# importing libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(scales)
library(lubridate)

# plot settings
theme_set(theme_bw())
options(repr.plot.width=4, repr.plot.height=3)

# loading in datasets
driver <- read.csv("driver_ids.csv")
ride <- read.csv("ride_ids.csv")
ride_stamps <- read.csv("ride_timestamps.csv")

# converting driver onboard dates from factors to dates
driver <- driver %>%
  mutate(driver_onboard_date2 = as.Date(as.character(driver_onboard_date)))

# converting ride timestamps from factors to dates
ride_stamps <- ride_stamps %>%
  mutate(timestamp2 = as.Date(as.character(timestamp)))

# converting ride timestamps from factors to date-times
ride_stamps <- ride_stamps %>%
  mutate(timestamp3 = ymd_hms(as.character(timestamp)))

# mean times prep, hms::as.hms doesn't account for timezones,
# leading to a loss of 4 hours, this is a preemptive measure
four_hours <- 60 * 60 * 4 # four hours in seconds
ride_stamps <- ride_stamps %>%
  mutate(timestamp4 = timestamp3 + four_hours) %>%
  mutate(times = hms::as.hms(timestamp4))

# -----------------------------------------------------------
#           D R I V E R   L I F E T I M E   V A L U E   
#                   C A L C U L A T I O N S
# -----------------------------------------------------------

# joining driver and ride datasets by driver_id since we're
# interested in Driver's Lifetime Value
ride_info <- left_join(driver,ride, by = "driver_id")
ride_info <- left_join(ride_info,ride_stamps, by = "ride_id")

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
  filter(!(is.na(driver_revenue))) %>%
  ungroup()
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
  group_by(driver_id) %>%
  summarize(driver_revenue = max(driver_revenue))%>%
  ggplot() +
  geom_histogram(aes(x = driver_revenue),
                 color = "white", 
                 fill = "indianred1",
                 bins = 20) + 
  geom_vline(xintercept = driver_revenue_median, lwd = 1.25,
             linetype = "dashed", color = "blue") +
  xlab("Driver Lifetime Value (Dollars)") +
  ylab("Count") 

# driver revenue cdf
# calculate the amount of time each driver has been with lyft
ride_info_drivers <- ride_info_drivers %>%
  group_by(driver_id) %>%
  mutate(driver_onboard_date2 = max(driver_onboard_date2), 
         last_ride_date = max(timestamp2)) %>%
  mutate(days_with_lyft = last_ride_date - driver_onboard_date2) %>%
  ungroup()

cdf <- ride_info_drivers %>%
  select(driver_id, driver_revenue, days_with_lyft) %>%
  group_by(driver_id) %>%
  summarize(driver_revenue = max(driver_revenue),
            days_with_lyft = max(days_with_lyft)) %>%
  arrange(desc(days_with_lyft)) %>%
  unique() %>%
  ungroup() %>%
  mutate(rank = rank(desc(days_with_lyft)),
         cdf = cumsum(driver_revenue) / sum(driver_revenue)
         )

cdf %>%
  ggplot(aes(x = rank, y = cdf)) +
  geom_line() +
  scale_y_continuous(label = percent) +
  ylab('Cumulative Percent of Driver') +
  xlab('Driver Rank by Days With Lyft') +
  ggtitle('Cumulative Driver Lifetime Value')

# -----------------------------------------------------------
#           D R I V E R   L I F E T I M E   V A L U E   
#                         F A C T O R S
# -----------------------------------------------------------

# To determine the most important driver lifetime value factors
# a linear regression is being used with the driver lifetime value
# as the response variable

### Data Cleaning + Data Prep

# factor generation:
  # time driver has been with Lyft
  days_with_lyft <- ride_info_drivers %>%
    select(driver_id, days_with_lyft) %>%
    group_by(driver_id) %>%
    summarize(days_with_lyft = max(days_with_lyft)) %>%
    ungroup()
  
    #histogram
    ride_info_drivers %>%
      group_by(driver_id) %>%
      summarize(days_with_lyft = max(days_with_lyft)) %>%
      mutate(days_with_lyft = as.numeric(days_with_lyft)) %>%
      ungroup() %>%
      ggplot() +
      geom_histogram(aes(x = days_with_lyft),
                     color = "white",
                     fill = "indianred1"
                      ) + 
      labs(title = "Distribution of Drivers' Days with Lyft") + 
      xlab("Days With Lyft") + 
      ylab("Count")
    
  # number of rides
    num_rides <- ride_info_drivers %>%
      group_by(driver_id) %>%
      distinct(ride_id) %>%
      summarize(num_rides = n())
    
  # mean requested times
    requested <- ride_info_drivers %>%
      select(driver_id, event, times) %>%
      filter(event == "requested_at") %>%
      group_by(driver_id) %>%
      summarize(requested_mean = mean(times)) %>%
      ungroup()
    
  # mean accepted times
    accepted <- ride_info_drivers %>%
      select(driver_id, event, times) %>%
      filter(event == "accepted_at") %>%
      group_by(driver_id) %>%
      summarize(accepted_mean = mean(times)) %>%
      ungroup()
    
  # mean arrived times
    arrived <- ride_info_drivers %>%
      select(driver_id, event, times) %>%
      filter(event == "arrived_at") %>%
      group_by(driver_id) %>%
      summarize(arrived_mean = mean(times)) %>%
      ungroup()
  
  # mean picked up times
    picked_up <- ride_info_drivers %>%
      select(driver_id, event, times) %>%
      filter(event == "picked_up_at") %>%
      group_by(driver_id) %>%
      summarize(picked_up_mean = mean(times))
  
  # mean dropped off times
    dropped_off <- ride_info_drivers %>%
      select(driver_id, event, times) %>%
      filter(event == "dropped_off_at") %>%
      group_by(driver_id) %>%
      summarize(dropped_off_mean = mean(times)) %>%
      ungroup()
    
  # requested-arrived time lapse
    requested_arrived <- full_join(requested, arrived, by = "driver_id")
    
    requested_arrived <- requested_arrived %>%
      mutate(requested_arrived = arrived_mean - requested_mean) %>%
      select(driver_id, requested_arrived) 
    
  # accepted - arrived time lapse
    
    accepted_arrived <- full_join(accepted, arrived, by = "driver_id")
    
    accepted_arrived <- accepted_arrived %>%
      mutate(accepted_arrived = arrived_mean - accepted_mean) %>%
      select(driver_id, accepted_arrived) 
    
  # requested - dropped off time lapse
    # ride duration is only based on the arrived - dropped off
    requested_dropped_off <- full_join(requested, dropped_off, by = "driver_id")
    
    requested_dropped_off <- requested_dropped_off %>%
      mutate(requested_dropped_off = dropped_off_mean - requested_mean) %>%
      select(driver_id, requested_dropped_off) 

  # mean months   
  month <- ride_info_drivers %>%
    filter(event == "accepted_at") %>%
    select(driver_id, timestamp3) %>%
    mutate(month = month(timestamp3)) %>%
    group_by(driver_id) %>%
    summarize(top_month = mean(month)) %>%
    ungroup()
    
  # day/night
    day_night <- ride_info_drivers %>%
      filter(event == "accepted_at") %>%
      select(driver_id, timestamp3) %>%
      mutate(hour = hour(timestamp3)) %>%
      group_by(driver_id) %>%
      summarize(hour = mean(hour)) %>%
      ungroup() %>%
      mutate(day_night = case_when((hour >= 6 & hour < 12) ~ "morning",
                                   (hour >= 12 & hour < 18) ~ "afternoon",
                                   (hour < 6 ) ~ "night",
                                   TRUE ~ "evening"
                                   )) %>%
      mutate(day_night = as.factor(day_night)) %>%
      select(driver_id, day_night)
    
  # weekend:weekdays ratio
    
    weekends <- ride_info_drivers %>%
      filter(event == "accepted_at") %>%
      select(driver_id, timestamp3) %>%
      mutate(day = day(timestamp3)) %>%
      mutate(weekend = case_when((day == 6 | day == 7) ~ "weekend",
                                    TRUE ~ "weekday"
      )) %>%
      filter(weekend == "weekend") %>%
      group_by(driver_id) %>%
      summarize(weekend_count = n()) %>%
      ungroup()
    
    weekdays <- ride_info_drivers %>%
      filter(event == "accepted_at") %>%
      select(driver_id, timestamp3) %>%
      mutate(day = day(timestamp3)) %>%
      mutate(weekend = case_when((day == 6 | day == 7) ~ "weekend",
                                 TRUE ~ "weekday"
      )) %>%
      filter(weekend == "weekday") %>%
      group_by(driver_id) %>%
      summarize(weekday_count = n()) %>%
      ungroup()
    
    weekends_weekdays_ratio <- full_join(weekends, weekdays, by = "driver_id") 
    
    weekends_weekdays_ratio <- weekends_weekdays_ratio %>% 
      mutate(weekends_weekdays_ratio = weekend_count / weekday_count)

# driver factors dataset
    driver_revenue <- ride_info_drivers %>%
      group_by(driver_id) %>%
      summarize(driver_revenue = max(driver_revenue))
    
    driver_revenue <- left_join(driver_revenue, days_with_lyft)
    driver_revenue <- left_join(driver_revenue, num_rides)
    driver_revenue <- left_join(driver_revenue, requested)
    driver_revenue <- left_join(driver_revenue, accepted)
    driver_revenue <- left_join(driver_revenue, arrived)
    driver_revenue <- left_join(driver_revenue, picked_up)
    driver_revenue <- left_join(driver_revenue, dropped_off)
    driver_revenue <- left_join(driver_revenue, requested_arrived)
    driver_revenue <- left_join(driver_revenue, accepted_arrived)
    driver_revenue <- left_join(driver_revenue, requested_dropped_off)
    driver_revenue <- left_join(driver_revenue, month)
    driver_revenue <- left_join(driver_revenue, day_night)
    driver_revenue <- left_join(driver_revenue, weekends) 

### Data Visualization + Statistical Tests (Parametric & Non-Parametric)
driver_revenue %>%
  mutate(num_rides_group = case_when((num_rides < 200) ~ "0 - 200",
                                     (num_rides < 400) ~ "200 - 400",
                                     (num_rides < 600) ~ "400 - 600",
                                     (num_rides < 800) ~ "600 - 800",
                                     TRUE ~ "800+"
  )) %>%
  mutate(num_rides_group = as.factor(num_rides_group)) %>%
  ggplot(aes(x = days_with_lyft, y = driver_revenue)) +
  geom_point(aes(color = num_rides_group)) +
  ylab("Driver Revenue, Dollars") + 
  xlab("Days with Lyft") +
  geom_smooth(aes(x = days_with_lyft, y = driver_revenue), group = 1, alpha = 0.25) + 
  scale_y_continuous(labels = comma) +
  labs(color = "Number of Rides")
# simple correlation tests in order to eliminate some factors
# with correlation plots
# (for month, day/night one-way ANOVA with Tukey HSD)
    
    

# exponential pdfs for time lapses

# model creation

# k-fold validation to check for RMSE and adjusted RSq
