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

ride_info_drivers <- ride_info_drivers %>%
  group_by(driver_id) %>%
  mutate(num_rides = n())

cdf <- ride_info_drivers %>%
  select(driver_id, driver_revenue, num_rides) %>%
  group_by(driver_id) %>%
  summarize(driver_revenue = max(driver_revenue),
            num_rides = max(num_rides)) %>%
  arrange(desc(num_rides)) %>%
  unique() %>%
  ungroup() %>%
  mutate(rank = rank(desc(num_rides)),
         cdf = cumsum(driver_revenue) / sum(driver_revenue)
         )
# a majority of the accumulative driver lifetime value is 
# earned by drivers who made 500 rides and less...
# The decrease in the slope also suggests that past this point
# it's economically more inefficient to encourage drivers to 
# make rides
cdf %>%
  ggplot(aes(x = rank, y = cdf)) +
  geom_line() +
  scale_y_continuous(label = percent) +
  ylab('Cumulative Percent of Driver') +
  xlab('Driver Rank by Number of Rides With Lyft') +
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
  
  # on board dates
    
    onboard_data <- ride_info_drivers %>%
      group_by(driver_id) %>%
      summarize(onboard_month = month(max(driver_onboard_date2)),
                onboard_day = day(max(driver_onboard_date2)))
    
  # average ride stats
    ride_stats <- ride_info_drivers %>%
      group_by(driver_id) %>%
      summarize(total_distance = sum(ride_distance_miles),
                total_duration = sum(ride_duration_min),
                average_distance = mean(ride_distance_miles),
                average_duration = mean(ride_duration_min),
                )
    
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
    driver_revenue <- left_join(driver_revenue, weekends_weekdays_ratio)
    driver_revenue <- left_join(driver_revenue, onboard_data)
    driver_revenue <- left_join(driver_revenue, ride_stats)
    
    driver_revenue <- driver_revenue %>%
      mutate(normalized_average_distance = average_distance / total_distance,
             normalized_average_duration = average_duration / total_duration)
    
  
### Data Visualization + Statistical Tests (Parametric & Non-Parametric)

# simple correlation tests in order to eliminate some factors
# r - linear correlation (+1 is a strong positive relationship,
#                         -1 is a strong negative relationship,
#                         0 means no relationship)
# R^2 - how much of the variation of the dependent variable
#       can be attributed to the independent variable

# days_with_lyft
# num_rides
# requested
# accepted
# arrived
# picked_up
# dropped_off
# requested_arrived
# accepted_arrived
# requested_dropped_off
# weekend:week ratio
# onboard_month
# onboard_day
# average_distance
# average_duration
# total_distance
# total_duration
# normalized average distance
# normalized average duration

# graph their r^sq values as a line with the size of the points
# scaling with reverse p-value

# removing NAs
driver_revenue2 <- na.omit(driver_revenue)

rsq <- rep(0,19)
p_values <- rep(0,19)

# Tried a for loop but loading vectors from lists are a little tricky
# (loading vectors from a vector of strings while converting the strings
# to their respective object names is a possible solution...)

possible_features <- c("Days With Lyft",
                       "Number of Rides",
                       "Average Requested Time",
                       "Average Accepted Time",
                       "Average Arrived Time",
                       "Average Picked Up Time",
                       "Average Dropped Off Time",
                       "Average Requested-Arrived Timelapse",
                       "Average Accepted-Arrived Timelapse",
                       "Average Requested-Dropped Off Timelapse",
                       "Weekend:Weekday Ratio",
                       "Onboard Month",
                       "Onboard Day",
                       "Total Distance",
                       "Total Duration",
                       "Mean Distance",
                       "Mean Duration",
                       "Normalized Mean Distance",
                       "Normalized Mean Duration")

rsq[1] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$days_with_lyft)))^2
rsq[2] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$num_rides)))^2
rsq[3] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$requested_mean)))^2
rsq[4] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$accepted_mean)))^2
rsq[5] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$arrived_mean)))^2
rsq[6] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$picked_up_mean)))^2
rsq[7] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$dropped_off_mean)))^2
rsq[8] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$requested_arrived)))^2
rsq[9] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$accepted_arrived)))^2
rsq[10] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$requested_dropped_off)))^2
rsq[11] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$weekends_weekdays_ratio)))^2
rsq[12] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$onboard_month)))^2
rsq[13] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$onboard_day)))^2
rsq[14] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$total_distance)))^2
rsq[15] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$total_duration)))^2
rsq[16] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$average_distance)))^2
rsq[17] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$average_duration)))^2
rsq[18] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$normalized_average_distance)))^2
rsq[19] <- (cor(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$normalized_average_duration)))^2


p_values[1] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$days_with_lyft))$p.value
p_values[2] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$num_rides))$p.value
p_values[3] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$requested_mean))$p.value
p_values[4] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$accepted_mean))$p.value
p_values[5] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$arrived_mean))$p.value
p_values[6] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$picked_up_mean))$p.value
p_values[7] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$dropped_off_mean))$p.value
p_values[8] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$requested_arrived))$p.value
p_values[9] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$accepted_arrived))$p.value
p_values[10] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$requested_dropped_off))$p.value
p_values[11] <- cor.test(driver_revenue2$driver_revenue, as.numeric(driver_revenue2$weekends_weekdays_ratio))$p.value

cor_dfr <- data_frame(possible_features, rsq)#, p_values)

cor_dfr %>%
  ggplot() +
  geom_col(aes(x = possible_features, y = rsq, fill = possible_features)) +
  coord_flip() +
  ylab("R-Squared Values") +
  xlab("Possible Features") +
  labs(title = "Two-Sided Correlation Tests") +
  theme(legend.position = "none")

# The only Rsq Value that's promising is the Rsq Value for the number
# of rides
# ONE CONCLUSION: LIFETIME DRIVER VALUE IS NOT RELATED TO
# THE SPEED OF THE DRIVER

# LIFETIME DRIVER VALUE IS ALSO BARELY RELATED TO HOW LONG THE 
# DRIVER HAS BEEN WITH LYFT AND HOW MUCH HE/SHE DECIDES TO
# DRIVE ON WEEKENDS COMPARED TO WEEKDAYS

# correlation plot

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

driver_revenue %>%
  mutate(days_with_lyft = as.numeric(days_with_lyft)) %>%
  mutate(days_with_lyft_group = case_when((days_with_lyft < 20) ~ "0 - 20",
                                     (days_with_lyft < 40) ~ "20 - 40",
                                     (days_with_lyft < 60) ~ "40 - 60",
                                     (days_with_lyft < 80) ~ "60 - 80",
                                     TRUE ~ "80+"
  )) %>%
  mutate(days_with_lyft_group = as.factor(days_with_lyft_group)) %>%
  ggplot(aes(x = num_rides, y = driver_revenue)) +
  geom_point(aes(color = days_with_lyft_group)) +
  ylab("Driver Revenue, Dollars") + 
  xlab("Number of Rides") +
  geom_smooth(aes(x = num_rides, y = driver_revenue), group = 1, alpha = 0.25, color = "red") + 
  scale_y_continuous(labels = comma) +
  labs(color = "Days With Lyft")


# (for month, day/night one-way ANOVA with Tukey HSD)
revenue <- driver_revenue$driver_revenue
top_month <- as.factor(as.integer(driver_revenue$top_month))
day_night <- as.factor(driver_revenue$day_night)

anova(lm(revenue ~ top_month)) 
# significant p-value reject the null
# means are statistically different

# Analysis of Variance Table
# 
# Response: revenue
# Df     Sum Sq    Mean Sq F value    Pr(>F)    
# top_month   2 1.1894e+10 5947090533  45.011 < 2.2e-16 ***
#   Residuals 834 1.1019e+11  132125327                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(lm(revenue ~ top_month)) # LOW R-SQUARED = LOW EFFECT SIZE... 

TukeyHSD(aov(lm(revenue ~ top_month))) # multiple comparisions test
# all p-values are significant, reject the null

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = lm(revenue ~ top_month))
# 
# $top_month
# diff       lwr       upr     p adj
# 4-3  9024.513  3693.787 14355.238 0.0002257
# 5-3 15261.708 10022.219 20501.198 0.0000000
# 5-4  6237.196  4276.770  8197.621 0.0000000

anova(lm(revenue ~ day_night)) # significant p-value
summary(lm(revenue ~ day_night)) # Low effect size
TukeyHSD(aov(lm(revenue ~ day_night)))
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = lm(revenue ~ day_night))
# 
# $day_night
# diff        lwr       upr     p adj
# evening-afternoon  -6983.2703 -11429.143 -2537.397 0.0003350
# morning-afternoon   -947.1945  -3831.179  1936.790 0.8326758
# night-afternoon   -13092.4205 -43956.873 17772.032 0.6945641
# morning-evening     6036.0758   1022.705 11049.446 0.0107793
# night-evening      -6109.1502 -37244.836 25026.536 0.9578750
# night-morning     -12145.2260 -43096.519 18806.067 0.7435521
driver_revenue2 %>%
  ggplot() + 
  geom_boxplot(aes(x = as.factor(as.integer(top_month)), y = driver_revenue, 
                   fill = as.factor(as.integer(top_month)))) +
  xlab("Top Month") +
  ylab("Driver Revenue") + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

driver_revenue2 %>%
  ggplot() + 
  geom_boxplot(aes(x = as.factor(day_night), y = driver_revenue,
                   fill = as.factor(day_night)))

#Majority of all rides are in the afternoon
driver_revenue2 %>%
  ggplot() + 
  geom_point(aes(x = num_rides, y = driver_revenue,
                 color = as.factor(day_night)))

driver_revenue2 %>%
  filter(day_night != "night") %>%
  ggplot() + 
  geom_bar(aes(x = day_night, fill = day_night), alpha = 0.5)


# exponential pdfs for time lapses

# model creation

# k-fold validation to check for RMSE and adjusted RSq
