library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)

driver <- read.csv("driver_ids.csv")
ride <- read.csv("ride_ids.csv")
ride_stamps <- read.csv("ride_timestamps.csv")

driver%>% View()
ride%>%View()


rideInfo <- left_join(driver,ride, by = "driver_id")

rideInfo%>%View()

driverNumRides <- rideInfo %>% group_by(driver_id) %>% summarize(count())

base_fare <- 2
cost_per_mile <- 1.15
cost_per_min <- 0.22
service_fee <- 1.75



rideInfo2 <- rideInfo %>% 
  mutate(ride_duration_min = ride_duration / 60.0) %>%
  mutate(ride_distance_miles = ride_distance / 1609.34) %>%
  mutate(price_per_trip = base_fare + (cost_per_mile * ride_distance_miles) + (cost_per_min * ride_duration_min) + service_fee)

str(rideInfo2)
summary(rideInfo2$price_per_trip)

# old_price = 0

# adjustment <- function(old_price){
#   new_price = old_price
#   if(old_price > 400.0){
#     new_price = 400.0
#   }
#   else if(old_price < 5.0){
#     new_price = 5.0
#   }
#   return(new_price)
# }

rideInfo3 <- rideInfo2 %>%
  mutate(price_per_trip_adj = case_when(
    (price_per_trip > 400) ~ 400,
    (price_per_trip < 5) ~ 5,
    TRUE ~ price_per_trip
  ))

rideInfo3$price_per_trip_adj %>% summary

lifetimeValue <- rideInfo3 %>%
  group_by(driver_id) %>%
  mutate(lifetime_value = sum(price_per_trip_adj))

lifetimeValue$lifetime_value %>% summary
