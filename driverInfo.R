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
