#install needed packagess
install.packages("tidyverse")
install.packages("geosphere")
install.packages("lubridate")
install.packages("dplyr")

#load the libraries
library(tidyverse)
library(geosphere)
library(lubridate)
library(dplyr)

#extract data 
file_location <- "C:\\DATA\\Coursera - Google Data Analytics\\Case Study 1\\Dataset Analyze"
folder_list <- list.files(file_location , full.names = TRUE, pattern = "*.csv")
original_df <- map_df(folder_list, ~read_csv(.))


#adding new column to the dataframe (ride duration, month of the year, day of the week, hour of day)
moded_df <- original_df %>%
  mutate(ride_duration = as.numeric(difftime(ended_at, started_at, unit="mins"))) %>% 
  mutate(ride_distance = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat))) %>%
  mutate(ride_year = year(started_at)) %>%
  mutate(ride_month = month(started_at, label = TRUE)) %>%
  mutate(day_of_week = weekdays(started_at)) %>%
  mutate(hour_of_day = hour(started_at))

#inspect data
colnames(moded_df) #list of column names
nrow(moded_df) #how many rows
dim(moded_df) #dimensions of the dataframe
str(moded_df) #list of cloumns and data types
summary(moded_df) #statistical summary

#Data Cleaning
#filter rides not starting from a station and rides not ending at a station
#filter rides with duration and distance of zero and minus
clean_data <- moded_df %>%
  filter(!is.na(start_station_name)) %>% 
  filter(!is.na(end_station_name)) %>% 
  filter(ride_duration > 0) %>%
  filter(ride_distance > 0) 

#Data Analysis
#membership type, month, day of week aggregated by average and max duration and distance
pivot_tbl <- clean_data %>%
  group_by(member_casual, rideable_type, ride_year, ride_month, day_of_week, hour_of_day) %>%
  summarise(number_of_rides = n(), avg_ride_duration = mean(ride_duration), avg_ride_distance = mean(ride_distance))

#write file to path
write_csv(pivot_tbl, "C:\\DATA\\Coursera - Google Data Analytics\\Case Study 1\\cyclistic_analysis_result.csv", append = FALSE)

view(pivot_tbl)

sum(is.na(pivot_tbl))