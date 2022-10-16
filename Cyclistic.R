getwd()
setwd("/Users/Liudmyla.Luzhna/Desktop/Cyclistic bike-share analysis")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)

#Upload datasets
tripdata_202109 = read_csv("202109-divvy-tripdata.csv")
tripdata_202110 = read_csv("202110-divvy-tripdata.csv")
tripdata_202111 = read_csv("202111-divvy-tripdata.csv")
tripdata_202112 = read_csv("202112-divvy-tripdata.csv")
tripdata_202201 = read_csv("202201-divvy-tripdata.csv")
tripdata_202202 = read_csv("202202-divvy-tripdata.csv")
tripdata_202203 = read_csv("202203-divvy-tripdata.csv")
tripdata_202204 = read_csv("202204-divvy-tripdata.csv")
tripdata_202205 = read_csv("202205-divvy-tripdata.csv")
tripdata_202206 = read_csv("202206-divvy-tripdata.csv")
tripdata_202207 = read_csv("202207-divvy-tripdata.csv")
tripdata_202208 = read_csv("202208-divvy-tripdata.csv")

#Compare column names each of the files
colnames(tripdata_202109)
colnames(tripdata_202110)
colnames(tripdata_202111)
colnames(tripdata_202112)
colnames(tripdata_202201)
colnames(tripdata_202202)
colnames(tripdata_202203)
colnames(tripdata_202204)
colnames(tripdata_202205)
colnames(tripdata_202206)
colnames(tripdata_202207)
colnames(tripdata_202208)

#Inspect the dataframes and look for incongruencies
str(tripdata_202109)
str(tripdata_202110)
str(tripdata_202111)
str(tripdata_202112)
str(tripdata_202201)
str(tripdata_202202)
str(tripdata_202203)
str(tripdata_202204)
str(tripdata_202205)
str(tripdata_202206)
str(tripdata_202207)
str(tripdata_202208)

#Combine all the datasets into one single dataframe
all_trips = bind_rows(tripdata_202109, tripdata_202110, tripdata_202111, 
                      tripdata_202112, tripdata_202201, tripdata_202202, 
                      tripdata_202203, tripdata_202204, tripdata_202205,
                      tripdata_202206, tripdata_202207, tripdata_202208)

#Remove columns not required or beyond the scope of project
all_trips = all_trips %>%
  select(-c(start_lat:end_lng))
glimpse(all_trips)

#Add columns that list the date, month, day, and year of each ride
all_trips$date = as.Date(all_trips$started_at)
all_trips$month = format(as.Date(all_trips$date), "%b_%y")
all_trips$day = format(as.Date(all_trips$date), "%d")
all_trips$year = format(as.Date(all_trips$date), "%y")
all_trips$day_of_week = format(as.Date(all_trips$date), "%a")

#Add a ride length calculation to all_trips in seconds
all_trips$ride_length = difftime(all_trips$ended_at, all_trips$started_at)

#Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length = as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#Checking for ride lengths less than 0
nrow(subset(all_trips, ride_length < 0))

#Checking for testrides that were made by company for quality checks
nrow(subset(all_trips, start_station_name %like% "TEST"))
nrow(subset(all_trips, start_station_name %like% "test"))
nrow(subset(all_trips, start_station_name %like% "Test"))

#Remove negative trip durations
all_trips_v2 = all_trips[!(all_trips$ride_length < 0),]

#Remove test rides
all_trips_v2 = all_trips_v2[!(all_trips_v2$start_station_name %like% "Test"),]
glimpse(all_trips_v2)

#Descriptive analysis on ride_length
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

#Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
            all_trips_v2$day_of_week, FUN = mean)

#Fix the order for the day_of_the_week and month variable so that they show up 
#in the same sequence in output tables and visualizations
all_trips_v2$day_of_week = ordered(all_trips_v2$day_of_week, levels=c("Sun","Mon","Tue",
                                                                      "Wed", "Thu","Fri","Sat"))
all_trips_v2$month = ordered(all_trips_v2$month, levels=c("Sep_21","Oct_21","Nov_21","Dec_21",
                                                          "Jan_22","Feb_22","Mar_22","Apr_22",
                                                          "May_22","Jun_22","Jul_22","Aug_22"))
#Run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
            all_trips_v2$day_of_week, FUN = mean)

#Analyze ridership data by type and weekday
all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), average_ride_length = mean(ride_length)) %>%
  arrange(member_casual, desc(number_of_rides))

#Visualization
all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title = "Total trips by customer type Vs. Days of the week") +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Avarage number of trips by customer type and month
unique(all_trips$month)

all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), average_ride_length = mean(ride_length)) %>%
  arrange(member_casual, desc(number_of_rides))

#Visualization
all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title = "Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Visualization of average trip duration by customer type on each day of the week
all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = mean(ride_length)) %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Day of the week")

#Visualization of average trip duration by customer type Vs. month
all_trips_v2 %>%  
  group_by(member_casual, month) %>% 
  summarise(average_ride_length = mean(ride_length)) %>%
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))

#Visualization of ride type Vs. number of trips by customer type
all_trips_v2 %>%
  group_by(rideable_type, member_casual) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= member_casual))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")

#Creating a csv file of the clean data for further analysis and visualizations
clean_data = aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(clean_data, "Clean Data.csv", row.names = F)

























































