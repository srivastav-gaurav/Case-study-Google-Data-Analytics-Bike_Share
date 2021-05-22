#Loading necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

#loading data files
X2020_04 <- read.csv("2020_04/csv/202004-divvy-tripdata.csv")
X2020_05 <- read.csv("2020_05/csv/202005-divvy-tripdata.csv")
X2020_06 <- read.csv("2020_06/csv/202006-divvy-tripdata.csv")
X2020_07 <- read.csv("2020_07/csv/202007-divvy-tripdata.csv")
X2020_08 <- read.csv("2020_08/csv/202008-divvy-tripdata.csv")
X2020_09 <- read.csv("2020_09/csv/202009-divvy-tripdata.csv")
X2020_10 <- read.csv("2020_10/csv/202010-divvy-tripdata.csv")
X2020_11 <- read.csv("2020_11/csv/202011-divvy-tripdata.csv")
X2020_12 <- read.csv("2020_12/csv/202012-divvy-tripdata.csv")
X2021_01 <- read.csv("2021_01/csv/202101-divvy-tripdata.csv")
X2021_02 <- read.csv("2021_02/csv/202102-divvy-tripdata.csv")
X2021_03 <- read.csv("2021_03/csv/202103-divvy-tripdata.csv")

#Checking consistency of colnames
colnames(X2020_03) == colnames(X2020_04)
colnames(X2020_03) == colnames(X2020_05)
colnames(X2020_03) == colnames(X2020_06)
colnames(X2020_03) == colnames(X2020_07)
colnames(X2020_03) == colnames(X2020_08)
colnames(X2020_03) == colnames(X2020_09)
colnames(X2020_03) == colnames(X2020_10)
colnames(X2020_03) == colnames(X2020_11)
colnames(X2020_03) == colnames(X2020_12)
colnames(X2020_03) == colnames(X2021_01)
colnames(X2020_03) == colnames(X2021_02)
colnames(X2020_03) == colnames(X2021_03)

#Inspecting dataframes for any incongruency
str(X2020_03)
str(X2020_04)
str(X2020_05)
str(X2020_06)
str(X2020_07)
str(X2020_08)
str(X2020_09)
str(X2020_10)
str(X2020_11)
str(X2020_12)
str(X2021_01)
str(X2021_02)
str(X2021_03)

#Converting data type of start_station_id and end_station_id from char to int
#in X2020_12, X2021_01, X2021_02, X2021_03
X2020_12 <- mutate(X2020_12, 
                   start_station_id = as.integer(start_station_id), 
                   end_station_id = as.integer(end_station_id))
X2021_03 <- mutate(X2021_03, 
                   start_station_id = as.integer(start_station_id), 
                   end_station_id = as.integer(end_station_id))
X2021_02 <- mutate(X2021_02, 
                   start_station_id = as.integer(start_station_id), 
                   end_station_id = as.integer(end_station_id))
X2021_01 <- mutate(X2021_01, 
                   start_station_id = as.integer(start_station_id), 
                   end_station_id = as.integer(end_station_id))


#Combining all 12 months of data from Mar 2020 to Mar 2021
all_trips <- bind_rows(X2020_03, X2020_04, X2020_05, X2020_06, X2020_07, 
                       X2020_08, X2020_09, X2020_10, X2020_11, X2020_12,
                       X2021_01, X2021_02, X2021_03)

#Removing unneccesary columns from all trips
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, 
                              end_lng))


#Inspecting new data frame we created
str(all_trips)
colnames(all_trips)
dim(all_trips)
nrow(all_trips)
head(all_trips)
summary(all_trips)

#Making indiviual coloumns for date, day, month, year of each trip
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), '%Y')
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#Adding ride_length column
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

#inspecting structure of columns
str(all_trips)

#Converting ride_length from factor to numeric to perform analyis
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))


#Removing bad data. Many obsrevation has negative ride length or rides ended at quality
#station
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

##Conducting descriptive analysis
summary(all_trips_v2$ride_length)

#Compare member and casual rides
aggregate(all_trips_v2["ride_length"], by= all_trips_v2["member_casual"], FUN = "mean")
aggregate(all_trips_v2["ride_length"], by= all_trips_v2["member_casual"], FUN = "median")
aggregate(all_trips_v2["ride_length"], by= all_trips_v2["member_casual"], FUN = "min")
aggregate(all_trips_v2["ride_length"], by= all_trips_v2["member_casual"], FUN = "max")

#Comparing average ride length of each day of the week of ench user type
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

#days_of week are out of order. Fixing that!
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Comparing average ride length of each day of the week of each user type
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

#Analyzing ridership data by type and weekday
all_trips_v3 <- all_trips_v2 %>%  mutate(weekday = wday(started_at, label=TRUE)) %>% #creates weekday field using wday
  group_by(member_casual, weekday) %>%   #groups by user-type and weekday
  summarise(number_of_rides = n(), avearge_duration = mean(ride_length)) %>%   #cal number of rides and average duration
  arrange(member_casual, weekday)   #sorts

#Visualize of average duration
all_trips_v2 %>%  mutate(weekday = wday(started_at, label=TRUE)) %>% #creates weekday field using wday
  group_by(member_casual, weekday) %>%   #groups by user-type and weekday
  summarise(number_of_rides = n(), avearge_duration = mean(ride_length)) %>%   #cal number of rides and average duration
  arrange(member_casual, weekday) %>% #sorts
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#Exporting summary file for further analysis
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
                      all_trips_v2$day_of_week, FUN = mean)

write.csv(counts, file = "C:/Users/GAURAV SRIVASTAVA/MyProjects/my_git_masters/Google Data Analytics/Case Study 1/avg_ride_length.csv")

#Exporting all trips necessary version for further analysis 
write.csv(all_trips_v3, "C:/Users/GAURAV SRIVASTAVA/MyProjects/my_git_masters/Google Data Analytics/Case Study 1/all_trips_v3.csv")

#Analyzing casual riders trip duration pattern with members (mean)
ride_length_mean <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
colnames(ride_length_mean) <- c("ride_length", "member_casual")

#Analyzing casual riders trip duration pattern with members (median)
ride_length_median <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
colnames(ride_length_median) <- c("ride_length", "member_casual")


#Counting casual member rides where mean of member trip_duration is smaller
all_trips_v2 %>% filter(member_casual == 'casual', ride_length>= ride_length_mean$member_casual[2]) %>%
  summarise(number_of_riders = n())     #output : 932068

#Counting casual member rides where median of member trip_duration is smaller
all_trips_v2 %>% filter(member_casual == 'casual', ride_length>= ride_length_median$member_casual[2]) %>%
  summarise(number_of_riders = n())     #output : 1136671