library(tidyverse)
library(ggplot2)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(datasauRus)
library(lubridate)
library(readr)
library(readxl)
library(data.table)
library(tidyr)
library(metR)
month_1 <- read_csv("march2022-divvy-tripdata.csv")
month_2 <- read_csv("april2022-divvy-tripdata.csv")
month_3 <- read_csv("may2022-divvy-tripdata.csv")
month_4 <- read_csv("june2022-divvy-tripdata.csv")
month_5 <- read_csv("july2022-divvy-tripdata.csv")
month_6 <- read_csv("aug2022-divvy-tripdata.csv")
month_7 <- read_csv("sep2022-divvy-tripdata.csv")
month_8 <- read_csv("oct2022-divvy-tripdata.csv")
month_9 <- read_csv("nov2022-divvy-tripdata.csv")
month_10 <- read_csv("dec2022-divvy-tripdata.csv")
month_11 <- read_csv("jan2023-divvy-tripdata.csv")
month_12 <- read_csv("feb2023-divvy-tripdata.csv")

colnames(month_1)
colnames(month_2)
colnames(month_3)
colnames(month_4)
colnames(month_5)
colnames(month_6)
colnames(month_7)
colnames(month_8)
colnames(month_9)
colnames(month_10)
colnames(month_11)
colnames(month_12)

str(month_1)
glimpse(month_1)
glimpse(month_2)
glimpse(month_3)
glimpse(month_4)
glimpse(month_5)
glimpse(month_6)
glimpse(month_7)
glimpse(month_8)
glimpse(month_9)
glimpse(month_10)
glimpse(month_11)
glimpse(month_12)

all_tripsdata <- bind_rows(month_1,month_2,month_3,month_4,month_5,month_6,month_7,month_8,month_9,month_10,month_11,month_12)

str(all_tripsdata)

colnames(all_tripsdata)
dim(all_tripsdata)
head(all_tripsdata)
tibble(all_tripsdata)
summary(all_tripsdata)

all_tripsdata %>% 
  select(member_casual)

n_distinct(all_tripsdata$member_casual)
unique(all_tripsdata$member_casual)


updated_tripsdata <- all_tripsdata %>% 
  select(-c(start_lat,end_lat,start_lng,end_lng))

table(updated_tripsdata$member_casual)

updated_tripsdata %>% 
  select(started_at)
as.Date(updated_tripsdata$started_at)

updated_tripsdata$date <- as.Date(updated_tripsdata$started_at)
updated_tripsdata$month <- format(as.Date(updated_tripsdata$date), "%m")
updated_tripsdata$day <- format(as.Date(updated_tripsdata$date), "%d")
updated_tripsdata$year <- format(as.Date(updated_tripsdata$date), "%Y")
updated_tripsdata$day_of_the_week <- format(as.Date(updated_tripsdata$date), "%A")

updated_tripsdata$ride_length <- difftime(updated_tripsdata$ended_at, updated_tripsdata$started_at)
str(updated_tripsdata)

is.factor(updated_tripsdata$ride_length)

updated_tripsdata$ride_length <- as.numeric(as.character(updated_tripsdata$ride_length))
is.numeric(updated_tripsdata$ride_length)

summary(updated_tripsdata)

new_tripsdata <- updated_tripsdata[!(updated_tripsdata$ride_length<0),]

mean(new_tripsdata$ride_length)
max(new_tripsdata$ride_length)
min(new_tripsdata$ride_length)
median(new_tripsdata$ride_length)

summary(new_tripsdata$ride_length)

aggregate(new_tripsdata$ride_length~new_tripsdata$member_casual, FUN = mean)
aggregate(new_tripsdata$ride_length~new_tripsdata$member_casual, FUN = median)
aggregate(new_tripsdata$ride_length~new_tripsdata$member_casual, FUN = max)
aggregate(new_tripsdata$ride_length~new_tripsdata$member_casual, FUN = min)

aggregate(new_tripsdata$ride_length ~ new_tripsdata$member_casual + new_tripsdata$day_of_the_week, FUN = mean)


new_tripsdata$day_of_week <- ordered(new_tripsdata$day_of_the_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
new_tripsdata %>% group_by(member_casual, day_of_week) %>% summarise(number_of_rides=n() ,average_ride_length = mean(ride_length), .grpoups="drop") %>% arrange(member_casual, day_of_week)

aggregate(new_tripsdata$ride_length ~ new_tripsdata$member_casual + new_tripsdata$day_of_week, FUN = mean)
 
cyclistic_dataset <- new_tripsdata %>% 
  mutate(day_of_week=lubridate::wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)

cyclistic_dataset2 <- new_tripsdata %>% 
  mutate(month=lubridate::month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, month)

head(new_tripsdata)
colnames(new_tripsdata)
min_date <- min(updated_tripsdata$date)
max_date <- max(updated_tripsdata$date)

cyclistic_dataset3 <- new_tripsdata %>% 
  group_by(member_casual) %>% 
  summarise(max(ride_length), average_ride_length = mean(ride_length), min(ride_length))
  
cyclistic_dataset4 <- new_tripsdata %>% 
  group_by(member_casual) %>% 
  summarise(rider_count = n())
  

ggplot(cyclistic_dataset) + geom_col(mapping=aes(x=day_of_week, y=number_of_rides, fill = member_casual), position="dodge") + labs(title = "Weekdays comparison by number of rides by user types", x="Day of Week", y="Number of Rides",
                                                                                                                                       caption=paste0("Data from: ", min_date, " to ", max_date)) 

ggplot(cyclistic_dataset) + geom_col(mapping=aes(x=day_of_week, y=average_duration, fill = member_casual), position="dodge") + labs(title = "Weekdays comparison by average ride duration by user", x="Day of Week", y="Average duration",
                                                                                                                                       caption=paste0("Data from: ", min_date, " to ", max_date))

ggplot(cyclistic_dataset2) + geom_col(mapping=aes(x=month, y=number_of_rides, fill = member_casual), position="dodge") + labs(title = "Monthly comparison by number of rides by user types", x="Months", y="Number of Rides",
                                                                                                                                       caption=paste0("Data from: ", min_date, " to ", max_date)) 

ggplot(cyclistic_dataset2) + geom_col(mapping=aes(x=month, y=average_duration, fill = member_casual), position="dodge") + labs(title = "Monthly comparison by average ride duration by user", x="Months", y="Average duration",
                                                                                                                                        caption=paste0("Data from: ", min_date, " to ", max_date)) 
counts <- aggregate(new_tripsdata$ride_length ~ new_tripsdata$member_casual + new_tripsdata$day_of_the_week, FUN = mean)


ggplot(cyclistic_dataset3) + geom_col(mapping=aes(x=member_casual, y=average_ride_length, fill=member_casual)) +labs(title = "Average ride length based on user types", x="Rider type", y="Average ride length", caption=paste0("Data from:", min_date, "to", max_date ))


ggplot(cyclistic_dataset4) + geom_col(mapping=aes(x=member_casual, y=rider_count, fill=member_casual)) + labs(title = "Comparison of users", x="Rider type", y="Total number of riders", caption=paste0("Data from:", min_date, "to", max_date))

head(new_tripsdata)
