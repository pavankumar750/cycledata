# Cyclistic Bike-Share Case Study

## Objective
To understand how casual riders and annual members use Cyclistic bikes differently, and provide actionable recommendations to increase membership.

##Tools Used
- Tableau (Data visualization)
- Excel / R  (Data cleaning)
- install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
q1_2019 <- read_csv("Divvy_Trips_2019_Q1 (2).csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
install.packages("readr")
library(readr)
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))
str(q1_2019)
str(q1_2020)
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))
all_trips <- bind_rows(q1_2019, q1_2020)#, q3_2019)#, q4_2019, q1_2020)
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))
colnames(all_trips) #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips) #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of data frame. Also tail(all_trips)
str(all_trips) #See list of columns and data types (numeric, character, etc)
summary(all_trips) #Statistical summary of data. Mainly for numerics
table(all_trips$member_casual)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
str(all_trips)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride
summary(all_trips_v2$ride_length)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
unique(all_trips_v2$day_of_week)
library(lubridate)
all_trips_v2$day_of_week <- factor(
  all_trips_v2$day_of_week,
  levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
  ordered = TRUE
)
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using
  wday()
group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates
            the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average
  duration)
arrange(member_casual, weekday)
str(all_trips_v2$started_at)
all_trips_v2$started_at <- as.POSIXct(all_trips_v2$started_at, format = "%Y-%m-%d %H:%M:%S")
all_trips_v2 %>%
  mutate(weekday = factor(
    wday(started_at, label = TRUE, abbr = FALSE),
    levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
    ordered = TRUE
  )) %>%
  group_by(member_casual, weekday) %>%
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length)
  ) %>%
  arrange(member_casual, weekday)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using
  wday()
group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates
            the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average
  duration
arrange(member_casual, weekday)
all_trips_v2$started_at <- as.POSIXct(all_trips_v2$started_at, format = "%Y-%m-%d %H:%M:%S")
library(dplyr)
library(lubridate)

all_trips_v2 %>%
  mutate(weekday = factor(
    wday(started_at, label = TRUE, abbr = FALSE),
    levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
    ordered = TRUE
  )) %>%
  group_by(member_casual, weekday) %>%
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length)
  ) %>%
  arrange(member_casual, weekday)
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
all_trips_v2 <- all_trips_v2 %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
library(lubridate)

# Ensure started_at is in POSIXct format
all_trips_v2$started_at <- as.POSIXct(all_trips_v2$started_at)

# Create and save 'day_of_week'
all_trips_v2$day_of_week <- wday(all_trips_v2$started_at, label = TRUE, abbr = FALSE)

# Make it an ordered factor from Sunday to Saturday
all_trips_v2$day_of_week <- factor(
  all_trips_v2$day_of_week,
  levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
  ordered = TRUE
)
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
                      all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

- GitHub (Portfolio hosting)

##Key Insights
- Casual riders prefer weekends and longer, leisure rides.
- Annual members ride more frequently on weekdays for shorter trips.
- There are clear patterns in ride time, duration, and seasonality.

##Top 3 Recommendations
1. Offer weekend membership promotions to convert casual riders.
2. Launch marketing campaigns targeting leisure and tourism riders.
3. Improve weekday service support for commuting members.

##Files
- `/Visuals/`: Contains all charts created in Tableau or Excel.
- `Cyclistic_Report.pdf`: Full case study write-up.
  ##Additional Links
-https://public.tableau.com/views/divy_data_20-19/Sheet1?:language=en-US&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link
# cycledata
