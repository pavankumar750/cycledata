# Cyclistic Bike-Share Case Study
# Install and load required packages
packages <- c("tidyverse", "lubridate", "dplyr", "readr")
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Load data
q1_2019 <- read_csv("Divvy_Trips_2019_Q1 (2).csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

# Rename columns for consistency
q1_2019 <- rename(q1_2019,
  ride_id = trip_id,
  rideable_type = bikeid,
  started_at = start_time,
  ended_at = end_time,
  start_station_name = from_station_name,
  start_station_id = from_station_id,
  end_station_name = to_station_name,
  end_station_id = to_station_id,
  member_casual = usertype
)

# Ensure correct data types
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))

# Combine datasets
all_trips <- bind_rows(q1_2019, q1_2020)

# Remove unnecessary columns
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, tripduration))

# Create ride_length
all_trips$ride_length <- as.numeric(difftime(all_trips$ended_at, all_trips$started_at))

# Remove invalid data (e.g., HQ QR station, negative ride_length)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0), ]

# Recode member_casual for consistency
all_trips_v2 <- all_trips_v2 %>%
  mutate(member_casual = recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))

# Ensure started_at is POSIXct
all_trips_v2$started_at <- as.POSIXct(all_trips_v2$started_at, format = "%Y-%m-%d %H:%M:%S")

# Create and save 'day_of_week' as an ordered factor
all_trips_v2$day_of_week <- wday(all_trips_v2$started_at, label = TRUE, abbr = FALSE)
all_trips_v2$day_of_week <- factor(
  all_trips_v2$day_of_week,
  levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
  ordered = TRUE
)

# Aggregate average ride length by member type and day of week, save to CSV
counts <- aggregate(ride_length ~ member_casual + day_of_week, data = all_trips_v2, FUN = mean)
write.csv(counts, file = "avg_ride_length.csv", row.names = FALSE)

# Summary statistics
print(summary(all_trips_v2$ride_length))
print(aggregate(ride_length ~ member_casual, data = all_trips_v2, FUN = mean))
print(aggregate(ride_length ~ member_casual, data = all_trips_v2, FUN = median))
print(aggregate(ride_length ~ member_casual, data = all_trips_v2, FUN = max))
print(aggregate(ride_length ~ member_casual, data = all_trips_v2, FUN = min))

# Plot: Number of rides by weekday and membership type
library(ggplot2)
all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Weekday and Membership Type", x = "Day of Week", y = "Number of Rides")

# Plot: Average ride duration by weekday and membership type
all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Duration by Weekday and Membership Type", x = "Day of Week", y = "Average Duration (seconds)")

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
