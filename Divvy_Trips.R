# This analysis is based on the Divvy case study "'Sophisticated, Clear, and 
# Polished': Divvy and Data Visualization" written by Kevin Hartman (found 
# here: https://artscience.blog/home/divvy-dataviz-case-study). The purpose of 
# this script is to consolidate downloaded Divvy data into a single dataframe 
# and then conduct simple analysis to help answer the key question: "In what 
# ways do members and casual riders use Divvy bikes differently?"


# Install and load required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()
setwd("C:/Users/User/Documents/danaputra/divvy_project")

# STEP 1: DATA COLLECTION

q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

# STEP 2: DATA WRANGLING

# Inspect columns name and check for the naming consistency
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

# Rename all columns that are not consistent with the latest data; q1_2020
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

# Inspect dataframes to check for the data type consistency
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character so they can stack correctly
q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))

q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))

q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))

# Stack all dataframes into one big dataframe
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped 
# beginning in q1_2020
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - 
            Rental Details Duration In Seconds Uncapped", "05 - Member Details 
            Member Birthday Year", "Member Gender", "tripduration"))

#STEP 3: DATA CLEANING

# Inspect the new table
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

# There are several problems
# 1. In the "member_casual" column, there are 2 names for members("member" and
# "subscriber") and 2 names for casual riders("Customer" and "casual"), so we
# need to consolidate them
# 2. The data is too granular, we need to add some additional columns such as
# "day", "month", and "year" so we can calculate the ride length in seconds
# 3. Add a calculated field for length of ride since the data q1_2020 did not
# have "tripduration" column. We will add "ride_length" to entire new table for
# consistency
# 4. There are some rides where tripduration shows up as a negative, we will
# eliminate these "bad data"

# First, seeing how many observations fall under each usertype
table(all_trips$member_casual)

# Mutate names in "member_casual" column as desired values
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check if the column already in desired values
table(all_trips$member_casual)

# Second, Add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) #The default format: yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Third, add a "ride_length" calculation column
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the ride_length column
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Fourth, remove "bad data" that tripduration with negative values 
# We will create a new dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | 
                              all_trips$ride_length<0),]

# STEP 4: DESCRIPTIVE ANALYSIS

# Descriptive analysis on ride_length (in seconds)
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_
          v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c
                                    ("Sunday", "Monday", "Tuesday", "Wednesday"
                                    ,"Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  # creates weekday field 
                                                        # using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()	# calculates the number of rides and 
                                  # average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the 
                                                          # average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# STEP 5: EXPORT SUMMARY FILE FOR FUTURE ANALYSIS

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
                      all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, "C:/Users/User/Documents/danaputra/divvy_summary.csv")
