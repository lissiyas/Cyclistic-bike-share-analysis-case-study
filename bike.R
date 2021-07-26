install.packages("lubridate")
library(tidyverse)
## read the datasets
library(lubridate)
library(ggplot2)
install.packages("dplyr")
library(dplyr) 

may <- read.csv("may2020.csv")
june <- read.csv("june2020.csv")
july <- read.csv("july2020.csv")
aug <- read.csv("aug2020.csv")
sep <- read.csv("sep2020.csv")
oct <- read.csv("oct2020.csv")
nov <- read.csv("nov2020.csv")
dec <- read.csv("dec2020.csv")
jan <- read.csv("jan2021.csv")
feb <- read.csv("feb2021.csv")
mar <- read.csv("mar2021.csv")
apr <- read.csv("apr2021.csv")

head(may)
colnames(may)
str(may)
mean(ride_len)

is.empty(apr$end_station_name)
view(apr$end_station_name=="")

##check the column names are equal
colnames(may)
colnames(june)
colnames(july)
colnames(aug)
colnames(sep)
colnames(nov)
colnames(dec)
colnames(jan)
colnames(feb)
colnames(mar)
colnames(apr)


###converting the datatypes to integer 

dec$start_station_id  = as.integer(dec$start_station_id)
dec$end_station_id   = as.integer(dec$end_station_id )

jan$start_station_id  = as.integer(jan$start_station_id)
jan$end_station_id   = as.integer(jan$end_station_id )

feb$start_station_id  = as.integer(feb$start_station_id)
feb$end_station_id   = as.integer(feb$end_station_id )

mar$start_station_id  = as.integer(mar$start_station_id)
mar$end_station_id   = as.integer(mar$end_station_id )

apr$start_station_id  = as.integer(apr$start_station_id)
apr$end_station_id   = as.integer(apr$end_station_id )


##convert the char format of date to dateformat

may$started_at = as.POSIXct(may$started_at)
may$ended_at = as.POSIXct(may$ended_at)
head(may)



june$started_at = as.POSIXct(june$started_at)
june$ended_at = as.POSIXct(june$ended_at)
head(june)

is.na(june)
 
july$started_at = as.POSIXct(july$started_at)
july$ended_at = as.POSIXct(july$ended_at)
head(july)

aug$started_at = as.POSIXct(aug$started_at)
aug$ended_at = as.POSIXct(aug$ended_at)
head(aug)

sep$started_at = as.POSIXct(sep$started_at)
sep$ended_at = as.POSIXct(sep$ended_at)
head(sep)

oct$started_at = as.POSIXct(oct$started_at)
oct$ended_at = as.POSIXct(oct$ended_at)
head(oct)

nov$started_at = as.POSIXct(nov$started_at)
nov$ended_at = as.POSIXct(nov$ended_at)
head(nov)

dec$started_at = as.POSIXct(dec$started_at)
dec$ended_at = as.POSIXct(dec$ended_at)
head(aug)

jan$started_at = as.POSIXct(jan$started_at)
jan$ended_at = as.POSIXct(jan$ended_at)
head(jan)

feb$started_at = as.POSIXct(feb$started_at)
feb$ended_at = as.POSIXct(feb$ended_at)
head(feb)

mar$started_at = as.POSIXct(mar$started_at)
mar$ended_at = as.POSIXct(mar$ended_at)
head(mar)

apr$started_at = as.POSIXct(apr$started_at)
apr$ended_at = as.POSIXct(apr$ended_at)
head(apr)




##combining all datas to aa single csv (yearly data)
year_df <- bind_rows(may,june,july,aug,sep,oct,nov,dec,jan,feb,mar,apr)
head(year_df)

year_df$end_station_name[year_df$end_station_name == ""] <- NA
year_df$start_station_name[year_df$start_station_name == ""] <- NA



head(new_df)

## remove the unwanted columns
year_df <- year_df %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

glimpse(year_df)


##Add columns that list the date, month, day, and year of each ride

year_df$date <- as.Date(year_df$started_at) #The default format is yyyy-mm-dd
year_df$month <- format(as.Date(year_df$date), "%m")
year_df$day <- format(as.Date(year_df$date), "%d")
year_df$year <- format(as.Date(year_df$date), "%Y")
year_df$day_of_week <- format(as.Date(year_df$date), "%A")

year_df$ride_length_min <- difftime(year_df$ended_at,year_df$started_at)/60

#calculate the ride length 

year_df$ride_length <- difftime(year_df$ended_at,year_df$started_at)


##convert the ridelength to numeric value


is.factor(year_df$ride_length)
year_df$ride_length <- as.numeric(as.character(year_df$ride_length))
is.numeric(year_df$ride_length)

year_df$ride_length_min <- as.numeric(as.character(year_df$ride_length_min))
is.numeric(year_df$ride_length_min)

head(year_df)

 mean(year_df$ride_length)


max(year_df$ride_length)

 min(year_df$ride_length)
 ##summary of the ride length
 year_df<- year_df[!( year_df$ride_length_min==0),]
 
 year_df <- year_df[!(year_df$start_station_name == "HQ QR" | year_df$ride_length<0),]

summary(year_df$ride_length)





#save the year data

##write.csv(year_df,file = file.choose(new = T))

##ridelength of casualvs member rides

aggregate(year_df$ride_length ~ year_df$member_casual, FUN = mean)
aggregate(year_df$ride_length ~ year_df$member_casual, FUN = median)
aggregate(year_df$ride_length ~ year_df$member_casual, FUN = max)
aggregate(year_df$ride_length ~ year_df$member_casual, FUN = min)

##find the average rides and total rides by the casual and members in sun to sat of year

year_df$day_of_week <- ordered(year_df$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(year_df$ride_length/60 ~ year_df$member_casual + year_df$day_of_week, FUN = mean)


total_rides <- year_df %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  arrange(member_casual, weekday)
write.csv(total_rides, "total_rides_min.csv")

View(total_rides)
str(total_rides)


##find the number of rides in each month

total_rides_month <- year_df %>% 
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  arrange(member_casual, month)
View(total_rides_month)
write.csv(total_rides_month, "total_rides_month.csv")


##rides throughout the hours

total_rides_hour <- year_df %>% 
  mutate(hour = as.factor(hour(started_at))) %>%
  group_by(member_casual, hour) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  arrange(member_casual, hour)

View(total_rides_hour)
write.csv(total_rides_hour,"total_ride_hour.csv")

##analysis by ride station

total_rides_station <- year_df %>% 
  group_by(member_casual, start_station_name) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  arrange(member_casual, -number_of_rides, start_station_name)
View(total_rides_station)
write.csv(total_rides_station,"total_ride_station.csv")

##end station analysis


total_end_station <- year_df %>% 
  group_by(member_casual, end_station_name) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  arrange(member_casual, -number_of_rides)
View(total_end_station)



##analysis based on the bike type


bike_type<-year_df %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(rideable_type, weekday,member_casual) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length/60)) %>% 
  arrange(rideable_type, weekday) 
view(bike_type)

write.csv(bike_type,"bike_type.csv")

##analysis based on started name  



  
##distinct(start_station_name, .keep_all = TRUE) %>%
























