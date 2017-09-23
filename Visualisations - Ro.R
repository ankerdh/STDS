library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(rsdmx)
library(data.table)
library(geosphere)
library(reshape2)
library(gmapsdistance)
library(sqldf)
library(tictoc)
library(RDS)
library(scales)
library(gdata)

data <- read_rds("/Users/RohanDanisCox/STDS/FullData.rds")
stations <- read_rds("/Users/RohanDanisCox/STDS/StationsWeatherAndDistance.rds")
str(data)

data$date <- as.Date(with(data,paste(year,month,day,sep="-")))
data1 <- data %>%
  group_by(station_key,cardinal_direction_seq) %>%
  mutate(Commencement= min(date))

data2 <- data1 %>% 
  distinct(station_key,Commencement) %>%
  filter(Commencement > "2015-03-01")

