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
  mutate(Commenced= min(date),Ceased=max(date)) %>%
  mutate(CommencedMonth=format(as.Date(Commenced),"%Y-%m"),CeasedMonth=format(as.Date(Ceased),"%Y-%m")) 

data3 <- data1 %>% 
  distinct(station_key,CommencedMonth,CeasedMonth) 

data4 <- data3 %>%
  group_by(Commenced) %>%
  summarise(Number=n())

# Where does the data come in and when does it stop
# Number of stations data entering - by month
ggplot(data3,aes(x=CommencedMonth))+
  geom_bar() 

# Number of stations data exiting - by month
ggplot(data3,aes(x=CeasedMonth))+
  geom_bar()

# Can we put these on the same graph?
data5 <- data3 %>%
  gather(Change,Date,-(station_key:cardinal_direction_seq))

### NSW WIDE GRAPHS SHOWING LOCATIONS
 
#Add average daily total and pull out relevant information

NSWgraph <- data %>%
  group_by(station_key) %>%
  mutate(average=mean(daily_total)) %>%
  distinct(station_key,wgs84_latitude,wgs84_longitude,lon,lat,average)

NSW <- ggmap(get_googlemap(c(lon=147.5, lat=-32.8), zoom=6, maptype="roadmap", size=c(600,500),color="bw"))

NSW +
  geom_point(aes(x=wgs84_longitude, y=wgs84_latitude,color=average), data=NSWgraph, alpha=0.5) +
  scale_color_gradient2(low="#F2C314",mid="red",high ="darkred",midpoint=30000)  

NSW +
  geom_point(aes(x=wgs84_longitude, y=wgs84_latitude,color=average), data=NSWgraph, alpha=0.5) +
  geom_point(aes(x=lon, y=lat), data=stations, colour="darkblue", size=1,alpha=.9) +
  scale_color_gradient2(low="#F2C314",mid="red",high ="darkred",midpoint=30000) 






syd <- ggmap(get_googlemap(c(lon=151, lat=-33.85), zoom=10, maptype="roadmap", size=c(600,400),color="bw"))

syd +
  geom_polygon()
  geom_point(aes(x=wgs84_longitude, y=wgs84_latitude, color = average), data=data1,size=2,shape=21) +
  geom_point(aes(x=lon, y=lat), data=stations, colour="blue", size=2) +
  ggtitle("Traffic counter and weather stations - Sydney") + 
  #scale_colour_gradientn(colours=rainbow(3)) 
  scale_color_gradient2(low="#434343",mid="#F2C314",high ="red",midpoint=40000)