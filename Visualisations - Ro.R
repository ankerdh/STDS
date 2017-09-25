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
library(ggmap)
library(rgdal)

data <- read_rds("/Users/RohanDanisCox/STDS/FullData.rds")
str(data)

data$date <- as.Date(with(data,paste(year,month,day,sep="-")))

data1 <- FinalData %>%
  group_by(station_key,cardinal_direction_seq) %>%
  mutate(Commenced= min(date),Ceased=max(date)) %>%
  mutate(CommencedMonth=format(as.Date(Commenced),"%Y-%m"),CeasedMonth=format(as.Date(Ceased),"%Y-%m")) 

data3 <- data1 %>% 
  distinct(station_key,CommencedMonth,CeasedMonth,.keep_all = TRUE) 

data4 <- data3 %>%
  group_by(CommencedMonth,road_functional_hierarchy) %>%
  summarise(Number=n())


# Where does the data come in and when does it stop
# Number of stations data entering - by month
ggplot(data,aes(x=CommencedMonth))+
  geom_bar() 

# Number of stations data exiting - by month
ggplot(data3,aes(x=CeasedMonth))+
  geom_bar()

# Can we put these on the same graph?
data5 <- data3 %>%
  gather(Change,Date,-(station_key:cardinal_direction_seq))

## Get data for Area Graph

data6 <- FinalData %>%
  group_by(station_key,date,road_functional_hierarchy) %>%
  summarise(number=n())

data7 <- data6 %>%
  group_by(date,road_functional_hierarchy) %>%
  summarise(number=n())

data7$road_functional_hierarchy <- factor(data7$road_functional_hierarchy,ordered = TRUE,levels = c("Motorway","Primary Road", "Arterial Road","Sub-Arterial Road","Distributor Road","Local Road"))

#### AREA GRAPH ####

ggplot(data7,aes(x=date,y=number,fill=road_functional_hierarchy,order=road_functional_hierarchy))+
  geom_area() +
  theme(axis.title = element_text(size=18)) +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=15))+
  theme(text=element_text(size=15)) +
  labs(y="Number of Traffic Stations",x="Time") +
  guides(fill=guide_legend(title="Road Function"))


### NSW WIDE GRAPHS SHOWING LOCATIONS
 
#Add average daily total and pull out relevant information

NSWgraph <- data %>%
  group_by(station_key) %>%
  mutate(average=mean(daily_total)) %>%
  distinct(station_key,wgs84_latitude,wgs84_longitude,lon,lat,average,.keep_all=TRUE)

NSW <- ggmap(get_googlemap(c(lon=147.5, lat=-32.8), zoom=6, maptype="roadmap", size=c(600,500),color="bw"))
NSW
NSW +
  geom_point(aes(x=wgs84_longitude, y=wgs84_latitude,color=average), data=NSWgraph, alpha=0.5,size=2) +
  scale_color_gradient2(low="#F2C314",mid="red",high ="darkred",midpoint=30000)+
  guides(colour=FALSE)

NSW +
  geom_point(aes(x=wgs84_longitude, y=wgs84_latitude,color=average), data=NSWgraph, alpha=0.5,size=2) +
  geom_point(aes(x=lon, y=lat), data=NSWgraph, colour="darkblue", size=2,alpha=.9) +
  scale_color_gradient2(low="#F2C314",mid="red",high ="darkred",midpoint=30000) +
  guides(colour=FALSE)

SYD <- ggmap(get_googlemap(c(lon=151.1, lat=-33.8), zoom=10, maptype="roadmap", size=c(600,500),color="bw"))
SYD +
geom_point(aes(x=wgs84_longitude, y=wgs84_latitude,color=average), data=NSWgraph, alpha=0.5,size=2) +
  geom_point(aes(x=lon, y=lat), data=NSWgraph, colour="darkblue", size=2,alpha=.9) +
  scale_color_gradient2(low="#F2C314",mid="red",high ="darkred",midpoint=30000) +
  guides(colour=FALSE)



Checking <- FinalData %>%
  filter(DailyRain>60)
