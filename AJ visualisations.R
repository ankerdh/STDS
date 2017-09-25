#exploratory visualisations - AJ

install.packages("RDS")
library(RDS)
library(ggplot2)
library(rgdal)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(ggmap)

data <- readRDS("/Users/AD/STDS/FullDataRain.rds")
str(data)

#order the rms region and road hierarchy factors so they chart sensibly
data$rms_region <- ordered(data$rms_region, levels = c("Sydney", "Hunter", "Northern", "Southern", "South West", "Western"))
data$road_functional_hierarchy <- ordered(data$road_functional_hierarchy, levels = c("Motorway", "Primary Road", "Arterial Road", "Sub-Arterial Road", "Distributor Road", "Local Road"))

#remove the one station that has no geography
data <- data[data$station_key != 18479663,]

#boxplot to show the variability of daily counts in different regions and road types
ggplot(data) +
  geom_boxplot(aes(y=daily_total,x=road_functional_hierarchy)) + 
  facet_wrap(~rms_region) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

motorways <- data[data$road_functional_hierarchy=="Motorway",]

ggplot(motorways, aes(daily_total)) +
  geom_histogram(binwidth = 1000, fill ="#F2C314") + 
  theme(text = element_text(size = 16, family="Raleway"))+
  theme(text = element_text(size = 16, family="Raleway"))+
  facet_wrap(~rms_region) 

#create rain bins
data <- data %>%
  mutate(rainclassification =
           cut(data$DailyRain,c(-1,5,10,20,50, Inf),labels=c("no rain","sprinkle","rain","heavy rain", "wow")))

#pair plots to look for relationships, data issues
pairs_data <- data[,c("daily_total", "DailyRain", "rainclassification", "rms_region", "road_functional_hierarchy", "month", "day_of_week", "Distance_CBD", "pop.density", "pop.work.age.percent", "pop.school.age.percent", "density.vehicles.light", "density.vehicles.heavy","public_holiday", "school_holiday")]
pairs_data[,] <- as.numeric(unlist(pairs_data[,]))
#pairs_data_small <- sample_n(pairs_data, 10000)


#rain vs traffic
ggplot(pairs_data, aes(daily_total)) +
  geom_histogram(binwidth = 1000, fill ="#F2C314") + 
  theme(text = element_text(size = 16, family="Raleway"))+
  facet_wrap(~rainclassification, scales="free_y") 

summary(pairs_data)

#Sydney Region only
data_Sydney <- data[data$rms_region=="Sydney",]
data_Sydney$density_workers <- data_Sydney$pop.density * data_Sydney$pop.work.age.percent / 100

#pairs in Sydney
pairs_data_Sydney <- data_Sydney[,c("daily_total", "road_functional_hierarchy", "month", "day_of_week", "Distance_CBD", "pop.density", "pop.work.age.percent", "density_workers", "pop.school.age.percent", "density.vehicles.light")]
pairs_data_Sydney[,] <- as.numeric(unlist(pairs_data_Sydney[,]))


#sample
pairs_data_Sydney_small <- sample_n(pairs_data_Sydney, 10000)
pairs(pairs_data_Sydney_small)


#correlations in Sydney region
library(polycor)
pairs_data_Sydney <- as.data.frame(pairs_data_Sydney)
hetcor(pairs_data_Sydney)

#correlations
pairs_data <- as.data.frame(pairs_data)
hetcor(pairs_data)

#PCA
install.packages("FactoMineR")
library("FactoMineR")
library("factoextra")
res.pca <- PCA(pairs_data, graph = FALSE)
eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])



#subsetting to find same same but different
  
same_same <- as.data.frame(
  data[
  data$rms_region=="Sydney" |
    data$road_functional_hierarchy=="Local Road" |
    data$Distance_CBD > 10, 
  ]
)

station_examples_large <- as.data.frame(cbind(same_same$station_key, same_same$full_name, same_same$lga))
station_examples_small <- sample_n(station_examples_large,50)

write.csv(station_examples_large, "temp.csv")

station_examples <- data[data$station_key %in% c("57453", "57168", "56795", "56716" , "56745"),]
station_characteristics <- data %>%
  group_by(station_key, full_name, suburb, rms_region, road_functional_hierarchy, mab_way_type, road_classification_admin, cardinal_direction_seq, Distance_CBD, lane_count) %>%
  distinct(station_key)

write.csv(station_characteristics, "stations_temp.csv")


# do we have a bunch of missing data, 
#and WTF at April 2015 where the median daily observations per station dropped significantly

# this next bit suggests that
#April 2015 is incomplete - all the stations report too few observations
#and 150 new stations turned up in April 2015
#but it is clear that there's not missing observations, just less traffic on these new stations

data1 <- data %>%
  group_by(station_key) %>%
  mutate(average=mean(daily_total)) %>%
  distinct(station_key,wgs84_latitude,wgs84_longitude,lon,lat,average)

data3 <- data %>%
  filter(year==2015,month==3) %>%
  group_by(station_key) %>%
  summarise(n())

data4 <- data %>%
  filter(year==2015,month==4) %>%
  group_by(station_key) %>%
  summarise(n())

data5 <- data %>%
  filter(year==2015,month==5) %>%
  group_by(station_key) %>%
  summarise(n())

str(data3)
str(data4)

data3_wrong_count <- subset(data3,!(`n()` %in% c(31,62)))
data4_wrong_count <- subset(data4,!(`n()` %in% c(30,60)))
data5_wrong_count <- subset(data5,!(`n()` %in% c(31,62)))

str(data3_wrong_count)
str(data4_wrong_count)
str(data5_wrong_count)

data_3and4 <- merge(x = data3, y = data4, by = "station_key", all = TRUE)
data_345 <- merge(x = data_3and4, y = data5, by = "station_key", all = TRUE)
data_345

#this bit examines how from 2015 there is a dramatic shift in type of roads being monitored
data$yearmonth <- as.character((data$year*100 + data$month)/100)
ggplot(data) +
  geom_boxplot(aes(y=daily_total,x=yearmonth)) + 
  facet_wrap(~road_functional_hierarchy, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
