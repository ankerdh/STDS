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
library(readr)
library(scales)


data <- readRDS("/Users/AD/STDS/FullDataRain.rds")
str(data)

#order the rms region and road hierarchy factors so they chart sensibly
data$rms_region <- ordered(data$rms_region, levels = c("Sydney", "Hunter", "Northern", "Southern", "South West", "Western"))
data$road_functional_hierarchy <- ordered(data$road_functional_hierarchy, levels = c("Motorway", "Primary Road", "Arterial Road", "Sub-Arterial Road", "Distributor Road", "Local Road"))

#remove the one station that has no geography
data <- data[data$station_key != 18479663,]

#boxplot to show the variability of daily counts in different regions and road types
sydney_hunter_data <- data[data$rms_region %in% c("Sydney","Hunter"),]
ggplot(sydney_hunter_data) +
  geom_boxplot(aes(y=daily_total,x=road_functional_hierarchy)) + 
  facet_wrap(~rms_region) + 
  theme(text = element_text(size = 16, family="Raleway")) +
  labs(y="Daily traffic volume", 
       x="Road function", 
       title = "Different RMS Regions and Road Functions\nexhibit different levels of traffic variability",
       subtitle = "In particular, Hunter motorways are more likely to have quiet periods"
       ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#simplified chart - histograms just of motorways in Sydney and Hunter
motorways <- data[data$road_functional_hierarchy=="Motorway",]
ggplot(motorways, aes(daily_total)) +
  geom_histogram(binwidth = 1000, fill ="#F2C314") + 
  theme(text = element_text(size = 16, family="Raleway"))+
  theme(text = element_text(size = 16, family="Raleway"))+
  facet_wrap(~rms_region) 


#rain vs traffic - #1 B&W initial plot
ggplot(data) +
  geom_point(aes(y=daily_total,x=DailyRain)) + 
  theme(text = element_text(size = 16, family="Raleway")) +
  labs(y="Daily traffic volume", 
       x="Daily rainfall (mm)", 
       title = "Traffic vs Rainfall - potential correlation",
       subtitle = "However, vertical bands invite a closer investigation"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#rain vs traffic - #2 - colour for days of week
ggplot(data) +
  geom_point(aes(y=daily_total,x=DailyRain, colour=factor(day_of_week))) + 
  theme(text = element_text(size = 16, family="Raleway")) +
  labs(y="Daily traffic volume", 
       x="Daily rainfall (mm)", 
       colour="",
       title = "Traffic vs Rainfall, by Day of Week",
       subtitle = "Makes clear that the plot is skewed by observations\nfrom just a few days of extreme rain"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#create rain bins (not used in final work)
data <- data %>%
  mutate(rainclassification =
           cut(data$DailyRain,c(-1,5,10,20,50, Inf),labels=c("no rain","sprinkle","rain","heavy rain", "wow")))

#trimmed data to look for LGA correlations, data issues
pairs_data_LGA <- data[,c("daily_total", "rms_region", "Distance_CBD", "pop.density", "pop.work.age.percent", "pop.school.age.percent", "density.vehicles.light", "density.vehicles.heavy")]
pairs(pairs_data_LGA)

#trimmed data to look for wider correlations, data issues
pairs_data <- data[,c("daily_total", "DailyRain", "rainclassification", "rms_region", "road_functional_hierarchy", "month", "day_of_week", "Distance_CBD", "pop.density", "pop.work.age.percent", "pop.school.age.percent", "density.vehicles.light", "density.vehicles.heavy","public_holiday", "school_holiday")]
cor_data[,] <- as.numeric(unlist(pairs_data[,]))

#traffic in different rain bins
ggplot(cor_data, aes(daily_total)) +
  geom_histogram(binwidth = 1000, fill ="#F2C314") + 
  theme(text = element_text(size = 16, family="Raleway"))+
  facet_wrap(~rainclassification, scales="free_y") 

#correlations 
library(polycor)
hetcor_results <- hetcor(cor_data)
cor_results <- cor(cor_data)
cor_results
hetcor_results

#explored but not used: Variable Importance package from the Random Forest world
library("Boruta")
boruta_output <- Boruta(daily_total ~ ., data=na.omit(data), doTrace=2)

#explored but not used
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
#create date fields that contain 1st of the month
data$yearmonth <- as.character((data$year*100 + data$month)/100)
data$fakedate <- paste(as.character(data$year), as.character(data$month),"01", sep="-")
data$fakedate <- as.Date(data$fakedate)

ggplot(data) +
  geom_boxplot(aes(y=daily_total,x=yearmonth)) + 
  facet_wrap(~road_functional_hierarchy, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#boxplot for final report showing changed nature of Primary Road data collected from 2015
primary_data <- data[data$road_functional_hierarchy=="Primary Road",]
ggplot(primary_data) +
  geom_boxplot(aes(y=daily_total,x=fakedate, group=fakedate)) + 
  theme(text = element_text(size = 16, family="Raleway")) +
  scale_y_continuous(label=comma) +
  labs(y="Daily traffic volume", 
       x="Time", 
       title = "Primary Road daily traffic distribution, by month",
       subtitle = "There is a clear discontinuity in April 2015"
       ) +
       scale_x_date(name = 'Time', 
                    date_breaks = '1 year',
                    date_labels = '%Y'
                    ) 
