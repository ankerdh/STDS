library(tidyverse)

traffic_vol$daily_total <- as.numeric(traffic_vol$daily_total)
traffic_vol$month <- as.factor(traffic_vol$month)
traffic_vol$day_of_week <- as.factor(traffic_vol$day_of_week)

eleven <- filter(traffic_vol, year==2011)
twelve <- filter(traffic_vol, year==2012)
thirteen <- filter(traffic_vol, year==2013)
fourteen <- filter(traffic_vol, year==2014)
fifteen <- filter(traffic_vol, year==2015)
sixteen <- filter(traffic_vol, year==2016)

ggplot(traffic_vol, aes(x=month, y=daily_total, color=year)) +
  geom_bar(stat="identity") +
  facet_wrap(~year) +
  guides(color=F)
  
ggplot(traffic_vol, aes(x=day_of_week, y=daily_total, color=year)) +
  geom_bar(stat="identity") +
  facet_wrap(~year) +
  guides(color=F)

ggplot(traffic_vol) + 
  geom_bar(aes(x=road_functional_hierarchy))
ggplot(traffic_vol) + 
  geom_bar(aes(x=lane_count))
ggplot(traffic_vol) + 
  geom_bar(aes(x=road_classification_admin))
ggplot(traffic_vol) + 
  geom_bar(aes(x=rms_region))

summary(traffic_vol$hour_00)

traffic_vol$daily_total <- as.numeric(traffic_vol$daily_total)
traffic_vol$month <- as.factor(traffic_vol$month)
traffic_vol$day_of_week <- as.factor(traffic_vol$day_of_week)

summary(traffic_vol$daily_total)     
class(traffic_vol$day_of_week)

a <- filter(traffic_vol, year==2013, month==6)
summary(a$daily_total)

b <- filter(traffic_vol, year==2011, month==6)
summary(b$daily_total)
sum(b$daily_total)
sum(a$daily_total)

ggplot(eleven) +
  geom_count(aes(x=road_functional_hierarchy, y=day_of_week))
