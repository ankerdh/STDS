data <- read_rds("/Users/David Anker/Documents/STDS/FullData.rds")

data$month <- as.factor(data$month)
data$day <- as.factor(data$day)
data$year <- as.factor(data$year)

#Observations

ggplot(data) + 
  geom_bar(aes(x=lane_count, fill=lane_count)) +
  guides(fill=F) +
  labs(title = "Total observations by lane count", y = "Total observations", x = "Lane count")

ggplot(data) + 
  geom_bar(aes(x=road_classification_admin, fill=road_classification_admin)) +
  guides(fill=F) +
  labs(title = "Total observations by road classification", y = "Total observations", x = "Road classification")

ggplot(data) + 
  geom_bar(aes(x=rms_region, fill=rms_region)) +
  guides(fill=F) +
  labs(title = "Total observations by RMS region", y = "Total observations", x = "RMS region")

ggplot(data) + 
  geom_bar(aes(x=road_functional_hierarchy, fill=road_functional_hierarchy)) +
  guides(fill=F) +
  labs(title = "Total observations by road type", y = "Total observations", x = "Road type")

ggplot(data) +
  geom_boxplot(aes(x=year, y=daily_total))

ggplot(filter(data,year==c(2014, 2015))) +
  geom_boxplot(aes(x=month, y=daily_total, colour=year))

ggplot(data) +
  geom_boxplot(aes(x=public_holiday, y=daily_total))

ggplot(data) +
  geom_boxplot(aes(x=school_holiday, y=daily_total))

a <- data %>%
  group_by(school_holiday) %>%
  mutate(average = mean(daily_total))

ggplot(a) +
  geom_bar(aes(x=school_holiday, y=average), stat="identity")

b <- filter(data,year==2015, month==5)
summary(b$daily_total)

#Observations by percentage

ggplot(data, aes(x = road_functional_hierarchy, fill=road_functional_hierarchy)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  guides(fill=F) +
  labs(title = "Total observations by road type", y = NULL, x = "Road type")

ggplot(data, aes(x = rms_region, fill=rms_region)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  guides(fill=F) +
  labs(title = "Total observations by RMS region", y = NULL, x = "RMS region")

ggplot(data, aes(x = lane_count, fill=lane_count)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  guides(fill=F) +
  labs(title = "Total observations by lane count", y = NULL, x = "Lane count")

ggplot(data, aes(x = road_classification_admin, fill=road_classification_admin)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  guides(fill=F) +
  labs(title = "Total observations by road classification", y = NULL, x = "Road classification")


#from here is junk

b <- a + geom_point(data=count, aes(x=road_functional_hierarchy, y=percent, color="red"))
b

count <- data %>%
  distinct(station_key, road_functional_hierarchy) %>%
  group_by(road_functional_hierarchy) %>%
  summarise(n=n())

count$percent <- (count$n/(sum(count$n))*100)

count

class(count$n)
count$n <- as.numeric(count$n)

count <- data %>%
  group_by(month, year) %>%
  distinct(station_key) %>%
  summarise(n=n())

#THIS TAKES A WHILE
ggplot(data, aes(x=month, y=daily_total, color=year)) +
  geom_bar(stat="identity") +
  facet_wrap(~year) +
  guides(color=F) +
  labs(title = "Total traffic count by month and year", y = "Total traffic count", x = "Month")

#THIS TAKES A WHILE
ggplot(data, aes(x=day_of_week, y=daily_total, color=year)) +
  geom_bar(stat="identity") +
  facet_wrap(~year) +
  guides(color=F) +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(title = "Total traffic count by day of week and year", y = "Total traffic count", x = "Day of week")

#ignore this one
ggplot(data, aes(x = as.factor(road_functional_hierarchy))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Road functional hierarchy - Observations vs Count", y = "Percent", x = "Road type")
