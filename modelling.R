#Load required packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

# Retrieve Data
data <- read_rds("/Users/RohanDanisCox/STDS/traffic_with_abs_thin.rds") # need to change to your location

#Split data into training and testing sets (this needs to be chronological)

str(data)
data$daily_total <- as.numeric(data$daily_total)
data$Distance_CBD <- as.numeric(data$Distance_CBD)
data$school_holiday <- as.factor(data$school_holiday)
# Need to change 0s into FALSE and 1 into true
data$public_holiday <- as.factor(data$public_holiday)
# Need to change 0s into FALSE and 1 into true
data$year <- as.integer(data$year)
data$permanent_station <- as.factor(data$permanent_station)
data$device_type <- as.factor(data$device_type)
data$rms_region <- as.factor(data$rms_region)
data$lane_count <- as.factor(data$lane_count)
data$road_classification_admin <- as.factor(data$road_classification_admin)
data$mab_way_type <- as.factor(data$mab_way_type)

test <- data %>%
  filter(year==2016)
train <- data %>%
  filter(year<2016)

#Build model
#train <- train %>%
  #select(road_functional_hierarchy,daily_total)

glm_model <- glm(daily_total~Distance_CBD + lane_count + pop.density + pop.work.age.percent,
                 data=train,family=poisson(link=log))
dispersiontest(glm_model)


lm_model <- lm(daily_total~Distance_CBD + lane_count + pop.density + pop.work.age.percent,
                 data=train)



summary.glm(glm_model)
summary(lm_model)
summary(data)
mab_way_type +
  road_classification_admin +
  lane_count +
  cardinal_direction_seq +
  classification_seq +
  rms_region +
  lga +
  suburb +
  post_code +
  device_type +
  permanent_station +
  wgs84_latitude +
  wgs84_longitude +
  year +
  month +
  day +
  day_of_week +
  public_holiday +
  school_holiday +
  Distance_CBD +
  nearest_weather_station +
  lat +
  lon +
  pop.density +
  pop.work.age.percent +
  pop.school.age.percent +
  density.vehicles.light +
  density.vehicles.heavy,


Control = trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE)
#Inspect model output
summary(glm_train)

#Plot model diagnostics
par(mfrow = c(2,2))
plot(glm_train)


