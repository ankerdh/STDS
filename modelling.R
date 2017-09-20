#Load required packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

#Read data
data <- FullData

#Split data into training and testing sets (this needs to be chronological)
set.seed(42)
train <- data[1:(nrow(data)-3),]
test <- data[(nrow(data)-2):nrow(data),]

#Build model
glm_train <- glm(count ~ road_functional_hierarchy +
            road_classification_type +
            lane_count +
            classification_seq +
            rms_region +
            lga +
            suburb + #unnecessary?
            post_code + #unnecessary?
            device_type + #unnecessary?
            wgs84_latitude + #unnecessary?
            wgs84_longitude + #unnecessary?
            traffic_direction_seq +
            cardinal_direction_seq +
            year +
            month +
            day +
            day_of_week +
            public_holiday +
            school_holiday +
            daily_total + #unnecessary?
            hour +
             #WEATHER +
             #ABS ,
           data=train, family=poisson(link="log"))

#Inspect model output
summary(glm_train)

#Plot model diagnostics
par(mfrow = c(2,2))
plot(glm_train)


