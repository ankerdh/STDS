#exploratory visualisations - AJ

install.packages("RDS")
library(RDS)
library(ggplot2)

data <- readRDS("/Users/AD/STDS/FullData.rds")

data$rms_region <- ordered(data$rms_region, levels = c("Sydney", "Hunter", "Northern", "Southern", "South West", "Western"))
data$road_functional_hierarchy <- ordered(data$road_functional_hierarchy, levels = c("Motorway", "Primary Road", "Arterial Road", "Sub-Arterial Road", "Distributor Road", "Local Road"))

ggplot(data) +
  geom_boxplot(aes(y=daily_total,x=road_functional_hierarchy)) + 
  facet_wrap(~rms_region, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

data_2015 <- data[data$year==2015,]

ggplot(data_2015) +
  geom_bar(aes(y=daily_total,x=station_key)) + 
  facet_wrap(~month, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

pairs_data <- data[,c("daily_total", "road_functional_hierarchy", "month", "day_of_week", "Distance_CBD", "pop.density", "pop.work.age.percent", "pop.school.age.percent", "density.vehicles.light")]

pairs_data[,] <- as.numeric(unlist(pairs_data[,]))
pairs_data_small <- pairs_data[1:5000,]

str(pairs_data)

pairs(pairs_data_small)


