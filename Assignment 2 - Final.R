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
library(AER)
library(caret)
library(MASS)
library(arm)

#########################################################
############ DATA ACQUISITION AND CLEANING ##############
#########################################################

# GET the full stations database using the API
stations_api<- GET("https://api.transport.nsw.gov.au/v1/roads/spatial?format=geojson&q=select%20*%20from%20road_traffic_counts_station_reference%20",
                   verbose(), 
                   encode="json", 
                   add_headers(`Authorization` = "apikey fUa8N1LC42AYtVDKIt6jbAzQXFPcf9b31GYv"))

# Extract a clean stations dataframe from the raw API output
stations_raw <- rawToChar(stations_api$content)
stations_clean <- fromJSON(stations_raw)
stations_df <- as.data.frame(stations_clean[[2]])
stations <- as.data.frame(stations_df$properties)

saveRDS(stations,file= "Stations-initial.rds")


# compare each station location to BOM weather stations and choose closest (SOME ARE WRONG!)
BOMrain <- read_rds("/Users/RohanDanisCox/STDS/BOMNSW.RDS") # need to change to your location
BOMrainStation <- data.frame(subset(BOMrain, select = c(1,2,4,5)))
colnames(BOMrainStation) <- c("StationNum","name", "lat", "lon")
UniqueBOMStations <- sqldf('SELECT DISTINCT * FROM BOMrainStation') 

saveRDS(UniqueBOMStations,file= "UniqueBOMStations.rds")

# create distance matrix
DistMatrix <- distm(stations[,c('wgs84_longitude','wgs84_latitude')], UniqueBOMStations[,c('lon','lat')], fun=distVincentyEllipsoid)/1000
NearestStation <- data.frame(UniqueBOMStations$StationNum[max.col(-DistMatrix)])

# Only take the unique BOM Stations required
UniqueNearestStation<-unique(NearestStation)

# Combine with traffic stations information
stations$nearest_weather_station <- NearestStation$UniqueBOMStations.StationNum.max.col..DistMatrix..
stations <- inner_join(stations, UniqueBOMStations,c("nearest_weather_station" = "StationNum"))

saveRDS(stations,file= "StationsAndWeather.rds")

# Split traffic stations to allow for separate calls to Googlemaps distance API
station.locations <- stations[,c("station_key","wgs84_latitude","wgs84_longitude")]
station.locations.200 <- station.locations %>%
  filter(row_number() %in% 1:200)
station.locations.400 <- station.locations %>%
  filter(row_number() %in% 201:400)
station.locations.600 <- station.locations %>%
  filter(row_number() %in% 401:600)
station.locations.800 <- station.locations %>%
  filter(row_number() %in% 601:800)
station.locations.1000 <- station.locations %>%
  filter(row_number() %in% 801:1000)
station.locations.1200 <- station.locations %>%
  filter(row_number() %in% 1001:1200)
station.locations.1400 <- station.locations %>%
  filter(row_number() %in% 1201:1400)
station.locations.1600 <- station.locations %>%
  filter(row_number() %in% 1401:1600)
station.locations.1800 <- station.locations %>%
  filter(row_number() %in% 1601:1800)
station.locations.1818 <- station.locations %>%
  filter(row_number() %in% 1801:1823)

# Create function to calculate distance by road to Sydney Tower
get.distance <- function(x,y,z) {
  results = gmapsdistance(
    origin = paste(x,y,sep=","), 
    destination = "-33.8704512,151.2058792", 
    mode = "driving", 
    traffic_model = "best_guess",
    shape = "long",
    key = "AIzaSyCwoq2tqj0RX54SrVnnRYccCs6AV1kHj34")
  results.df<-data.frame(results)
  results.df$station_key <- z
  results.df
}

# Calculate distance for each station
distance.df200 <- get.distance(station.locations.200$wgs84_latitude,station.locations.200$wgs84_longitude, station.locations.200$station_key)
distance.df400 <- get.distance(station.locations.400$wgs84_latitude,station.locations.400$wgs84_longitude, station.locations.400$station_key)
distance.df600 <- get.distance(station.locations.600$wgs84_latitude,station.locations.600$wgs84_longitude, station.locations.600$station_key)
distance.df800 <- get.distance(station.locations.800$wgs84_latitude,station.locations.800$wgs84_longitude, station.locations.800$station_key)
distance.df1000 <- get.distance(station.locations.1000$wgs84_latitude,station.locations.1000$wgs84_longitude, station.locations.1000$station_key)
distance.df1200 <- get.distance(station.locations.1200$wgs84_latitude,station.locations.1200$wgs84_longitude, station.locations.1200$station_key)
distance.df1400 <- get.distance(station.locations.1400$wgs84_latitude,station.locations.1400$wgs84_longitude, station.locations.1400$station_key)
distance.df1600 <- get.distance(station.locations.1600$wgs84_latitude,station.locations.1600$wgs84_longitude, station.locations.1600$station_key)
distance.df1800 <- get.distance(station.locations.1800$wgs84_latitude,station.locations.1800$wgs84_longitude, station.locations.1800$station_key)
distance.df1818 <- get.distance(station.locations.1818$wgs84_latitude,station.locations.1818$wgs84_longitude, station.locations.1818$station_key)
combine <- rbind(distance.df200,distance.df400,distance.df600,distance.df800,distance.df1000,distance.df1200,distance.df1400,distance.df1600,distance.df1800,distance.df1818)

# Establish dataframe of distances and merge to stations
distances.table <- combine[,c("station_key", "Distance.Distance")]
stations <- inner_join(stations,distances.table,by="station_key")
stations <- rename(stations,"Distance_CBD" ="Distance.Distance")

# Save file and access 
saveRDS(stations,file= "StationsWeatherAndDistance.rds")
stations <- read_rds("/Users/RohanDanisCox/STDS/StationsWeatherAndDistance.rds") # need to change to your location

# Loop to access permanent stations traffic counts NSW Transport API (monthly to access full dataset) and store 
year <- 2011:2016
month <- 1:12
perm_count_wide <-data.frame()
for(i in year) {
    for (j in month){
      url<- paste("https://api.transport.nsw.gov.au/v1/roads/spatial?format=geojson&q=select%20*%20from%20road_traffic_counts_hourly_permanent%20where%20year%20%3D%20",i,"%20and%20month%20%3D%20",j,"%20",sep="")
      perm_count_api <- GET(url,
                            verbose(),
                            encode="json",
                            add_headers(`Authorization` = "apikey fUa8N1LC42AYtVDKIt6jbAzQXFPcf9b31GYv"))
      perm_count_raw <- rawToChar(perm_count_api$content)
      perm_count_clean <- fromJSON(perm_count_raw)
      perm_count_df <- as.data.frame(perm_count_clean[[2]])
      perm_count_wide <- rbind(perm_count_wide,perm_count_df$properties)
    }}

# Save the compiled file as an RDS
saveRDS(perm_count_wide,file="perm_count_wide.rds")

# Loop to access sample stations traffic counts NSW Transport API (monthly to access full dataset) and store 
samp_count_wide <-data.frame()
for(i in year) {
  for (j in month){
    url<- paste("https://api.transport.nsw.gov.au/v1/roads/spatial?format=geojson&q=select%20*%20from%20road_traffic_counts_hourly_sample%20where%20year%20%3D%20'",i,"'%20and%20month%20%3D%20'",j,"'",sep="")
    samp_count_api <- GET(url,
                          verbose(),
                          encode="json",
                          add_headers(`Authorization` = "apikey fUa8N1LC42AYtVDKIt6jbAzQXFPcf9b31GYv"))
    samp_count_raw <- rawToChar(samp_count_api$content)
    samp_count_clean <- fromJSON(samp_count_raw)
    samp_count_df <- as.data.frame(samp_count_clean[[2]])
    samp_count_wide <- rbind(samp_count_wide,samp_count_df$properties)
  }}

# combine the permanent stations with the sample stations
count_wide <- rbind(perm_count_wide,samp_count_wide)

# add a monthly day count
count_wide <- count_wide%>%
  mutate(day=mday(date))

# Fix station_key to be an integer ready for merging
count_wide$station_key <- as.integer(count_wide$station_key)

# Merge the two tables using the key identifier of Station key - inner join returns where a match is found
traffic_vol <- inner_join(count_wide,stations,by = "station_key")

# Save and Retrieve
saveRDS(traffic_vol,file= "traffic_vol.rds")
traffic_vol <- read_rds("/Users/RohanDanisCox/STDS/traffic_vol.rds") # need to change to your location

# Reclassify certain factor variables to labels from NSW Transport data documentation
traffic_vol$classification_seq <- factor(traffic_vol$classification_seq,levels=c(0,1,2,3,-9),labels = c("UNCLASSIFIED","ALL VEHICLES","LIGHT VEHICLES","HEAVY VEHICLES","MISSING"))
traffic_vol$traffic_direction_seq <- factor(traffic_vol$traffic_direction_seq,levels=c(0,1,2),labels = c("COUNTER","PRESCRIBED","BOTH"))
traffic_vol$cardinal_direction_seq <- factor(traffic_vol$cardinal_direction_seq,levels=c(1,3,5,7,9,10),labels = c("NORTH","EAST","SOUTH","WEST","NORTHBOUND AND SOUTHBOUND","EASTBOUND & WESTBOUND"))
traffic_vol$day_of_week <- factor(traffic_vol$day_of_week,levels=c(1,2,3,4,5,6,7),labels = c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY"))

# Read in selected variables from the ABS table ABS_REGIONAL_LGA
LGAurl <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ABS_REGIONAL_LGA/ERP_18+ERP_21+ERP_6+200IND+MVC_14+MVC_15+MVC_16+MVC_17+MVC_18+MVC_19+MVC_20+MVC_21+MVC_22+MVC_38+CABEE_36+CABEE_35+INCOME_17+LAND.LGA2014..A/all?startTime=2011&endTime=2016&format=compact_v2"

LGA_raw <- readSDMX(LGAurl)
LGA <- as.data.frame(LGA_raw)

# Names
LGAdsd_url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/ABS_REGIONAL_LGA"
LGAdsd <- readSDMX(LGAdsd_url)

# Get codelists from DSD
LGAcls <- slot(LGAdsd, "codelists")
LGAcodelists <- sapply(slot(LGAcls, "codelists"), function(x) slot(x, "id")) #get list of codelists
LGAcodelists
LGAcodelist <- as.data.frame(slot(LGAdsd, "codelists"), codelistId = "CL_ABS_REGIONAL_LGA_REGION") #get a codelist

# Get concepts from DSD
LGAconcepts <- as.data.frame(slot(LGAdsd, "concepts"))

# Join
LGA_full <- inner_join(LGA,LGAcodelist,c("REGION" = "id")) 

# Transform LGA region names into correct format to match other data
LGA_full$label.en <- gsub(" \\(.*$","", LGA_full$label.en)

# Apply 2014 LGA land area to all years
land.area.2014 <- LGA_full[LGA_full$MEASURE=="LAND",c("REGION","OBS_VALUE")]
LGA_full <- merge(x = LGA_full, y = land.area.2014, by = "REGION", all.x = TRUE)

# Make a wide table
LGA.wide <- dcast(LGA_full, REGION + label.en + TIME + OBS_VALUE.y ~ MEASURE, value.var = c("OBS_VALUE.x"))

# Change col names
setnames(LGA.wide,c("label.en","TIME", "OBS_VALUE.y","ERP_21","ERP_18", "ERP_6", "CABEE_36", "CABEE_35", "INCOME_17"),c("lga","year","lga.area.2014","pop.density","pop.work.age.percent","pop.school.age.percent","transport.businesses", "total.businesses", "median.income"))

# Change variable classes 
LGA.wide$year <- as.integer(LGA.wide$year)
LGA.wide[,c("lga.area.2014","pop.density","pop.work.age.percent","pop.school.age.percent","MVC_14","MVC_15","MVC_16","MVC_17","MVC_18","MVC_19","MVC_20","MVC_21","MVC_22","transport.businesses", "total.businesses", "median.income")] <- as.numeric(unlist(LGA.wide[,c("lga.area.2014","pop.density","pop.work.age.percent","pop.school.age.percent","MVC_14","MVC_15","MVC_16","MVC_17","MVC_18","MVC_19","MVC_20","MVC_21","MVC_22","transport.businesses", "total.businesses", "median.income")]))

# Create light vehicle and heavy vehicle stats
LGA.wide$vehicles.light <- sum(LGA.wide$MVC_14, LGA.wide$MVC_15, LGA.wide$MVC_16, LGA.wide$MVC_17, LGA.wide$MVC_22, na.rm=TRUE)
LGA.wide$vehicles.heavy <- sum(LGA.wide$MVC_18, LGA.wide$MVC_19, LGA.wide$MVC_20, LGA.wide$MVC_21, na.rm=TRUE)

# Create densities for vehicles, businesses
LGA.wide$density.vehicles.light <- LGA.wide$vehicles.light / LGA.wide$lga.area.2014
LGA.wide$density.vehicles.heavy <- LGA.wide$vehicles.heavy / LGA.wide$lga.area.2014
LGA.wide$density.transport.businesses <- LGA.wide$transport.businesses / LGA.wide$lga.area.2014
LGA.wide$density.total.businesses <- LGA.wide$total.businesses / LGA.wide$lga.area.2014

# Subset the final columns we need
LGA.wide <- LGA.wide[,c("lga","year","lga.area.2014","pop.density","pop.work.age.percent","pop.school.age.percent","density.vehicles.light","density.vehicles.heavy","density.transport.businesses","density.total.businesses","median.income")]

saveRDS(LGA.wide,file="LGA_wide.rds")
LGA.wide <- read_rds("/Users/RohanDanisCox/STDS/LGA_wide.rds") # need to change to your location

# Left join to add ABS data for LGA/Year to each row of traffic data
traffic.with.abs <- merge(x = traffic_vol, y = LGA.wide, by = c("lga", "year"), all.x = TRUE)
traffic_with_abs <- traffic.with.abs

# Save and Retrieve 
saveRDS(traffic.with.abs,file= "traffic_with_abs.rds")
traffic_with_abs <- read_rds("/Users/RohanDanisCox/STDS/traffic_with_abs.rds") # need to change to your location

# Select the variables of interest in the dataframe (exclude unnecessary variables)
names(traffic_with_abs)
data <- traffic_with_abs %>%
  select(station_key, 
         full_name, 
         road_functional_hierarchy,
         mab_way_type,
         road_classification_admin,
         lane_count,
         cardinal_direction_seq,
         classification_seq,
         rms_region,
         lga,
         suburb,
         post_code,
         device_type,
         permanent_station,
         wgs84_latitude,
         wgs84_longitude,
         year,
         month,
         day,
         day_of_week,
         public_holiday,
         school_holiday,
         Distance_CBD,
         nearest_weather_station,
         lat,
         lon,
         pop.density,
         pop.work.age.percent,
         pop.school.age.percent,
         density.vehicles.light,
         density.vehicles.heavy,
         daily_total)

# Clean the variables
data$daily_total <- as.integer(data$daily_total)
data$Distance_CBD <- as.numeric(data$Distance_CBD)
data$school_holiday[data$school_holiday == 0] <- FALSE
data$school_holiday[data$school_holiday == 1] <- TRUE
data$school_holiday <- as.factor(data$school_holiday)
data$public_holiday[data$public_holiday == 0] <- FALSE
data$public_holiday[data$public_holiday == 1] <- TRUE
data$public_holiday <- as.factor(data$public_holiday)
data$year <- as.integer(data$year)
data$permanent_station <- as.factor(data$permanent_station)
data$device_type <- as.factor(data$device_type)
data$rms_region <- as.factor(data$rms_region)
data$lane_count <- as.factor(data$lane_count)
data$road_classification_admin <- as.factor(data$road_classification_admin)
data$mab_way_type <- as.factor(data$mab_way_type)
data$road_functional_hierarchy <- as.factor(data$road_functional_hierarchy)
data$road_functional_hierarchy <- trim(data$road_functional_hierarchy)

# Fixing up duplicates
data <- data %>%
  distinct(station_key,year,month,day,cardinal_direction_seq,classification_seq,.keep_all = TRUE)

# Removing impact of classification
data <- data %>% 
  group_by(station_key,full_name,road_functional_hierarchy,mab_way_type,road_classification_admin,
           lane_count,cardinal_direction_seq,rms_region,lga,suburb,post_code,device_type,permanent_station,
           wgs84_latitude,wgs84_longitude,year,month,day,day_of_week,public_holiday,school_holiday,
           Distance_CBD,nearest_weather_station,lat,lon,pop.density,pop.work.age.percent,
           pop.school.age.percent,density.vehicles.light,density.vehicles.heavy) %>%
  summarise(daily_total=sum(daily_total))

# Store and Retrieve Data
saveRDS(data,file= "FullData.rds")
FullData <- read_rds("/Users/RohanDanisCox/STDS/FullData.rds") # fix to your computer location

# Adding in weather data
BOMNSW <- read_rds("/Users/RohanDanisCox/STDS/BOMNSW.RDS")
str(BOMNSW)

BOMNSW$Date1<-dmy(BOMNSW$Day.Month.Year.in.DD.MM.YYYY.format)
BOMNSW$Year1<-year(BOMNSW$Date1)
BOMNSW$Month1<-month(BOMNSW$Date1)
BOMNSW$Day1<-day(BOMNSW$Date1)
BOMNSW$Key1<-paste(BOMNSW$Station.Number,"_",BOMNSW$Date1)

# Subset BOM file for relevant columns
RainByDay <- data.frame(subset(BOMNSW, select = c(1,2,3,4,5,8,17,19,20,18,21)))
colnames(RainByDay) <- c("StationNum","name", "Locality","lat", "lon", "rain","Year1","Month1", "Day1", "Date1","Key1")

RainCount <- RainByDay %>%
  group_by(Key1) %>%
  mutate(DailyRain=sum(rain, na.rm = TRUE)) %>%
  distinct(StationNum,name,Locality,lat,lon,Year1, Month1,Day1, Date1, DailyRain)

# Fix data for merge
FullData$date <- as.Date(with(FullData, paste(year, month, day,sep="-")), "%Y-%m-%d")
FullData$Key1<-paste(FullData$nearest_weather_station,"_",FullData$date)
FullDataRain<-merge(FullData,RainCount, by="Key1")

# Save
saveRDS(FullDataRain,file= "FullDataRain.rds")

# Fixing columns
FinalData <- FullDataRain %>%
  select(-c(Key1,hour_01:hour_23,StationNum:Date1)) %>%
  select(c(station_key:wgs84_longitude,Distance_CBD:lon.x,pop.density:density.vehicles.heavy,
           year:day,date,day_of_week:school_holiday,DailyRain,daily_total))

# Fix ABS figures for 2016 which were missing from the data
LGA2015 <- LGA.wide %>%
  filter(year<2016)
LGA2016 <- LGA.wide %>%
  filter(year==2015)
LGA2016 <- LGA2016 %>%
  mutate(year2=(year+1)) %>%
  select(lga,year2,pop.density,pop.work.age.percent,pop.work.age.percent,pop.school.age.percent,density.vehicles.light,density.vehicles.heavy)
LGA2015 <- LGA2015 %>%
  select(lga,year,pop.density,pop.work.age.percent,pop.work.age.percent,pop.school.age.percent,density.vehicles.light,density.vehicles.heavy)
LGA2016 <- rename(LGA2016, year=year2)
LGAfix <- rbind(LGA2015,LGA2016)

Check <- FinalData %>%
  select(-c(pop.density,pop.work.age.percent,pop.work.age.percent,pop.school.age.percent,density.vehicles.light,density.vehicles.heavy))
  
Check2 <- merge(x = Check, y = LGAfix, by = c("lga", "year"), all.x = TRUE)
FinalData<-Check2

# Remove Null values for RMS_region
FinalData <- FinalData %>%
  drop_na()
FinalData$rms_region <- factor(FinalData$rms_region)

# Save and Retrieve Data
saveRDS(FinalData,file= "FinalData.rds")
FinalData <- read_rds("/Users/RohanDanisCox/STDS/FinalData.rds") # fix to your computer location

#########################################################
################       MODELLING     ####################
#########################################################

# Subset to data following April 2015
DataApril2015 <- FinalData %>%
  filter(date>"2015-03-31")

# Save and Retrieve
saveRDS(DataApril2015,file= "DataApril2015.rds")
DataApril2015 <- read_rds("/Users/RohanDanisCox/STDS/DataApril2015.rds") # fix to your computer location

# Split into Holdout and Working sets
set.seed(42)
train = createDataPartition(y = DataApril2015$daily_total, p = 0.80, list = F)
Working = DataApril2015[train,]
Holdout = DataApril2015[-train,]

# Split Working set into training and test set
set.seed(42)
train1 = createDataPartition(y = Working$daily_total, p = 0.70, list = F)
trainset = Working[train1,]
testset = Working[-train1,]

# Full model
QP_Full <- glm(daily_total~ Distance_CBD + road_functional_hierarchy + day_of_week + public_holiday + DailyRain + school_holiday+
                    pop.density + density.vehicles.light + pop.work.age.percent + year + month + day + date + rms_region,
                  data=trainset,family=quasipoisson(link=log)) 

NB_Full <-glm.nb(daily_total~ Distance_CBD + road_functional_hierarchy + day_of_week + public_holiday + DailyRain + school_holiday+
                            pop.density + density.vehicles.light + pop.work.age.percent + year + month + day + date + rms_region,
                          data=trainset) 

# Consider summary for each model
summary.glm(QP_Full)
summary.glm(NB_Full)

# Model Selection -> via forward selection
variables <- c("DailyRain","pop.density","day_of_week","road_functional_hierarchy","density.vehicles.light","pop.work.age.percent",
              "year","month","day","Distance_CBD","public_holiday","school_holiday","date","rms_region")
Variable1 <-data.frame()
for(i in variables) {
    model <- glm(paste("daily_total~", i[[1]]),
                 data=trainset,family=quasipoisson(link=log))
    prediction <- exp(predict(model,newdata = testset))
    check <- cbind(testset,prediction)
    SummaryTest <- check %>%
      mutate(error=sqrt((daily_total-prediction)^2)) %>%
      summarise(average=mean(error,na.rm=TRUE))
    bind <- cbind(i,SummaryTest)
    Variable1 <- rbind(Variable1,bind)
}
# Variable 1 = Distance_CBD
variables2 <- variables <- c("DailyRain","pop.density","day_of_week","road_functional_hierarchy","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","public_holiday","school_holiday","date","rms_region")
Variable2 <-data.frame()
for(i in variables2) {
  model <- glm(paste("daily_total~ Distance_CBD +", i[[1]]),
               data=trainset,family=quasipoisson(link=log))
  prediction <- exp(predict(model,newdata = testset))
  check <- cbind(testset,prediction)
  SummaryTest <- check %>%
    mutate(error=sqrt((daily_total-prediction)^2)) %>%
    summarise(average=mean(error,na.rm=TRUE))
  bind <- cbind(i,SummaryTest)
  Variable2 <- rbind(Variable2,bind)
}
# Variable 2 = road_functional_heirarchy
variables3 <- variables <- c("DailyRain","pop.density","day_of_week","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","public_holiday","school_holiday","date","rms_region")
Variable3 <-data.frame()
for(i in variables3) {
  model <- glm(paste("daily_total~ Distance_CBD + road_functional_hierarchy +", i[[1]]),
               data=trainset,family=quasipoisson(link=log))
  prediction <- exp(predict(model,newdata = testset))
  check <- cbind(testset,prediction)
  SummaryTest <- check %>%
    mutate(error=sqrt((daily_total-prediction)^2)) %>%
    summarise(average=mean(error,na.rm=TRUE))
  bind <- cbind(i,SummaryTest)
  Variable3 <- rbind(Variable3,bind)
}
# Variable 3 = rms_region
variables4 <- variables <- c("DailyRain","pop.density","day_of_week","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","public_holiday","school_holiday","date")
Variable4 <-data.frame()
for(i in variables4) {
  model <- glm(paste("daily_total~ Distance_CBD + road_functional_hierarchy + rms_region +", i[[1]]),
               data=trainset,family=quasipoisson(link=log))
  prediction <- exp(predict(model,newdata = testset))
  check <- cbind(testset,prediction)
  SummaryTest <- check %>%
    mutate(error=sqrt((daily_total-prediction)^2)) %>%
    summarise(average=mean(error,na.rm=TRUE))
  bind <- cbind(i,SummaryTest)
  Variable4 <- rbind(Variable4,bind)
}
# Variable 4 = day_of_week
variables5 <- variables <- c("DailyRain","pop.density","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","public_holiday","school_holiday","date")
Variable5 <-data.frame()
for(i in variables5) {
  model <- glm(paste("daily_total~ Distance_CBD + road_functional_hierarchy + rms_region + day_of_week +", i[[1]]),
               data=trainset,family=quasipoisson(link=log))
  prediction <- exp(predict(model,newdata = testset))
  check <- cbind(testset,prediction)
  SummaryTest <- check %>%
    mutate(error=sqrt((daily_total-prediction)^2)) %>%
    summarise(average=mean(error,na.rm=TRUE))
  bind <- cbind(i,SummaryTest)
  Variable5 <- rbind(Variable5,bind)
}
# Variable 5 = public_holiday
variables6 <- variables <- c("DailyRain","pop.density","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","school_holiday","date")
Variable6 <-data.frame()
for(i in variables6) {
  model <- glm(paste("daily_total~ Distance_CBD + road_functional_hierarchy + rms_region + day_of_week + public_holiday + ", i[[1]]),
               data=trainset,family=quasipoisson(link=log))
  prediction <- exp(predict(model,newdata = testset))
  check <- cbind(testset,prediction)
  SummaryTest <- check %>%
    mutate(error=sqrt((daily_total-prediction)^2)) %>%
    summarise(average=mean(error,na.rm=TRUE))
  bind <- cbind(i,SummaryTest)
  Variable6 <- rbind(Variable6,bind)
}
# Variable 6 = DailyRain
variables7 <- variables <- c("pop.density","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","school_holiday","date")
Variable7 <-data.frame()
for(i in variables7) {
  model <- glm(paste("daily_total~ Distance_CBD + road_functional_hierarchy + rms_region + day_of_week + public_holiday + DailyRain + ", i[[1]]),
               data=trainset,family=quasipoisson(link=log))
  prediction <- exp(predict(model,newdata = testset))
  check <- cbind(testset,prediction)
  SummaryTest <- check %>%
    mutate(error=sqrt((daily_total-prediction)^2)) %>%
    summarise(average=mean(error,na.rm=TRUE))
  bind <- cbind(i,SummaryTest)
  Variable7 <- rbind(Variable7,bind)
}
# Variable 7 = school_holiday STOP -> No further benefit - This code was also completed for a negative binomial model

# Final Model 
QP_Final <- glm(daily_total~ Distance_CBD + road_functional_hierarchy +rms_region + day_of_week + public_holiday + DailyRain + school_holiday,
                   data=trainset,family=quasipoisson(link=log)) 
NB_Final <- glm.nb(daily_total~ Distance_CBD + road_functional_hierarchy + rms_region + day_of_week + public_holiday + DailyRain + school_holiday,
                   data=trainset) 

# Summary of Final Models
summary.glm(QP_Final)
summary.glm(NB_Final)

# Considering performance on the testset
QP_prediction <- exp(predict.glm(QP_Final,newdata = testset))
NB_prediction <- exp(predict.glm(NB_Final,newdata = testset))
testset <- cbind(testset,QP_prediction,NB_prediction)
SummaryTest <- testset %>%
  mutate(QP_error=sqrt((daily_total-QP_prediction)^2),NB_error=sqrt((daily_total-NB_prediction)^2)) %>%
  summarise(QP_average=mean(QP_error,na.rm=TRUE),NB__average=mean(NB_error,na.rm=TRUE))

# Considering performance on the holdout set
prediction <- exp(predict.glm(QP_Final,newdata = Holdout))
Holdout <- cbind(Holdout,prediction)
SummaryHoldout <- Holdout %>%
  mutate(Error=sqrt((daily_total-prediction)^2)) %>%
  summarise(AverageError=mean(Error,na.rm=TRUE))

# Consider performance on holdout set by RMS region
RMS_Region_Summary <- Holdout %>%
  mutate(Error=sqrt((daily_total-prediction)^2)) %>%
  group_by(rms_region) %>%
  summarise(Average_Error = mean(Error))
write.csv(RMS_Region_Summary,"RMSerror.csv")

# Consider performance on holdout set by Road Functional Hierarchy
RoadHierarchy <- Holdout %>%
  mutate(Error=sqrt((daily_total-prediction)^2)) %>%
  group_by(road_functional_hierarchy) %>%
  summarise(Average_Error = mean(Error))
write.csv(RoadHierarchy,"RoadHierarchy.csv")

# Consider performance on holdout set by Day of week
DayOfWeek <- Holdout %>%
  mutate(Error=sqrt((daily_total-prediction)^2)) %>%
  group_by(day_of_week) %>%
  summarise(Average_Error = mean(Error))
write.csv(DayOfWeek,"DayOfWeek.csv")

# Consider performance on holdout set by rainfall groups
summary(Holdout$Distance_CBD)

Distance <- Holdout %>%
  mutate(DtoCBD= cut(Holdout$Distance_CBD,c(0,5000,20000,40000,100000,250000, Inf),labels=c("Within 5km","5km to 20km","20km to 40km","40km to 100km","100km to 250km", "Greater than 250km"))) %>%
  mutate(Error=sqrt((daily_total-prediction)^2)) %>%
  group_by(DtoCBD) %>%
  summarise(Average_Error = mean(Error))
write.csv(Distance,"DtoCBD.csv")

# Consider performance on holdout set by rainfall groups
Rain <- Holdout %>%
  mutate(rainclassification = cut(Holdout$DailyRain,c(-1,5,10,20,50, Inf),labels=c("No Rain","Sprinkle","Rain","Heavy Rain", "Torrential Rain"))) %>%
  mutate(Error=sqrt((daily_total-prediction)^2)) %>%
  group_by(rainclassification) %>%
  summarise(Average_Error = mean(Error))
write.csv(Rain,"Rain.csv")


#########################################################
################    VISUALISATIONS    ###################
#########################################################



#### INCLUDE VISUALISATIONS HERE ####



# Visualising predicted vs actuals
ggplot(Holdout,aes(x=daily_total,y=prediction,colour=road_functional_hierarchy))+
  geom_point(size=0.5,alpha=0.4) +
  geom_line()
  guides(colour = guide_legend(override.aes = list(alpha=1,size=5))) +
  labs(y="QuasiPoisson Predicted Daily Count",x="Actual Daily Count") + 
  theme(axis.title = element_text(size=20)) +
  theme(legend.title = element_text(size=20)) +
  theme(legend.text = element_text(size=16))+
  theme(text=element_text(size=16)) 

# Visualising the distribution by Day of Week
ggplot(Holdout) +
  geom_jitter(aes(x=day_of_week,y=prediction),colour="red",size=0.2,alpha=0.1) +
  geom_jitter(aes(x=day_of_week,y=daily_total),colour="blue",size=0.2,alpha=0.1) +
  labs(y="Daily Count (Predicted = Red, Actuals = Blue)",x="Day of Week") + 
  theme(axis.title = element_text(size=20)) +
  theme(legend.title = element_text(size=20)) +
  theme(legend.text = element_text(size=16))+
  theme(text=element_text(size=16)) 

# Visualising for rainfall
Rain <- Holdout %>%
  filter(DailyRain>5)
ggplot(Rain,aes(x=daily_total,y=prediction,colour=DailyRain))+
  geom_point(size=3,alpha=0.3) +
  scale_color_gradient2(low="white",mid="blue",high ="darkblue",midpoint=40)+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=5))) +
  labs(y="QuasiPoisson Predicted Daily Count",x="Actual Daily Count") + 
  theme(axis.title = element_text(size=20)) +
  theme(legend.title = element_text(size=20)) +
  theme(legend.text = element_text(size=16))+
  theme(text=element_text(size=16)) 
