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

#### Distance to City --- DO NOT USE THIS SHIT----
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

#calculate distance for each station
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

distances.table <- combine[,c("station_key", "Distance.Distance")]
stations <- inner_join(stations,distances.table,by="station_key")
stations <- rename(stations,"Distance_CBD" ="Distance.Distance")
saveRDS(stations,file= "StationsWeatherAndDistance.rds")

# Open this instead 
stations <- read_rds("/Users/RohanDanisCox/STDS/StationsWeatherAndDistance.rds") # need to change to your location

# Cycle through each month and store in perm_count_wide
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

# Store the compiled file as an RDS
saveRDS(perm_count_wide,file="perm_count_wide.rds")

# Cycle through each month and store in samp_count_wide
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

# Seems abnormally low at only 35,597 rows but I checked this by running the full time period (237,000) rows
# and then subsetting to only 2011 onwward which pulled up the exact same amount. I then riffled through
# the data to eye test it and it does appear as though the vast majority of sample stations were taken 
# before 2011. This makes intuitive sense in that increased tech would make permanent stations easier.

# combine the permanent stations with the sample stations
count_wide <- rbind(perm_count_wide,samp_count_wide)

# add the a monthly day count
count_wide <- count_wide%>%
  mutate(day=mday(date))

# Fix station_key to be an integer ready for merging
count_wide$station_key <- as.integer(count_wide$station_key)

# Merge the two tables using the key identifier of Station key - inner join returns where a match is found
traffic_vol <- inner_join(count_wide,stations,by = "station_key")

# Store the traffic_vol file as an RDS
saveRDS(traffic_vol,file= "traffic_vol.rds")

# Retrieve Data
traffic_vol <- read_rds("/Users/RohanDanisCox/STDS/traffic_vol.rds") # need to change to your location

## classification_seq = 0: UNCLASSIFIED 1: ALL VEHICLES 2: LIGHT VEHICLES 3: HEAVY VEHICLES -9: MISSING    
## traffic_direction_seq = 0: COUNTER 1: PRESCRIBED 2: BOTH -- not sure if this is needed
## cardinal_direction_seq = 1: NORTH 3: EAST 5: SOUTH 7: WEST 9: NORTHBOUND AND SOUTHBOUND 10: EASTBOUND AND WESTBOUND

## update factors based on above characteristics
traffic_vol$classification_seq <- factor(traffic_vol$classification_seq,levels=c(0,1,2,3,-9),labels = c("UNCLASSIFIED","ALL VEHICLES","LIGHT VEHICLES","HEAVY VEHICLES","MISSING"))
traffic_vol$traffic_direction_seq <- factor(traffic_vol$traffic_direction_seq,levels=c(0,1,2),labels = c("COUNTER","PRESCRIBED","BOTH"))
traffic_vol$cardinal_direction_seq <- factor(traffic_vol$cardinal_direction_seq,levels=c(1,3,5,7,9,10),labels = c("NORTH","EAST","SOUTH","WEST","NORTHBOUND AND SOUTHBOUND","EASTBOUND & WESTBOUND"))
traffic_vol$day_of_week <- factor(traffic_vol$day_of_week,levels=c(1,2,3,4,5,6,7),labels = c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY"))

#read in selected variables from the ABS table ABS_REGIONAL_LGA
LGAurl <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ABS_REGIONAL_LGA/ERP_18+ERP_21+ERP_6+200IND+MVC_14+MVC_15+MVC_16+MVC_17+MVC_18+MVC_19+MVC_20+MVC_21+MVC_22+MVC_38+CABEE_36+CABEE_35+INCOME_17+LAND.LGA2014..A/all?startTime=2011&endTime=2016&format=compact_v2"

LGA_raw <- readSDMX(LGAurl)
LGA <- as.data.frame(LGA_raw)

# Names
LGAdsd_url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/ABS_REGIONAL_LGA"
LGAdsd <- readSDMX(LGAdsd_url)

#get codelists from DSD
LGAcls <- slot(LGAdsd, "codelists")
LGAcodelists <- sapply(slot(LGAcls, "codelists"), function(x) slot(x, "id")) #get list of codelists
LGAcodelists
LGAcodelist <- as.data.frame(slot(LGAdsd, "codelists"), codelistId = "CL_ABS_REGIONAL_LGA_REGION") #get a codelist
#get concepts from DSD
LGAconcepts <- as.data.frame(slot(LGAdsd, "concepts"))
#join
LGA_full <- inner_join(LGA,LGAcodelist,c("REGION" = "id")) 

#transform LGA region names into correct format to match other data
LGA_full$label.en <- gsub(" \\(.*$","", LGA_full$label.en)

#apply 2014 LGA land area to all years
land.area.2014 <- LGA_full[LGA_full$MEASURE=="LAND",c("REGION","OBS_VALUE")]
LGA_full <- merge(x = LGA_full, y = land.area.2014, by = "REGION", all.x = TRUE)

#make a wide table
LGA.wide <- dcast(LGA_full, REGION + label.en + TIME + OBS_VALUE.y ~ MEASURE, value.var = c("OBS_VALUE.x"))

#change col names
setnames(LGA.wide,c("label.en","TIME", "OBS_VALUE.y","ERP_21","ERP_18", "ERP_6", "CABEE_36", "CABEE_35", "INCOME_17"),c("lga","year","lga.area.2014","pop.density","pop.work.age.percent","pop.school.age.percent","transport.businesses", "total.businesses", "median.income"))

#change variable classes 
LGA.wide$year <- as.integer(LGA.wide$year)
LGA.wide[,c("lga.area.2014","pop.density","pop.work.age.percent","pop.school.age.percent","MVC_14","MVC_15","MVC_16","MVC_17","MVC_18","MVC_19","MVC_20","MVC_21","MVC_22","transport.businesses", "total.businesses", "median.income")] <- as.numeric(unlist(LGA.wide[,c("lga.area.2014","pop.density","pop.work.age.percent","pop.school.age.percent","MVC_14","MVC_15","MVC_16","MVC_17","MVC_18","MVC_19","MVC_20","MVC_21","MVC_22","transport.businesses", "total.businesses", "median.income")]))

#create light vehicle and heavy vehicle stats
LGA.wide$vehicles.light <- sum(LGA.wide$MVC_14, LGA.wide$MVC_15, LGA.wide$MVC_16, LGA.wide$MVC_17, LGA.wide$MVC_22, na.rm=TRUE)
LGA.wide$vehicles.heavy <- sum(LGA.wide$MVC_18, LGA.wide$MVC_19, LGA.wide$MVC_20, LGA.wide$MVC_21, na.rm=TRUE)

#create densities for vehicles, businesses
LGA.wide$density.vehicles.light <- LGA.wide$vehicles.light / LGA.wide$lga.area.2014
LGA.wide$density.vehicles.heavy <- LGA.wide$vehicles.heavy / LGA.wide$lga.area.2014
LGA.wide$density.transport.businesses <- LGA.wide$transport.businesses / LGA.wide$lga.area.2014
LGA.wide$density.total.businesses <- LGA.wide$total.businesses / LGA.wide$lga.area.2014

#subset the final columns we need
LGA.wide <- LGA.wide[,c("lga","year","lga.area.2014","pop.density","pop.work.age.percent","pop.school.age.percent","density.vehicles.light","density.vehicles.heavy","density.transport.businesses","density.total.businesses","median.income")]

saveRDS(LGA.wide,file="LGA_wide.rds")
LGA.wide <- read_rds("/Users/RohanDanisCox/STDS/LGA_wide.rds") # need to change to your location

#left join to add ABS data for LGA/Year to each row of traffic data
traffic.with.abs <- merge(x = traffic_vol, y = LGA.wide, by = c("lga", "year"), all.x = TRUE)
traffic_with_abs <- traffic.with.abs

# Store the traffic.with.abs file as an RDS
saveRDS(traffic.with.abs,file= "traffic_with_abs.rds")

# Retrieve Data
traffic_with_abs <- read_rds("/Users/RohanDanisCox/STDS/traffic_with_abs.rds") # need to change to your location

# select the variables of interest in the dataframe (exclude unnecessary variables)
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
         daily_total,
         hour_00,
         hour_01,
         hour_02,
         hour_03,
         hour_04,
         hour_05,
         hour_06,
         hour_07,
         hour_08,
         hour_09,
         hour_10,
         hour_11,
         hour_12,
         hour_13,
         hour_14,
         hour_15,
         hour_16,
         hour_17,
         hour_18,
         hour_19,
         hour_20,
         hour_21,
         hour_22,
         hour_23)


# Clean the variables
str(data)
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
  summarise(hour_01= sum(hour_01),hour_02= sum(hour_02),hour_03= sum(hour_03),hour_04= sum(hour_04),
            hour_05= sum(hour_05),hour_06= sum(hour_06),hour_07= sum(hour_07),hour_08= sum(hour_08),
            hour_09= sum(hour_09),hour_10= sum(hour_10),hour_11= sum(hour_11),hour_12= sum(hour_12),
            hour_13= sum(hour_13),hour_14= sum(hour_14),hour_15= sum(hour_15),hour_16= sum(hour_16),
            hour_17= sum(hour_17),hour_18= sum(hour_18),hour_19= sum(hour_19),hour_20= sum(hour_20),
            hour_21= sum(hour_21),hour_22= sum(hour_22),hour_23= sum(hour_23),
            daily_total=sum(daily_total))

# Store the traffic.with.abs file as an RDS
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


# subset BOM file for relevant columns
RainByDay <- data.frame(subset(BOMNSW, select = c(1,2,3,4,5,8,17,19,20,18,21)))
colnames(RainByDay) <- c("StationNum","name", "Locality","lat", "lon", "rain","Year1","Month1", "Day1", "Date1","Key1")

RainCount <- RainByDay %>%
  group_by(Key1) %>%
  mutate(DailyRain=sum(rain, na.rm = TRUE)) %>%
  distinct(StationNum,name,Locality,lat,lon,Year1, Month1,Day1, Date1, DailyRain)

FullData <- data
str(FullData)

FullData$date <- as.Date(with(FullData, paste(year, month, day,sep="-")), "%Y-%m-%d")
FullData$Key1<-paste(FullData$nearest_weather_station,"_",FullData$date)
FullDataRain<-merge(FullData,RainCount, by="Key1")

saveRDS(FullDataRain,file= "FullDataRain.rds")
names(FullDataRain)

# fixing columns
FinalData <- FullDataRain %>%
  select(-c(Key1,hour_01:hour_23,StationNum:Date1)) %>%
  select(c(station_key:wgs84_longitude,Distance_CBD:lon.x,pop.density:density.vehicles.heavy,
           year:day,date,day_of_week:school_holiday,DailyRain,daily_total))

saveRDS(FinalData,file= "FinalData.rds")
FinalData <- read_rds("/Users/RohanDanisCox/STDS/FinalData.rds") # fix to your computer location

################################################
############## REMAINING STEPS #################
# 1) Model data (daily_total ~ other variables)
# 2) Interpret modelling
################################################

# to fix ABS figures for 2016 which were missing from the data
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

saveRDS(FinalData,file= "FinalData.rds")
FinalData <- read_rds("/Users/RohanDanisCox/STDS/FinalData.rds") # fix to your computer location

# split the data into train and test
test <- FinalData %>%
  filter(year==2016)
train <- FinalData %>%
  filter(year<2016)

# fit a poisson log linear model i.e link=log
poisson_model <- glm(daily_total~road_functional_hierarchy + mab_way_type + lane_count + cardinal_direction_seq +
                   rms_region + lga + device_type + permanent_station + Distance_CBD + pop.density + pop.work.age.percent +
                   pop.school.age.percent + density.vehicles.light + density.vehicles.heavy + year + month + day + day_of_week + 
                   public_holiday + school_holiday + DailyRain,
                 data=train,family=poisson(link=log)) # creates a huge model - 1.5gig

# check for overdispersion
mean(train$daily_total) # [1] 17018.47
var(train$daily_total) # [1] 178949195
dispersiontest(poisson_model) # [1] dispersion of 2301.105 which is way bigger than 1. Need to control
summary(poisson_model) #clearly broken - every single variable has a p value of virtually 0.
summary(poisson_model, dispersion=2301.105,correlation=TRUE,symbolic.cor = TRUE) # this seems to work much better but still has plenty of meaningless crap

# will need to use this to get coefficients back to scale of data
exp(coef(poisson_model))

# another way to control for overdispersion is to use a quasipoisson model - throws up a wierd error but still works
quasipoisson_model <- glm(daily_total~road_functional_hierarchy + mab_way_type + lane_count + cardinal_direction_seq +
                   rms_region + lga + device_type + permanent_station + Distance_CBD + pop.density + pop.work.age.percent +
                   pop.school.age.percent + density.vehicles.light + density.vehicles.heavy + year + month + day + day_of_week + 
                   public_holiday + school_holiday + DailyRain,
                 data=train,family=quasipoisson(link=log))
summary.glm(quasipoisson_model)$dispersion
summary(quasipoisson_model) # all variables again have nearly 0 p value

# No idea where to go from here would be better off just guessing what should go in the model e.g.
poisson_model2 <- glm(daily_total~road_functional_hierarchy + Distance_CBD + pop.density + pop.work.age.percent +
                       density.vehicles.light + year + month + day_of_week + public_holiday + school_holiday + DailyRain,
                     data=train,family=poisson(link=log)) 
dispersiontest(poisson_model2) # [1] dispersion of 7981.038 
summary(poisson_model2) 
summary(poisson_model2, dispersion=7981.038,correlation=TRUE,symbolic.cor = TRUE) 

# try slimmed down quasipoisson_model
quasipoisson_model2 <- glm(daily_total~road_functional_hierarchy + Distance_CBD + pop.density + pop.work.age.percent +
                        density.vehicles.light + year + month + day_of_week + public_holiday + school_holiday + DailyRain,
                      data=train,family=poisson(link=log)) 
summary.glm(quasipoisson_model2)$dispersion
summary(quasipoisson_model2) # all variables again have nearly 0 p value

# Don't really know what to make of any of this... 
exp(predict(quasipoisson_model2,data.frame(road_functional_hierarchy="Motorway",Distance_CBD=20,pop.density=2000,
                                       pop.work.age.percent=60,density.vehicles.light=200,year=2015,month=12,
                                       day_of_week="SATURDAY",public_holiday=as.factor(FALSE),school_holiday=as.factor(FALSE),
                                       DailyRain=0)))
# [1] 45384.74
# works but fuck knows if it is any good...if I change to "MONDAY" instead I get 47541 which seems sensible
exp(predict(quasipoisson_model2,data.frame(road_functional_hierarchy="Motorway",Distance_CBD=20,pop.density=2000,
                                           pop.work.age.percent=60,density.vehicles.light=200,year=2015,month=12,
                                           day_of_week="MONDAY",public_holiday=as.factor(FALSE),school_holiday=as.factor(FALSE),
                                           DailyRain=0)))
# [1] 47541.97
# If I add heaps of rain it reduces so maybe it is actually working okay
exp(predict(quasipoisson_model2,data.frame(road_functional_hierarchy="Motorway",Distance_CBD=20,pop.density=2000,
                                           pop.work.age.percent=60,density.vehicles.light=200,year=2015,month=12,
                                           day_of_week="SATURDAY",public_holiday=as.factor(FALSE),school_holiday=as.factor(FALSE),
                                           DailyRain=50)))

# Prediction won't work until we sort 2016 ABS data. In the meantime could try this:
quasipoisson_model3 <- glm(daily_total~road_functional_hierarchy + Distance_CBD + year + month + day_of_week + public_holiday +
                             school_holiday + DailyRain,
                           data=train,family=poisson(link=log)) 

prediction <- exp(predict(quasipoisson_model3,newdata = test))
test <- cbind(test,prediction)
SummaryTest <- test %>%
  mutate(error=sqrt((daily_total-prediction)^2)) %>%
  summarise(average=mean(error,na.rm=TRUE))
# 6730

prediction <- exp(predict(quasipoisson_model2,newdata = test))
test <- cbind(test,prediction)
SummaryTest <- test %>%
  mutate(error=sqrt((daily_total-prediction)^2)) %>%
  summarise(average=mean(error,na.rm=TRUE))


plot(quasipoisson_model3)
# 6762
# seems to underestimate - possibly because of change in 2015


###
### Cutting down to April 2015
###

DataApril2015 <- FinalData %>%
  filter(date>"2015-03-31")

saveRDS(DataApril2015,file= "DataApril2015.rds")
DataApril2015 <- read_rds("/Users/RohanDanisCox/STDS/DataApril2015.rds") # fix to your computer location

# split into test & training
set.seed(42)
train = createDataPartition(y = DataApril2015$daily_total, p = 0.80, list = F)
Working = DataApril2015[train,]
Holdout = DataApril2015[-train,]

train = createDataPartition(y = Working$daily_total, p = 0.70, list = F)
trainset = Working[train,]
testset = Working[-train,]

## FORWARD SELECTION

variables <- c("DailyRain","pop.density","day_of_week","road_functional_hierarchy","density.vehicles.light","pop.work.age.percent",
              "year","month","day","Distance_CBD","public_holiday","school_holiday")
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
# Distance_CBD
variables2 <- variables <- c("DailyRain","pop.density","day_of_week","road_functional_hierarchy","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","public_holiday","school_holiday")
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
# road_functional_heirarchy
variables3 <- variables <- c("DailyRain","pop.density","day_of_week","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","public_holiday","school_holiday")
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
# day_of_week
variables4 <- variables <- c("DailyRain","pop.density","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","public_holiday","school_holiday")
Variable4 <-data.frame()
for(i in variables4) {
  model <- glm(paste("daily_total~ Distance_CBD + road_functional_hierarchy + day_of_week +", i[[1]]),
               data=trainset,family=quasipoisson(link=log))
  prediction <- exp(predict(model,newdata = testset))
  check <- cbind(testset,prediction)
  SummaryTest <- check %>%
    mutate(error=sqrt((daily_total-prediction)^2)) %>%
    summarise(average=mean(error,na.rm=TRUE))
  bind <- cbind(i,SummaryTest)
  Variable4 <- rbind(Variable4,bind)
}
# public_holiday
variables5 <- variables <- c("DailyRain","pop.density","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","school_holiday")
Variable5 <-data.frame()
for(i in variables5) {
  model <- glm(paste("daily_total~ Distance_CBD + road_functional_hierarchy + day_of_week + public_holiday + ", i[[1]]),
               data=trainset,family=quasipoisson(link=log))
  prediction <- exp(predict(model,newdata = testset))
  check <- cbind(testset,prediction)
  SummaryTest <- check %>%
    mutate(error=sqrt((daily_total-prediction)^2)) %>%
    summarise(average=mean(error,na.rm=TRUE))
  bind <- cbind(i,SummaryTest)
  Variable5 <- rbind(Variable5,bind)
}
# DailyRain
variables6 <- variables <- c("pop.density","density.vehicles.light","pop.work.age.percent",
                             "year","month","day","school_holiday")
Variable6 <-data.frame()
for(i in variables6) {
  model <- glm(paste("daily_total~ Distance_CBD + road_functional_hierarchy + day_of_week + public_holiday + DailyRain + ", i[[1]]),
               data=trainset,family=quasipoisson(link=log))
  prediction <- exp(predict(model,newdata = testset))
  check <- cbind(testset,prediction)
  SummaryTest <- check %>%
    mutate(error=sqrt((daily_total-prediction)^2)) %>%
    summarise(average=mean(error,na.rm=TRUE))
  bind <- cbind(i,SummaryTest)
  Variable6 <- rbind(Variable6,bind)
}
# School_Holiday
variables7 <- variables <- c("pop.density","density.vehicles.light","pop.work.age.percent",
                             "year","month","day")
Variable7 <-data.frame()
for(i in variables7) {
  model <- glm(paste("daily_total~ Distance_CBD + road_functional_hierarchy + day_of_week + public_holiday + DailyRain + school_holiday +", i[[1]]),
               data=trainset,family=quasipoisson(link=log))
  prediction <- exp(predict(model,newdata = testset))
  check <- cbind(testset,prediction)
  SummaryTest <- check %>%
    mutate(error=sqrt((daily_total-prediction)^2)) %>%
    summarise(average=mean(error,na.rm=TRUE))
  bind <- cbind(i,SummaryTest)
  Variable7 <- rbind(Variable7,bind)
}
## STOP

## Final Model 

Final_Model <- glm(daily_total~ Distance_CBD + road_functional_hierarchy + day_of_week + public_holiday + DailyRain + school_holiday,
                   data=trainset,family=quasipoisson(link=log)) 
Alt_model <- glm.nb(daily_total~ Distance_CBD + road_functional_hierarchy + day_of_week + public_holiday + DailyRain + school_holiday,
                   data=trainset) 
summary(Alt_model)
prediction <- exp(predict(Alt_model,newdata = testset))
prediction <- exp(predict.glm(Final_Model,newdata = testset))
testset <- cbind(testset,prediction)
ggplot(testset,aes(x=daily_total,y=prediction,colour=road_functional_hierarchy))+
  geom_point(size=0.5,alpha=0.2)

?glm.nb

names(Final_Model)
summary.glm(Final_Model)$dispersion
summary(Final_Model, dispersion=11946.67,correlation=TRUE,symbolic.cor = TRUE)

prediction <- exp(predict.glm(Final_Model,type="response",newdata = testset))
testset <- cbind(testset,prediction)
ggplot(testset,aes(x=daily_total,y=prediction,colour=road_functional_hierarchy))+
  geom_point(size=0.5,alpha=0.2) + 
  expand_limits(x=c(0,80000), y=c(0,80000))
ggplot(testset,aes(x=daily_total,y=prediction,colour=rms_region))+
  geom_point(size=0.5,alpha=0.2) + 
  expand_limits(x=c(0,80000), y=c(0,80000))
ggplot(testset) +
  geom_jitter(aes(x=day_of_week,y=prediction),colour="red",size=0.05,alpha=0.1) +
  geom_jitter(aes(x=day_of_week,y=daily_total),colour="blue",size=0.05,alpha=0.1) 

?glm
# Try just Sydney RMS

Sydney <- Working %>%
  filter(rms_region=="Sydney")

train = createDataPartition(y = Sydney$daily_total, p = 0.70, list = F)
Sydtrainset = Working[train,]
Sydtestset = Working[-train,]


Sydney_Model <- glm(daily_total~ Distance_CBD + road_functional_hierarchy + day_of_week + public_holiday + DailyRain + school_holiday,
                   data=Sydtrainset,family=poisson(link=log)) 

prediction <- exp(predict(Sydney_Model,newdata = Sydtestset))
Sydtestset <- cbind(Sydtestset,prediction)
ggplot(Sydtestset,aes(x=daily_total,y=prediction))+
  geom_point(size=0.5,alpha=0.2) +
  geom_smooth() + 
  expand_limits(x=c(0,80000), y=c(0,80000))
