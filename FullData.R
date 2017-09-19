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

# compare each station location to BOM weather stations and choose closest (SOME ARE WRONG!)
BOMrain <- read_rds("/Users/RohanDanisCox/STDS/BOM_NSW_rain.RDS") # need to change to your location
BOMrainStation <- data.frame(subset(BOMrain, select = c(1,2, 5,6))) ### WHAT DOES THIS DO?
colnames(BOMrainStation) <- c("StationNum","name", "lat", "lon")
UniqueBOMStations <- sqldf('SELECT DISTINCT * FROM BOMrainStation') 

# create distance matrix
DistMatrix <- distm(stations[,c('wgs84_longitude','wgs84_latitude')], UniqueBOMStations[,c('lon','lat')], fun=distVincentyEllipsoid)/1000
NearestStation <- data.frame(UniqueBOMStations$StationNum[max.col(-DistMatrix)])

# Only take the unique BOM Stations required
UniqueNearestStation<-unique(NearestStation)

# Combine with traffic stations information
stations$nearest_weather_station <- NearestStation$UniqueBOMStations.StationNum.max.col..DistMatrix..
stations <- inner_join(stations, UniqueBOMStations,c("nearest_weather_station" = "StationNum"))

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
  filter(row_number() %in% 1801:1818)


# Create function to calculate distance by road to Sydney Tower
get.distance <- function(x,y,z) {
  results = gmapsdistance(
    origin = paste(x,y,sep=","), 
    destination = "-33.8704512,151.2058792", 
    mode = "driving", 
    traffic_model = "best_guess",
    shape = "long",
    key = "AIzaSyBlcTlunVo6Zmc2P2i98dwwlNPEKjKFouk")
  results.df<-data.frame(results)
  results.df$station_key <- z
  results.df
}

#calculate distance for each station
tic() 
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
check <- rbind(distance.df200,distance.df400,distance.df600,distance.df800,distance.df1000,distance.df1200,distance.df1400,distance.df1600,distance.df1800,distance.df1818)

distances.table <- check[,c("station_key", "Distance.Distance")]
stations <- inner_join(stations,distances.table,by="station_key")
stations <- rename(stations,"Distance_CBD" ="Distance.Distance")
saveRDS(stations,file= "SecretFile.rds")

# Open this instead 
stations <- read_rds("/Users/RohanDanisCox/STDS/SecretFile.rds") # need to change to your location

getloc_key<- function(year,month){
  url<- paste("http://dataservice.accuweather.com/locations/v1/cities/geoposition/search?apikey=HNyjZnlRykmDQLvWoyCOc460vyjPg9dJ&q=",lat,"%2C",long,"&details=true",sep="")
  bomgetkey<-GET(url)
  bomgetkey$status_code
  location_raw<-rawToChar(bomgetkey$content)
  location_cln <-fromJSON(location_raw,simplifyDataFrame = TRUE)
  location_key <-location_clean$Key
}
#call the function to get location key for the geocoordinates
getloc_key(-33,151)

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

# combine the permanent stations with the sample stations
count_wide <- rbind(perm_count_wide,samp_count_wide)

# add the a monthly day count
count_wide <- count_wide%>%
  mutate(day=mday(date))

# gather the hourly counts into a single column by hour
count <-gather(count_wide,key=hour,value=count,hour_00:hour_23)
count$hour <- gsub("hour_","", count$hour)
count$hour <- as.numeric(count$hour)

# Merge the two tables using the key identifier of Station key - inner join returns where a match is found
traffic_vol <- inner_join(stations,count,by = "station_key") #lost 400 or so observations

# select the variables of interest in the dataframe (exclude unnecessary variables)
names(traffic_vol)
traffic_vol_small <- traffic_vol %>%
  select(station_key,
         full_name,
         road_functional_hierarchy,
         road_classification_type,
         lane_count,
         classification_seq,
         rms_region,
         lga,
         suburb,
         post_code,
         device_type,
         wgs84_latitude,
         wgs84_longitude,
         traffic_direction_seq,
         cardinal_direction_seq,
         year,
         month,
         day,
         day_of_week,
         public_holiday,
         school_holiday,
         daily_total,
         hour,
         count)

## classification_seq = 0: UNCLASSIFIED 1: ALL VEHICLES 2: LIGHT VEHICLES 3: HEAVY VEHICLES -9: MISSING    
## traffic_direction_seq = 0: COUNTER 1: PRESCRIBED 2: BOTH -- not sure if this is needed
## cardinal_direction_seq = 1: NORTH 3: EAST 5: SOUTH 7: WEST 9: NORTHBOUND AND SOUTHBOUND 10: EASTBOUND AND WESTBOUND

## update factors based on above characteristics
traffic_vol_small$classification_seq <- factor(traffic_vol_small$classification_seq,levels=c(0,1,2,3,-9),labels = c("UNCLASSIFIED","ALL VEHICLES","LIGHT VEHICLES","HEAVY VEHICLES","MISSING"))
traffic_vol_small$traffic_direction_seq <- factor(traffic_vol_small$traffic_direction_seq,levels=c(0,1,2),labels = c("COUNTER","PRESCRIBED","BOTH"))
traffic_vol_small$cardinal_direction_seq <- factor(traffic_vol_small$cardinal_direction_seq,levels=c(1,3,5,7,9,10),labels = c("NORTH","EAST","SOUTH","WEST","NORTHBOUND AND SOUTHBOUND","EASTBOUND & WESTBOUND"))
traffic_vol_small$day_of_week <- factor(traffic_vol_small$day_of_week,levels=c(1,2,3,4,5,6,7),labels = c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY"))

# Store the completed file as an RDS
saveRDS(traffic_vol_small,file= "traffic_vol_small.rds")

# Retrieve Data
traffic_vol_small <- read_rds("/Users/RohanDanisCox/STDS/traffic_vol_small.rds") # need to change to your location

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

#Check INCOME_17 aka median.income as it may not be available all years?

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

#left join to add ABS data for LGA/Year to each row of traffic data
traffic.with.abs <- merge(x = traffic_vol_small, y = LGA.wide, by = c("lga", "year"), all.x = TRUE)

# Test modelling
Model <- lm(count~lane_count + pop.density + day_of_week + road_functional_hierarchy ,data = traffic.with.abs)
summary(Model)

################################################
############## REMAINING STEPS #################
# 1) Add weather data
# 2) realign code so that distance is handled earlier and sample stations are considered
# 3) Model data (daily_total ~ other variables)
# 4) Interpret modelling
################################################

