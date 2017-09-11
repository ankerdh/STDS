library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(rsdmx)
library(data.table)
library(geosphere)
library(reshape2)
library(gmapsdistance)

# GET the full stations database using the API

stations_api<- GET("https://api.transport.nsw.gov.au/v1/roads/spatial?format=geojson&q=select%20*%20from%20road_traffic_counts_station_reference%20",
                   verbose(), 
                   encode="json", 
                   add_headers(`Authorization` = "apikey fUa8N1LC42AYtVDKIt6jbAzQXFPcf9b31GYv"))

stations_raw <- rawToChar(stations_api$content)
stations_clean <- fromJSON(stations_raw)
stations_df <- as.data.frame(stations_clean[[2]])
stations <- as.data.frame(stations_df$properties)

# GET the count database using the API for 2011 onwards

count_api<- GET("https://api.transport.nsw.gov.au/v1/roads/spatial?format=geojson&q=select%20*%20from%20road_traffic_counts_hourly_permanent%20where%20year%20%3E%3D2011",
                verbose(), 
                encode="json", 
                add_headers(`Authorization` = "apikey fUa8N1LC42AYtVDKIt6jbAzQXFPcf9b31GYv"))

count_api$status_code
count_raw <- rawToChar(count_api$content)
count_clean <- fromJSON(count_raw)
count_df <- as.data.frame(count_clean[[2]])
count_wide <- as.data.frame(count_df$properties)
count_wide <- count_wide%>%
  mutate(day=mday(date))

# Merge the two tables using the key identifier of Station key

traffic_vol <- inner_join(stations,count_wide,by = "station_key") #lost 20 observations

# trim df to the variables of interest
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
         daily_total)

## classification_seq = 0: UNCLASSIFIED 1: ALL VEHICLES 2: LIGHT VEHICLES 3: HEAVY VEHICLES -9: MISSING    
## traffic_direction_seq = 0: COUNTER 1: PRESCRIBED 2: BOTH -- not sure if this is needed
## cardinal_direction_seq = 1: NORTH 3: EAST 5: SOUTH 7: WEST 9: NORTHBOUND AND SOUTHBOUND 10: EASTBOUND AND WESTBOUND

## update factors
traffic_vol_small$classification_seq <- factor(traffic_vol_small$classification_seq,levels=c(0,1,2,3,-9),labels = c("UNCLASSIFIED","ALL VEHICLES","LIGHT VEHICLES","HEAVY VEHICLES","MISSING"))
traffic_vol_small$traffic_direction_seq <- factor(traffic_vol_small$traffic_direction_seq,levels=c(0,1,2),labels = c("COUNTER","PRESCRIBED","BOTH"))
traffic_vol_small$cardinal_direction_seq <- factor(traffic_vol_small$cardinal_direction_seq,levels=c(1,3,5,7,9,10),labels = c("NORTH","EAST","SOUTH","WEST","NORTHBOUND AND SOUTHBOUND","EASTBOUND & WESTBOUND"))
traffic_vol_small$day_of_week <- factor(traffic_vol_small$day_of_week,levels=c(1,2,3,4,5,6,7),labels = c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY"))

#read in selected variables from the ABS table ABS_REGIONAL_LGA
LGAurl <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ABS_REGIONAL_LGA/ERP_18+ERP_21+200IND+MVC_14+MVC_15+MVC_16+MVC_17+MVC_18+MVC_19+MVC_20+MVC_21+MVC_22+MVC_38+LAND.LGA2014..A/all?startTime=2011&endTime=2016&format=compact_v2"
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
setnames(LGA.wide,c("label.en","TIME", "OBS_VALUE.y","ERP_21","ERP_18", "MVC_14", "MVC_38"),c("lga","year","lga.area.2014","pop.density",'pop.work.age.percent',"vehicles.rego.cars","vehicles.rego.total"))

#change variable classes and create densities for vehicles
LGA.wide$year <- as.integer(LGA.wide$year)
LGA.wide[,c("lga.area.2014","pop.density",'pop.work.age.percent',"vehicles.rego.cars","vehicles.rego.total")] <- as.numeric(unlist(LGA.wide[,c("lga.area.2014","pop.density",'pop.work.age.percent',"vehicles.rego.cars","vehicles.rego.total")]))
LGA.wide$density.vehicles.cars <- LGA.wide$vehicles.rego.cars / LGA.wide$lga.area.2014
LGA.wide$density.vehicles.total <- LGA.wide$vehicles.rego.total / LGA.wide$lga.area.2014

#subset the final columns we need
LGA.wide <- LGA.wide[,c("lga","year","lga.area.2014","pop.density",'pop.work.age.percent',"density.vehicles.cars","density.vehicles.total")]

#left join to add ABS data for LGA/Year to each row of traffic data

###   NOTE  NOTE   NOTE  #######just a TEST for now, not the full traffic dataset
traffic_vol_test<- traffic_vol_small[seq(1, 90000, by = 1000),]
traffic.with.abs <- merge(x = traffic_vol_test, y = LGA.wide, by = c("lga", "year"), all.x = TRUE)

#now calculate distance from Sydney CBD for each station

#make list of stations
###   NOTE  NOTE   NOTE  #######just a TEST for now, not the full traffic dataset
station.locations <- unique(traffic_vol_test[,c("station_key","wgs84_latitude","wgs84_longitude")])

#create function to calculate distance by road to Sydney Tower
get.distance <- function(x,y,z) {
  results = gmapsdistance(
    origin = paste(x,y,sep=","), 
    destination = "-33.8704512,151.2058792", 
    mode = "driving", 
    traffic_model = "best_guess",
    shape = "long",
    key = "AIzaSyDDihK34ya701nYseOdUXLDwH7XWfYMuC0")
  results.df<-data.frame(results)
  results.df$station_key <- z
  results.df
}

#calculate distance for each station
distance.df <- get.distance(station.locations$wgs84_latitude,station.locations$wgs84_longitude, station.locations$station_key)
distances.table <- distance.df[,c("station_key", "Distance.Distance")]

#add to the traffic table
traffic.with.abs <- merge(x = traffic.with.abs, y = distances.table, by = "station_key", all.x = TRUE)

################################################
############## REMAINING STEPS #################
# 1) Fill data for 2017 (need some assumptions)
# 2) Model data (daily_total ~ other variables)
# 3) Interpret modelling
################################################
