library(httr)
library(jsonlite)
library(tidyverse)

# GET the full stations database using the API

stations_api<- GET("https://api.transport.nsw.gov.au/v1/roads/spatial?format=geojson&q=select%20*%20from%20road_traffic_counts_station_reference%20",
                   verbose(), 
                   encode="json", 
                   add_headers(`Authorization` = "apikey fUa8N1LC42AYtVDKIt6jbAzQXFPcf9b31GYv"))

stations_raw <- rawToChar(stations_api$content)
stations_clean <- fromJSON(stations_raw)
stations_df <- as.data.frame(stations_clean[[2]])
stations <- as.data.frame(stations_df$properties)
M1 <- filter(stations,station_id=="SHT")
harbour_tunnel <- filter(stations,station_id=="01003")
harbour_tunnel$station_key

# GET the full count database using the API 

count_api<- GET("https://api.transport.nsw.gov.au/v1/roads/spatial?format=geojson&q=select%20*%20from%20road_traffic_counts_hourly_permanent%20",
                       verbose(), 
                       encode="json", 
                       add_headers(`Authorization` = "apikey fUa8N1LC42AYtVDKIt6jbAzQXFPcf9b31GYv"))

count_api$status_code
count_raw <- rawToChar(count_api$content)
count_clean <- fromJSON(count_raw)
count_df <- as.data.frame(count_clean[[2]])
count_wide <- as.data.frame(count_df$properties)
count <-gather(count_wide,key=hour,value=count,hour_00:hour_23)

# Merge the two tables using the key identifier of Station key

traffic_vol <- inner_join(stations,tunnel_count,by = "station_key") #lost about 450 observations - not the end of the world

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
         day_of_week,
         public_holiday,
         school_holiday,
         daily_total,
         hour,
         count)

## classification_seq = 0: UNCLASSIFIED 1: ALL VEHICLES 2: LIGHT VEHICLES 3: HEAVY VEHICLES -9: MISSING    
## traffic_direction_seq = 0: COUNTER 1: PRESCRIBED 2: BOTH -- not sure if this is needed
## cardinal_direction_seq = 1: NORTH 3: EAST 5: SOUTH 7: WEST 9: NORTHBOUND AND SOUTHBOUND 10: EASTBOUND AND WESTBOUND
