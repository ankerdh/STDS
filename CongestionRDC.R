library(httr)
library(jsonlite)
library(tidyverse)

# First we need to identify the stations (roads) we are interesting in considering
# SQL query = select * from road_traffic_counts_station_reference 

stations_api<- GET("https://api.transport.nsw.gov.au/v1/roads/spatial?format=geojson&q=select%20*%20from%20road_traffic_counts_station_reference%20",
             verbose(), 
             encode="json", 
             add_headers(`Authorization` = "apikey fUa8N1LC42AYtVDKIt6jbAzQXFPcf9b31GYv"))

stations_raw <- rawToChar(stations_api$content)
stations_clean <- fromJSON(stations_raw)
class(stations_clean) #it's a list
length(stations_clean) 
stations_df <- as.data.frame(stations_clean[[2]])
stations <- as.data.frame(stations_df$properties)
M1 <- filter(stations,station_id=="SHT")
harbour_tunnel <- filter(stations,station_id=="01003")
harbour_tunnel$station_key

# This GET retrieves hourly counts for the above station key
# SQL query = select * from road_traffic_counts_hourly_permanent where station_key=55304

tunnel_count_api<- GET("https://api.transport.nsw.gov.au/v1/roads/spatial?format=geojson&q=select%20*%20from%20road_traffic_counts_hourly_permanent%20where%20station_key%3D55304",
             verbose(), 
             encode="json", 
             add_headers(`Authorization` = "apikey fUa8N1LC42AYtVDKIt6jbAzQXFPcf9b31GYv"))

tunnel_count_api$status_code
tunnel_count_raw <- rawToChar(tunnel_count_api$content)
tunnel_count_clean <- fromJSON(tunnel_count_raw)
tunnel_count_df <- as.data.frame(tunnel_count_clean[[2]])
tunnel_count_wide <- as.data.frame(tunnel_count_df$properties)
tunnel_count <-gather(tunnel_count_wide,key=hour,value=count,hour_00:hour_23)

holidays <- tunnel_count %>%
  filter(public_holiday==TRUE)


# This GET retrieves 1000 records from 2017 of various day counts of certain counter stations
# From here I need to identify specific counter stations to focus on and specific dates to worry about
# SQL query = select * from road_traffic_counts_hourly_permanent where year=2017 limit 1000

result<- GET("https://api.transport.nsw.gov.au/v1/roads/spatial?format=geojson&q=select%20*%20from%20road_traffic_counts_hourly_permanent%20where%20year%3D2017%20limit%201000%20",
              verbose(), 
              encode="json", 
              add_headers(`Authorization` = "apikey fUa8N1LC42AYtVDKIt6jbAzQXFPcf9b31GYv"))

content(result)
names(result)
result$status_code
head(result$content)
raw.content <- rawToChar(result$content)
nchar(raw.content)
substr(raw.content, 1, 100)
clean.content <- fromJSON(raw.content)
class(clean.content) #it's a list
length(clean.content) 
Congestion <- as.data.frame(clean.content[[2]])
Wide.Congestion <- as.data.frame(Congestion$properties)
Narrow <-gather(Wide.Congestion,key=hour,value=count,hour_00:hour_23)

