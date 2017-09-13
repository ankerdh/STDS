library(rwunderground)
library(dplyr)
#set the api key for weather underground AP
set_api_key("61d151be5bc81c52")
#loc<-geolookup(set_location(lat_long = "-33,151"))

#pass the location coordinates and set the location
latlong <-"-33,151"
location <- set_location(lat_long = latlong)

#Get hourly forecast for next 24hrs
forecast_24<-hourly(location)

#Get hourly forecast for next 10 days
forecast_10day <-hourly10day(location)

#Get historical weather for a particular date
histdate <-history(location,date="20150101")

#get historical weather data for a range of dates 
#remember that API allows calls only 10times per minute. the package however has
#inbuilt delay set to wait before next minute as.Enter dates in YYYYMMDD format
history<- history_range(set_location(lat_long = "-33,151"),date_start="20150101",
                        date_end="20150110",message=TRUE)
