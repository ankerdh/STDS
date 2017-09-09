library(httr)
library(jsonlite)
library(tidyverse)

#get location key for the required location from Accuweather location API
getloc_key<- function(lat,long){
  url<- paste("http://dataservice.accuweather.com/locations/v1/cities/geoposition/search?apikey=HNyjZnlRykmDQLvWoyCOc460vyjPg9dJ&q=",lat,"%2C",long,"&details=true",sep="")
  bomgetkey<-GET(url)
  bomgetkey$status_code
  location_raw<-rawToChar(bomgetkey$content)
  location_cln <-fromJSON(location_raw,simplifyDataFrame = TRUE)
  location_key <-location_clean$Key
}
#call the function to get location key for the geocoordinates
getloc_key(-33,151)

#get the current weather data for the location from Accuweather current conditions API
url_current <- paste("http://dataservice.accuweather.com/currentconditions/v1/",location_key,"?apikey=HNyjZnlRykmDQLvWoyCOc460vyjPg9dJ&details=true",sep="")
getcurrent <- GET(url_current)
names(getcurrent)
getcurrent$status_code
content(getcurrent)
current_raw <- rawToChar(getcurrent$content)
current_clean <-fromJSON(current_raw,simplifyDataFrame = TRUE)

#get forecast weather data for next 1 hour
url_fcast<- paste("http://dataservice.accuweather.com/forecasts/v1/hourly/12hour/",location_key,"?apikey=HNyjZnlRykmDQLvWoyCOc460vyjPg9dJ&details=true",sep="")
getforecast<- GET(url_fcast)
getforecast$status_code
content(getforecast)
forecast_raw <-rawToChar(getforecast$content)
class(forecast_raw)
forecast_clean <-fromJSON(forecast_raw,simplifyDataFrame = TRUE)
