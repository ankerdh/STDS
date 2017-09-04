library(httr)
library(jsonlite)
library(tidyverse)
#get location key for the required location from Accuweather location API
getlocation<- GET("http://dataservice.accuweather.com/locations/v1/postalcodes/search?q=2140&apikey=HNyjZnlRykmDQLvWoyCOc460vyjPg9dJ")
names(getlocation)
getlocation$status_code
content(getlocation)
head(getlocation$content)
location_raw <- rawToChar(getlocation$content)
location_clean <-fromJSON(location_raw,simplifyDataFrame = TRUE)
location_key <- location_clean[1,]$Key

#get the current weather data for the location from Accuweather current conditions API

getcurrent <- GET("http://dataservice.accuweather.com/currentconditions/v1/54968_PC?apikey=HNyjZnlRykmDQLvWoyCOc460vyjPg9dJ")
names(getcurrent)
getcurrent$status_code
content(getcurrent)
current_raw <- rawToChar(getcurrent$content)
current_clean <-fromJSON(current_raw,simplifyDataFrame = TRUE)
  