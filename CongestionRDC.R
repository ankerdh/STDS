library(httr)
library(jsonlite)
library(tidyverse)

# This GET retrieves 1000 records from 2017 of various day counts of certain counter stations
# From here I need to identify specific counter stations to focus on and specific dates to worry about

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
Wide.Congestion <- as.data.frame(clean.content[[2]])

# Need to tidy up the variables using tidyverse (job for another day)