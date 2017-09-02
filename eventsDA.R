library(httr)
library(jsonlite)
events <- GET("https://api.transport.nsw.gov.au/v1/live/hazards/majorevent/all", 
              verbose(), 
              encode="json", 
              add_headers(Authorization = "apikey 5I42m6prdSGmGohy1mMub1SHNGm6hxWjrg0a", lastPublished=""))

names(events)
events$status_code
content(events)
head(events$content)
raw.events <- rawToChar(events$content)
nchar(raw.events)
substr(raw.events, 1, 100)
events <- fromJSON(raw.events)
class(events)
length(events)

events[[5]]

events.df <- as.data.frame(events[[5]])
events.prop <- as.data.frame(events.df$properties)
events.geo <- as.data.frame(events.df$geometry)
events.df <- merge(events.geo,events.prop)
