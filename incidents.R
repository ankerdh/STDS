library(httr)
library(jsonlite)
incidents <- GET("https://api.transport.nsw.gov.au/v1/live/hazards/incident/all", 
              verbose(), 
              encode="json", 
              add_headers(Authorization = "apikey 5I42m6prdSGmGohy1mMub1SHNGm6hxWjrg0a", lastPublished=""))

names(incidents)
incidents$status_code
content(incidents)
head(incidents$content)
raw.incidents <- rawToChar(incidents$content)
nchar(raw.incidents)
substr(raw.incidents, 1, 100)
incidents <- fromJSON(raw.incidents)
class(incidents)
length(incidents)

incidents[[5]]

incidents.df <- as.data.frame(incidents[[5]])
incidents.prop <- as.data.frame(incidents.df$properties)
incidents.geo <- as.data.frame(incidents.df$geometry)
incidents.df <- merge(incidents.geo,incidents.prop)

