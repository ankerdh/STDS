library(httr)
library(jsonlite)
roadwork <- GET("https://api.transport.nsw.gov.au/v1/live/hazards/roadwork/all", 
              verbose(), 
              encode="json", 
              add_headers(Authorization = "apikey 5I42m6prdSGmGohy1mMub1SHNGm6hxWjrg0a", lastPublished=""))

names(roadwork)
roadwork$status_code
content(roadwork)
head(roadwork$content)
raw.roadwork <- rawToChar(roadwork$content)
nchar(raw.roadwork)
substr(raw.roadwork, 1, 100)
roadwork <- fromJSON(raw.roadwork)
class(roadwork)
length(roadwork)

roadwork[[5]]

roadwork.df <- as.data.frame(roadwork[[5]])
roadwork.prop <- as.data.frame(roadwork.df$properties)
roadwork.geo <- as.data.frame(roadwork.df$geometry)
roadwork.df.other <- merge(roadwork.geo,roadwork.prop)
