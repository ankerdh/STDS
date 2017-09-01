library(httr)
library(jsonlite)
result <- GET("https://api.transport.nsw.gov.au/v1/live/hazards/incident/all", 
              verbose(), 
              encode="json", 
              add_headers(Authorization = "apikey 5I42m6prdSGmGohy1mMub1SHNGm6hxWjrg0a", lastPublished=""))

names(result)
result$status_code
content(result)
head(result$content)
raw.data <- rawToChar(result$content)
nchar(raw.data)
substr(raw.data, 1, 100)
data <- fromJSON(raw.data)
class(data)
length(data)

data[[5]]

df <- as.data.frame(data[[5]])
df
df$properties$headline
