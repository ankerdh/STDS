library(httr)
library(jsonlite)
result <- GET("https://api.transport.nsw.gov.au/v1/live/hazards/majorevent/all", 
              verbose(), 
              encode="json", 
              add_headers(`Authorization` = "apikey 5I42m6prdSGmGohy1mMub1SHNGm6hxWjrg0a"))

names(result)
result$status_code
content(result)
head(result$content)
this.raw.content <- rawToChar(result$content)
nchar(this.raw.content)
substr(this.raw.content, 1, 100)
this.content <- fromJSON(this.raw.content)
class(this.content)
length(this.content)

this.content[[1]]
this.content[[2]]
this.content[[3]]
this.content[[4]]
this.content[[5]]

this.content.df <- do.call(what = "rbind",
                           args = lapply(this.content, as.data.frame))

a <- as.data.frame(this.content[[5]])
a
a$properties$headline
