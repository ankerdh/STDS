library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)

events.df$geometry$coordinates <- gsub("c(","",events.df$geometry$coordinates, fixed=TRUE)
events.df$geometry$coordinates <- gsub(")","",events.df$geometry$coordinates, fixed=TRUE)
incidents.df$geometry$coordinates <- gsub("c(","",incidents.df$geometry$coordinates, fixed=TRUE)
incidents.df$geometry$coordinates <- gsub(")","",incidents.df$geometry$coordinates, fixed=TRUE)

events.df$geometry <- events.df$geometry %>% separate(coordinates, c("lon","lat"), sep=",")
incidents.df$geometry <- incidents.df$geometry %>% separate(coordinates, c("lon","lat"), sep=",")

events.df$geometry$lon <- as.numeric(events.df$geometry$lon)
events.df$geometry$lat <- as.numeric(events.df$geometry$lat)
incidents.df$geometry$lon <-  as.numeric(incidents.df$geometry$lon)
incidents.df$geometry$lat <-  as.numeric(incidents.df$geometry$lat)

syd <- ggmap(get_googlemap(c(lon=151, lat=-33.9), zoom=9, maptype="roadmap", size=c(600,400)))

syd +
  geom_point(aes(x=geometry$lon, y=geometry$lat), data=events.df, colour="blue") +
  geom_point(aes(x=geometry$lon, y=geometry$lat), data=incidents.df, colour="red")
