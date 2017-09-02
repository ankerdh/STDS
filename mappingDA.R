library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)

events.df$coordinates <- gsub("c(","",events.df$coordinates, fixed=TRUE)
events.df$coordinates <- gsub(")","",events.df$coordinates, fixed=TRUE)
incidents.df$coordinates <- gsub("c(","",incidents.df$coordinates, fixed=TRUE)
incidents.df$coordinates <- gsub(")","",incidents.df$coordinates, fixed=TRUE)
roadwork.df$coordinates <- gsub("c(","",roadwork.df$coordinates, fixed=TRUE)
roadwork.df$coordinates <- gsub(")","",roadwork.df$coordinates, fixed=TRUE)

events.df <- events.df %>% separate(coordinates, c("lon","lat"), sep=",")
incidents.df <- incidents.df %>% separate(coordinates, c("lon","lat"), sep=",")
roadwork.df <- roadwork.df %>% separate(coordinates, c("lon","lat"), sep=",")


events.df$lon <- as.numeric(events.df$lon)
events.df$lat <- as.numeric(events.df$lat)
incidents.df$lon <-  as.numeric(incidents.df$lon)
incidents.df$lat <-  as.numeric(incidents.df$lat)
roadwork.df$lon <-  as.numeric(roadwork.df$lon)
roadwork.df$lat <-  as.numeric(roadwork.df$lat)

syd <- ggmap(get_googlemap(c(lon=151, lat=-33.9), zoom=10, maptype="terrain", size=c(600,400)))

syd +
  geom_point(aes(x=lon, y=lat), data=events.df, colour="blue", size=3) +
  geom_point(aes(x=lon, y=lat), data=incidents.df, colour="red", size=3) +
  geom_point(aes(x=lon, y=lat), data=roadwork.df, colour="darkorange", size=3) +
  ggtitle("Traffic incidents, roadwork and major events")
