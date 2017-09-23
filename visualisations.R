###GOOGLE MAPS###

library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)

data1 <- data %>%
  group_by(station_key) %>%
  mutate(average=mean(daily_total)) %>%
  distinct(station_key,wgs84_latitude,wgs84_longitude,lon,lat,average)

data2 <- data %>%
  filter(year==2015,month==4) %>%
  group_by(station_key) %>%
  summarise(n())

data3 <- data %>%
  filter(year==2015,month==3) %>%
  group_by(station_key) %>%
  summarise(n())

data4 <- data %>%
  filter(year==2014,month==3) %>%
  group_by(station_key) %>%
  summarise(n())

data5 <- data %>%
  filter(year==2015,month==3,station_key==56162)

data6 <- data %>%
  filter(year==2015,month==4,station_key==56162)

ggplot(data2,aes(y=month,x=average)) +
  geom_point()
  

datatry <- data %>%
  group_by_all()

?group_by_all
gather()


NSW <- ggmap(get_googlemap(c(lon=147.5, lat=-32.8), zoom=6, maptype="roadmap", size=c(600,500)))

NSW +
  geom_point(aes(x=wgs84_longitude, y=wgs84_latitude,color=average), data=data1, alpha=0.7) +
  geom_point(aes(x=lon, y=lat), data=stations, colour="blue", size=.5) +
  scale_colour_gradientn(colours=rainbow(3)) +
  ggtitle("Traffic counter and weather stations - NSW")

syd <- ggmap(get_googlemap(c(lon=151, lat=-33.85), zoom=10, maptype="roadmap", size=c(600,400),color="bw"))

syd +
  geom_point(aes(x=wgs84_longitude, y=wgs84_latitude, color = average), data=data1,size=2,shape=21) +
  geom_point(aes(x=lon, y=lat), data=stations, colour="blue", size=2) +
  ggtitle("Traffic counter and weather stations - Sydney") + 
  #scale_colour_gradientn(colours=rainbow(3)) 
  scale_color_gradient2(low="#434343",mid="#F2C314",high ="red",midpoint=40000)

?get_googlemap
?scale_color_gradient2

detail <- syd <- ggmap(get_googlemap(c(lon=151.2, lat=-33.85), zoom=13, maptype="roadmap", size=c(600,400)))
detail +
  geom_point(aes(x=wgs84_longitude, y=wgs84_latitude, size = average), data=data1, colour="red",shape=21) +
  geom_point(aes(x=lon, y=lat), data=stations, colour="blue", size=2) +
  ggtitle("Traffic counter and weather stations - CBD")

###'WAFFLE' CHART (THE ONE WITH ICONS)###

library(waffle)
library(extrafont)

##follow steps in link to blog post on Slack - you need to load a set of icons onto your local drive first

font_import() #this takes a long time

#check relevant font has been imported
fonts()[grep("Awesome", fonts())]

loadfonts()

#this next step seemed to be required on my Windows computer
windowsFonts(Awesome=windowsFont("FontAwesome"))

iron(waffle(c(Peak = 80, Offpeak = 20), rows = 5, use_glyph = "car", glyph_size = 6, title = "Highway"),
  waffle(c(Peak = 73, Offpeak = 27), rows = 5, use_glyph = "car", glyph_size = 6, title = "Side street"))
  

