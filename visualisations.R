###GOOGLE MAPS###

library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)

NSW <- ggmap(get_googlemap(c(lon=147.5, lat=-32.8), zoom=6, maptype="roadmap", size=c(600,500)))

NSW +
  geom_point(aes(x=wgs84_longitude, y=wgs84_latitude), data=stations, colour="red", size=.3, alpha=0.5) +
  geom_point(aes(x=lon, y=lat), data=stations, colour="blue", size=.5) +
  ggtitle("Traffic counter and weather stations - NSW")

syd <- ggmap(get_googlemap(c(lon=151, lat=-33.85), zoom=10, maptype="roadmap", size=c(600,400)))

syd +
  geom_point(aes(x=wgs84_longitude, y=wgs84_latitude), data=stations, colour="red", size=.8,alpha=0.8) +
  geom_point(aes(x=lon, y=lat), data=stations, colour="blue", size=2) +
  ggtitle("Traffic counter and weather stations - Sydney")

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
  

