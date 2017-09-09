library(rsdmx)
library(data.table)
library(geosphere)
library(reshape2)
library(dplyr)
library(gmapsdistance)

#read in selected variables from the ABS table ABS_REGIONAL_LGA

# LGA
LGAurl <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ABS_REGIONAL_LGA/ERP_18+ERP_21+200IND+MVC_14+MVC_15+MVC_16+MVC_17+MVC_18+MVC_19+MVC_20+MVC_21+MVC_22+MVC_38+LAND.LGA2014..A/all?startTime=2011&endTime=2016&format=compact_v2"
LGA_raw <- readSDMX(LGAurl)
LGA <- as.data.frame(LGA_raw)

# Names
LGAdsd_url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/ABS_REGIONAL_LGA"
LGAdsd <- readSDMX(LGAdsd_url)

#get codelists from DSD
LGAcls <- slot(LGAdsd, "codelists")
LGAcodelists <- sapply(slot(LGAcls, "codelists"), function(x) slot(x, "id")) #get list of codelists
LGAcodelists
LGAcodelist <- as.data.frame(slot(LGAdsd, "codelists"), codelistId = "CL_ABS_REGIONAL_LGA_REGION") #get a codelist
#get concepts from DSD
LGAconcepts <- as.data.frame(slot(LGAdsd, "concepts"))
#join
LGA_full <- inner_join(LGA,LGAcodelist,c("REGION" = "id")) 

#transform LGA region names into correct format to match other data
LGA_full$label.en <- gsub(" \\(.*$","", LGA_full$label.en)

#apply 2014 LGA land area to all years
land.area.2014 <- LGA_full[LGA_full$MEASURE=="LAND",c("REGION","OBS_VALUE")]
LGA_full <- merge(x = LGA_full, y = land.area.2014, by = "REGION", all.x = TRUE)

#make a wide table
LGA.wide <- dcast(LGA_full, REGION + label.en + TIME + OBS_VALUE.y ~ MEASURE, value.var = c("OBS_VALUE.x"))

#change col names
setnames(LGA.wide,c("label.en","TIME", "OBS_VALUE.y","ERP_21","ERP_18", "MVC_14", "MVC_38"),c("lga","year","lga.area.2014","pop.density",'pop.work.age.percent',"vehicles.rego.cars","vehicles.rego.total"))

#change variable classes and create densities for vehicles
LGA.wide$year <- as.integer(LGA.wide$year)
LGA.wide[,c("lga.area.2014","pop.density",'pop.work.age.percent',"vehicles.rego.cars","vehicles.rego.total")] <- as.numeric(unlist(LGA.wide[,c("lga.area.2014","pop.density",'pop.work.age.percent',"vehicles.rego.cars","vehicles.rego.total")]))
LGA.wide$density.vehicles.cars <- LGA.wide$vehicles.rego.cars / LGA.wide$lga.area.2014
LGA.wide$density.vehicles.total <- LGA.wide$vehicles.rego.total / LGA.wide$lga.area.2014

#subset the final columns we need
LGA.wide <- LGA.wide[,c("lga","year","lga.area.2014","pop.density",'pop.work.age.percent',"density.vehicles.cars","density.vehicles.total")]

#left join to add ABS data for LGA/Year to each row of traffic data

###   NOTE  NOTE   NOTE  #######just a TEST for now, not the full traffic dataset
traffic_vol_test<- traffic_vol[seq(1, 90000, by = 6500),]
traffic.with.abs <- merge(x = traffic_vol_test, y = LGA.wide, by = c("lga", "year"), all.x = TRUE)

#now calculate distance from Sydney CBD for each station

#make list of stations
###   NOTE  NOTE   NOTE  #######just a TEST for now, not the full traffic dataset
station.locations <- unique(traffic_vol_test[,c("station_id","wgs84_latitude","wgs84_longitude")])

#create function to calculate distance by road to Sydney Tower
get.distance <- function(x,y,z) {
  results = gmapsdistance(
  origin = paste(x,y,sep=","), 
  destination = "-33.8704512,151.2058792", 
  mode = "driving", 
  traffic_model = "best_guess",
  shape = "long",
  key = "AIzaSyDDihK34ya701nYseOdUXLDwH7XWfYMuC0")
  results.df<-data.frame(results)
  results.df$station_id <- z
  results.df
}

#calculate distance for each station
distance.df <- get.distance(station.locations$wgs84_latitude,station.locations$wgs84_longitude, station.locations$station_id)
distances.table <- distance.df[,c("station_id", "Distance.Distance")]

#add to the traffic table
traffic.with.abs <- merge(x = traffic.with.abs, y = distances.table, by = "station_id", all.x = TRUE)

