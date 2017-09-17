library("readr") # lib.loc="C:/JM_Local/Rlibrary"
library(dplyr)
library(sqldf)

# JM - read consolidated weather file
BOMrain <- read_rds("C:/JM_local/Rworking/BoM/BOM_NSW_rain.RDS") 

# JM - limit to station number, lat and long
BOMrainStation <- data.frame(subset(BOMrain, select = c(1,2, 5,6)))
colnames(BOMrainStation) <- c("StationNum","name", "lat", "lon")

# JM - take unique values (ie. one record per weather station)

# BOMrain1 <- unique(BOMrainStation) didn't give expected result
# BOMrain2 <- duplicated(BOMrainStation) didn't give expected result
# BOMrain2<-BOMrain[duplicated(BOMrainStation), ] didn't give expected result
# BOMrain2 <- duplicated(data.frame(BOMrainStation)) didn't give expected result
UniqueBOMStations <- sqldf('SELECT DISTINCT * FROM BOMrainStation') # JM - finally one that does work as expected (SQL never fails me :)

# ----------------------------------------------------------
library(geosphere)

# Read Traffic stations file
TrafficStations<-read.csv(file="C:/JM_local/Rworking/Ass2/Traffic_Volume_Viewer_-_Station_Information.csv", header=TRUE)

# create distance matrix
DistMatrix <- distm(TrafficStations[,c('lon','lat')], UniqueBOMStations[,c('lon','lat')], fun=distVincentyEllipsoid)/1000
DistMatrix
# find the StationNum of the closest UniqueBOMStation based on shortest distance in the matrix
NearestStation <- data.frame(UniqueBOMStations$StationNum[max.col(-DistMatrix)])
# Only take the unique BOM Stations required
UniqueNearestStation<-unique(NearestStation)
# UniqueNearestStation <- sqldf('SELECT DISTINCT * FROM NearestStation') This time unique appears to work as expected

