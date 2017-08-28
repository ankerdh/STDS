# runs through 3 nested loops for combinations of origin, destination and departure time
# queries the Google Maps Distance API using gmapsdistance()
# writes the resulting driving time estimates to a timestamped csv file

#you will need to install and load gmapsdistance if you haven't already

#install.packages("gmapsdistance")
#library(gmapsdistance)

#we have a single list of places and a single list of times in separate csvs
places <- read.csv("places.csv")
times <- read.csv("times.csv", stringsAsFactors = FALSE)

#initialise a data frame to catch the results
results_frame <- data.frame(
  Time=numeric(), 
  Distance=numeric(),
  Status=character(), 
  origin=character(),
  destination=character(),
  depart_num=numeric(),
  depart_timestamp=as.POSIXct(character()),
  predict_num=numeric(),
  predict_timestamp=as.POSIXlt(character()),
  stringsAsFactors=FALSE) 

#loop through departure times from the times list

for (time_count in (1:4)) {

  #note the time we are getting the data and the departure time
  predict_num <- as.numeric(as.POSIXct(Sys.time(), "Australia/Sydney"))
  depart_num <- as.numeric(as.POSIXct(times$time[time_count], "Australia/Sydney"))
  predict_timestamp <- as.POSIXlt(predict_num, "Australia/Sydney", origin ="1970-01-01")
  depart_timestamp <- as.POSIXlt(depart_num, "Australia/Sydney", origin ="1970-01-01")
  
  #only if departure is in the future
  
  if (predict_num <= depart_num) {
  
  #loop through origins and destinations from the places list
  
  for (origin_count in (1:3)) {
  
     for (dest_count in (1:2)) {
      
      if (origin_count != dest_count) { 
        
      #set arguments to feed the API
      origin <-places$place[origin_count]
      destination <- places$place[dest_count]
      
      #query the API
      results = gmapsdistance(
          origin = origin, 
          destination = destination, 
          mode = "driving", 
          traffic_model = "best_guess",
          departure = depart_num,
          shape = "long",
          key = "AIzaSyDDihK34ya701nYseOdUXLDwH7XWfYMuC0")       # *****HEY GUYS, you need add your own API key in here*****
          
      results$origin <- origin
      results$destination <- destination
      results$depart_num <- depart_num
      results$depart_timestamp <- depart_timestamp
      results$predict_num <- predict_num
      results$predict_timestamp <- predict_timestamp
      
      return <- data.frame(results)
      results_frame <- rbind(results_frame,return)

        }#end of if origin !=destination
     }#end of dest loop
    }#end of origin loop
  }#end of if departure is in future
}#end of departure time loop

write.csv(results_frame, paste("results ",format(results_frame$predict_timestamp[1], "%Y-%m-%d-%H-%M-%S", tz="Australia/Sydney"), ".csv", sep = ""))

