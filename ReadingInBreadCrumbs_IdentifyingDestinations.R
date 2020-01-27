
df<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/ParticipantListwithDemogs.csv")

file_list <- list.files(path="C:/Users/julie.wisch/Documents/RoeCollab/DailyFiles/Raw/", pattern="*.csv") 
file_list<-grep('Breadcrumb', file_list, value=TRUE) #Keeping only the files that contain breadcrumb data in my file list
#This is just data from like the last year and a half.....


setwd("C:/Users/julie.wisch/Documents/RoeCollab/DailyFiles/Raw/")
bc = lapply(file_list, read.csv)

CleanItUp<-function(bc){
  bc<-data.frame(bc)
bc<-bc[,c("Vehicle_Name", "Date", "Time", "Latitude", "Longitude", "Vehicle_Speed", "Event_Type", 
          "Odometer_Reading", "Trip.Distance")]
bc<-bc[bc$Vehicle_Name %in% df$DeviceID | bc$Vehicle_Name %in% df$DeviceID2 | bc$Vehicle_Name %in% df$DeviceID3,]
bc<-bc[bc$Event_Type == "TRIP_END_MESSAGE",]

bc<-bc[complete.cases(bc[,c("Vehicle_Name", "Date", "Time", "Latitude", "Longitude")]),
       c("Vehicle_Name", "Date", "Time", "Latitude", "Longitude")]

# bc$rowindex<-1:length(bc$Vehicle_Name)
# index<-bc[bc$Event_Type == "TRIP_START_MESSAGE", "rowindex"]
# 
# bc$Group<-cut(bc$rowindex, breaks = index)
# 
# out <- split( bc , f =bc$Group) #Creating a list of trajectories - each trip has one list element
out<-bc
return(out)}
#out<-CleanItUp(bc[[1]])
out<-lapply(bc, CleanItUp)

  result = do.call(rbind, out)
  
write.csv(result, "C:/Users/julie.wisch/Documents/RoeCollab/Destinations.csv")

library(googleway)
api_key <- 'AIzaSyA5FUPHv6mj70ENpeuwxOQGX7TfOagh-FA'

google_places(location = c(result$Latitude[1],result$Longitude[1]),
              radius = 1,
              key = api_key)
