library(trajr)

df<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/ParticipantListwithDemogs.csv")
bc<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/Breadcrumb_07Jan2020.csv")
bc<-bc[,c("Vehicle_Name", "Date", "Time", "Latitude", "Longitude", "Vehicle_Speed", "Speed.Limit", "Event_Type", 
          "Odometer_Reading", "Trip.Distance")]
bc<-bc[bc$Vehicle_Name %in% df$DeviceID | bc$Vehicle_Name %in% df$DeviceID2 | bc$Vehicle_Name %in% df$DeviceID3,]
bc<-bc[bc$Event_Type == "GPS_MESSAGE" | bc$Event_Type == "TRIP_START_MESSAGE" | bc$Event_Type == "TRIP_END_MESSAGE",]

# oneT<-bc[bc$Vehicle_Name == 5042072106,]
# oneT<-bc[1:12,]

bc<-bc[complete.cases(bc[,c("Vehicle_Name", "Date", "Time", "Latitude", "Longitude", "Event_Type")]),]
bc$rowindex<-1:length(bc$Vehicle_Name)
index<-bc[bc$Event_Type == "TRIP_START_MESSAGE", "rowindex"]

bc$Group<-cut(bc$rowindex, breaks = index)



out <- split( bc , f =bc$Group) #Creating a list of trajectories - each trip has one list element

datalist = list()
for(i in 1:length(out)){
#Then further splitting the list out into each trip
hold<-out[[i]]
# hold$rowindex<-1:length(hold$Vehicle_Name)
# index<-hold[hold$Event_Type == "TRIP_START_MESSAGE", "rowindex"]
# hold<-hold[!(duplicated(hold$Latitude) & duplicated(hold$Longitude)),]

trj<-TrajFromCoords(track = hold[,c("Latitude", "Longitude", "Time")], xCol = "Latitude", yCol = "Longitude", fps = 0.03333)

# Plot original trajectory
# plot(trj, lwd = 1, lty = 1)
# TrajStraightness(trj) #measure of distance/length of route. Tells how straight the route was. Between 0 and 1. 1 means perfect straight
# TrajSinuosity2(trj) #The sinuosity index defined by Benhamou (2004) may be an appropriate measure of the tortuosity of a random search path.
# #Directional change is defined as the change in direction over time, and has been used to assess locomotor mimicry in butterflies (Kitamura & Imafuku, 2015). 
# #Directional change is defined for each pair of steps, so a trajectory (or a portion thereof) may be characterised by the mean (DC) and standard deviation (SDDC) of all directional changes. 
# mean(TrajDirectionalChange(trj))
# sd(TrajDirectionalChange(trj))




#for (i in 1:length(file_list)) {
  result<-data.frame("Vehicle_Name" = hold[1, "Vehicle_Name",], "Date" = hold[1, "Date"], 
                     "StartTime" = hold[1, "Time"], "Straightness" = TrajStraightness(trj), 
                     "Sinuosity" = TrajSinuosity2(trj), "meanTDC" = mean(TrajDirectionalChange(trj)),
                     "sdTDC" = sd(TrajDirectionalChange(trj)))

  datalist[[i]] <- result
}

result = do.call(rbind, datalist)

result<-result[!is.na(result$Straightness),]


#############################################################################
#############################################################################
#This is how you can smooth the route...not sure that I'll go that route
# Create a smoothed trajectory, filter order 3, length 5
# smoothed <- TrajSmoothSG(trj, p = 3, n = 5)
# # Plot it in slightly transparent red
# lines(smoothed, col = "#FF0000A0", lwd = 2)
# legend("topright", c("Original", "Smoothed"), lwd = c(1, 2), lty = c(1, 1), col = c("black", "red"), inset = 0.01)
#############################################################################
#############################################################################
test<-bc[bc$Vehicle_Name == 7082363338,]
trj<-TrajFromCoords(track = test[120:194,c("Latitude", "Longitude", "Time")], xCol = "Latitude", yCol = "Longitude", fps = 0.03333)
plot(trj)
