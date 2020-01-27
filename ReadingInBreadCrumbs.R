
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
bc<-bc[bc$Event_Type == "GPS_MESSAGE" | bc$Event_Type == "TRIP_START_MESSAGE" | bc$Event_Type == "TRIP_END_MESSAGE",]

bc<-bc[complete.cases(bc[,c("Vehicle_Name", "Date", "Time", "Latitude", "Longitude", "Event_Type")]),]
bc$rowindex<-1:length(bc$Vehicle_Name)
index<-bc[bc$Event_Type == "TRIP_START_MESSAGE", "rowindex"]

bc$Group<-cut(bc$rowindex, breaks = index)

out <- split( bc , f =bc$Group) #Creating a list of trajectories - each trip has one list element
out<-bc
return(out)}
#out<-CleanItUp(bc[[1]])
out<-lapply(bc, CleanItUp)
datalist = list()

# for(i in 1:length(out)){
#   temp<-out[[i]]
#   Groups<-levels(temp$Group)
#   temp<-temp[temp$Event_Type == "GPS_MESSAGE",]
#   trjlist<-list()
#   for(j in 1:length(Groups)){
#     temp<-temp[temp$Group == Groups[j],]
#     trj<-TrajFromCoords(track = temp[,c("Latitude", "Longitude", "Time")], xCol = "Latitude", yCol = "Longitude", fps = 0.03333)
#     trjlist[[j]] <- trj
#   }
# }

k<-0
for(i in 1:length(out)){
  #Then further splitting the list out into each trip
  temp<-out[[i]]
  Groups<-levels(temp$Group)

  temp<-temp[temp$Event_Type == "GPS_MESSAGE",]
  temp<-temp[temp$Group %in% Groups,]
  temp$Group<-as.character(temp$Group)

  for(j in 1:length(Groups)){
    k<- k+1
    temp_hold<-temp[temp$Group == Groups[j],]
    if(length(temp_hold$Vehicle_Name) > 1){
    trj<-TrajFromCoords(track = temp_hold[,c("Latitude", "Longitude", "Time")], xCol = "Latitude", yCol = "Longitude", fps = 0.03333)
    result<-data.frame("Vehicle_Name" = temp_hold[1, "Vehicle_Name",], "Date" = temp_hold[1, "Date"], 
                       "StartTime" = temp_hold[1, "Time"], "Straightness" = TrajStraightness(trj), 
                       "Sinuosity" = TrajSinuosity2(trj), "meanTDC" = mean(TrajDirectionalChange(trj)),
                       "sdTDC" = sd(TrajDirectionalChange(trj)))
    
    datalist[[k]] <- result
    }}}
  
  result = do.call(rbind, datalist)
  
write.csv(result, "C:/Users/julie.wisch/Documents/RoeCollab/TrajRresults.csv")
