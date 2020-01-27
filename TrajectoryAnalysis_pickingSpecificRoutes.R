#The purpose of this code is to identify a single route and then create a really nice etsy-like plot 

library(trajr)
library(osmdata)
library(tidyverse)
library(sf)

FILEPATH_DATA<-"C:/Users/julie.wisch/Documents/RoeCollab/"

df<-read.csv(paste(FILEPATH_DATA, "ParticipantListwithDemogs.csv", sep = ""))
#Breadcrumb data comes from the box file with the daily azuga files. You will have to download the one you want for this
#It's not included in the dataset that I'm putting in the transition box
bc<-read.csv(paste(FILEPATH_DATA, "DailyFiles/Raw/Breadcrumb_30Oct2019.csv", sep = ""))
bc<-bc[,c("Vehicle_Name", "Date", "Time", "Latitude", "Longitude", "Vehicle_Speed", "Speed.Limit", "Event_Type", 
          "Odometer_Reading", "Trip.Distance")]
bc<-bc[bc$Vehicle_Name %in% df$DeviceID | bc$Vehicle_Name %in% df$DeviceID2 | bc$Vehicle_Name %in% df$DeviceID3,]
bc<-bc[bc$Event_Type == "GPS_MESSAGE" | bc$Event_Type == "TRIP_START_MESSAGE" | bc$Event_Type == "TRIP_END_MESSAGE",]



bc<-bc[complete.cases(bc[,c("Vehicle_Name", "Date", "Time", "Latitude", "Longitude", "Event_Type")]),]
bc<-bc[bc$Vehicle_Name == "4103071242",] #Selecting a specific car


bc$rowindex<-1:length(bc$Vehicle_Name)
index<-bc[bc$Event_Type == "TRIP_START_MESSAGE", "rowindex"]

bc$Group<-cut(bc$rowindex, breaks = index)
bc<-bc[!is.na(bc$Group),]


trj<-TrajFromCoords(track = bc[,c("Latitude", "Longitude", "Time")], xCol = "Latitude", yCol = "Longitude", fps = 0.03333)

# Plot original trajectory
plot(trj, lwd = 1, lty = 1) #Defiance, MO



BlackPlot<-function(XMIN, XMAX, YMIN, YMAX){
  p<-ggplot() +
    geom_sf(data = streets$osm_lines,
            inherit.aes = FALSE,
            color = "#FFFFFF",
            size = .4,
            alpha = .8) +
    geom_sf(data = small_streets$osm_lines,
            inherit.aes = FALSE,
            color = "#F7F7F7",
            size = .2,
            alpha = .6) +
    coord_sf(xlim = c(XMIN, XMAX), 
             ylim = c(YMIN, YMAX),
             expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#000000")
    )
  return(p)}


CITY<- "St. Louis County, Missouri USA"
getbb(CITY) #You'll use this to get the lat long that R is going to pull for the data
#You'll want to compare it to the trajectory plot you made above and make sure that you're pulling coordinates
#That cover all the area covered by the original trajectory plot
#If a single county doesn't cut it, you'll have to pull a bunch of them
#Which is what I had to do here....Keep following along....
streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential")) %>%
  osmdata_sf()



CITY<- "Franklin County, Missouri USA"
getbb(CITY)
streets2 <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()


small_streets2 <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential")) %>%
  osmdata_sf()

streets<-c(streets, streets2)
small_streets<-c(small_streets, small_streets2)

#Color palette: c("#F0EEE2", "#5B6C88", "#48594E", "#A8D0CF", "#BABBB1")

pdf(paste(FILEPATH_DATA, "IndividualPathPlot.pdf", sep = ""), width = 6, height = 4)
ggplot(aes(y, x, color = "#5B6C88", fill = "#5B6C88"), data =trj) + 
  geom_point( size = 1.5, alpha = 0.3, color = "#5B6C88", fill = "#5B6C88") +
 # geom_path (linetype=1, size=1, arrow = arrow(angle=15, type="closed")) +
  geom_path (linetype=1, size=1, color = "#5B6C88", fill = "#5B6C88") +
  
    geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#A8D0CF",
          size = .4,
          alpha = .8)+
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#A8D0CF",
          size = .2,
          alpha = .6) +
  coord_sf(xlim = c(-90.83, -90.68), #manually tweak latitude and longitude until you get the right framing for the map
           ylim = c(38.68, 38.725),
           expand = FALSE) + theme(legend.position = "none") + xlab("") + ylab("") +
  theme(
    plot.background = element_rect(fill = "#F0EEE2"),
    panel.background = element_rect(fill = "#F0EEE2"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.text.x = element_text(colour = "#A8D0CF"),
    axis.text.y = element_text(colour = "#A8D0CF"),
    axis.ticks.x = element_line(colour = "#A8D0CF"),
    axis.ticks.y = element_line(colour = "#A8D0CF")
  )
dev.off()
