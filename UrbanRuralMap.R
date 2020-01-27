#Running this file produces a map that colorizes the US by urban/rural/extremely rural
#Does not actually include any roe data...
#just a useful visual and potentially nice figure for presentations talking about urban and rural

#From 2010 census
#https://data.world/nrippner/fips-to-zip-code-crosswalk/workspace/file?filename=explanation_zcta_county_rel_10.pdf
library(maptools)
library(stringr)

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}


zip<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/fips_zip_x.csv")
#ZCTA5 = 5 digit zip
#2010 census data
#POPPT = population for relationship record
#ZPOP = population for zip code
#HUPT and ZHU are housing unit counts for the relationship record and ZCTA respectively
#COPOP = 2010 census population living in county
#CPOPOPCT = percentage of the total population of the county that lives withing the ZCTA


# U.S. Department of Veteran Affairs, Office of Rural Health What is Rural? http://www.ruralhealth.va.gov/about/rural-veterans.asp "At this time, the Department of Veterans Affairs utilizes the US Census Bureau's definition for "Urban", "Rural" and "Highly Rural".
# 
# Urban Area: Census Bureau-defined urbanized area, which is any block or block group having a population density of at least 1000 people per square mile.
# Rural Area: Any non-urban or non-highly rural area.
# Highly Rural Area: An area having < 7 civilians per square mile."

zip$ZAREALAND_sqmi<-zip$ZAREALAND/ 2590000 #converting from square meter to square mile
zip$Class<-zip$ZPOP / zip$ZAREALAND_sqmi #Calculating number of people per square mile
zip$Class<-as.factor(cut(zip$Class, c(0, 7, 1000, Inf)))
levels(zip$Class)<-c("HighlyRural", "Rural", "Urban")

zip<-zip[,c("ZCTA5", "STATE", "COUNTY", "Class", "GEOID")]

df<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/DemogswithZip.csv")
df<-df[,c("ID", "szipt")]
df<-df[complete.cases(df),]

df<-merge(df, zip, by.x = "szipt", by.y = "ZCTA5")


library(zipcode)
library(tidyverse)
library(maps)
library(viridis)
library(ggthemes)
library(albersusa)#installed via github
#data
data(zipcode)

df<-merge(df, zipcode, by.x = "szipt", by.y = "zip", all = FALSE)
df<-df[!duplicated(df$ID),]
df$latlong<-as.factor(paste(round(df$latitude, 7), round(df$longitude, 7), sep = "-"))

df_agg<-as.data.frame(table(df$latlong))
df_agg<-merge(df_agg, df[,c("szipt", "Class", "city", "state", "latitude", "longitude", "latlong")],
              by.x = "Var1", by.y = "latlong", all.x = TRUE, all.y = FALSE)
df_agg<-df_agg[!duplicated(df_agg$Var1),]
rm(df)
us<-map_data('state')

#660 x 430
ggplot(df_agg,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color="#BABBB1",fill="#F0EEE2",alpha=.35)+
  geom_point(aes(color = Class, size = Freq),alpha=0.6) +
  scale_colour_manual(values = c("#5B6C88", "#A8D0CF")) +
  xlim(-125,-65)+ylim(25,50) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        rect = element_rect(fill = "white", colour = "black",
                            size = 0.5, linetype = 1),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        #legend.position = "bottom",
        legend.background = element_rect(fill = "white", colour = "white"),
        legend.title = element_blank(),
        legend.position = c(0.96, 0.03),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right") + xlab("Longitude") + ylab("Latitude") +
        guides(size=FALSE)





testPoints <- data.frame(x = round(df_agg$longitude, 3), y = round(df_agg$latitude, 3) )
df_agg$County<-latlong2county(testPoints)

df_agg[2, "County"]<-"florida,pinellas" #manually fixing the one missing county entry
# Remove all before and up to ":":
df_agg$County<-gsub(".*,","",df_agg$County)
zipcode<-zipcode[!is.na(zipcode$latitude),]
testPoints<-data.frame(x = zipcode$longitude, y = zipcode$latitude)
zipcode$County<-latlong2county(testPoints)
zipcode$County<-gsub(".*,","",zipcode$County)

rm(testPoints, us)

cty_sf <- counties_sf("aeqd")
cty_sf$county<-tolower(as.character(cty_sf$name))
cty_sf$state<-as.character(cty_sf$iso_3166_2)
colnames(zipcode)[6]<-"county"
data.fm<-left_join(cty_sf,zipcode,by=c('state','county'))

zip$county<-str_pad(zip$COUNTY, 3, pad = "0")
data.fm$county_fips<-as.character(data.fm$county_fips)
data.fm$state_fips<-as.integer(as.character(data.fm$state_fips))
data.fm2<-merge(data.fm, zip, by.x = c("county_fips", "state_fips"),by.y = c("county", "STATE"))

data.fm3<-data.fm2[!duplicated(data.fm2$GEOID),]
data.fm3$Classnum<-as.numeric(data.fm3$Class)
#Color palette: c("#F0EEE2", "#5B6C88", "#48594E", "#A8D0CF", "#BABBB1")
data.fm3<-data.fm3[!is.na(data.fm3$Class),]

rm(data.fm, data.fm2)

  ggplot() + 
  geom_sf(data = data.fm3, aes(fill = Class, color = Class)) + 
  scale_fill_manual(values = c("#F0EEE2","#5B6C88", "#A8D0CF")) + 
  scale_color_manual(values = c("#F0EEE2","#5B6C88",  "#A8D0CF")) +
    theme(panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(size = 0.5, linetype = "solid",
                           colour = "black"),
  rect = element_rect(fill = "white", colour = "black",
                      size = 0.5, linetype = 1),
  panel.background = element_rect(fill = 'white', colour = 'black'),
  legend.background = element_rect(fill = "white", colour = "white"),
  legend.title = element_blank(),
  legend.position = "bottom")

