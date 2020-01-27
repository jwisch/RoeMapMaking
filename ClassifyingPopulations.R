#sourced from: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=CF
pop<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/Population.csv")

# U.S. Department of Veteran Affairs, Office of Rural Health What is Rural? http://www.ruralhealth.va.gov/about/rural-veterans.asp "At this time, the Department of Veterans Affairs utilizes the US Census Bureau's definition for "Urban", "Rural" and "Highly Rural".
# 
# Urban Area: Census Bureau-defined urbanized area, which is any block or block group having a population density of at least 1000 people per square mile.
# Rural Area: Any non-urban or non-highly rural area.
# Highly Rural Area: An area having < 7 civilians per square mile."
pop$Classification<-cut(pop$Density.per.square.mile.of.land.area...Population, c(0, 7, 1000, Inf))
levels(pop$Classification)<-c("HighlyRural", "Rural", "Urban")

#Need to use regex to seperate out states and cities into seperate columns to make things easier later

#A space (), then any character (.) any number of times (*) until the end of the string ($). 
pop$Geographic.area.state<-sub(".* - *(.*?) * - .*", "\\1", pop$Geographic.area)
pop$Geographic.area.city<-sub('.*\\-', '', pop$Geographic.area)

pop<-pop[!grepl("United States", pop$Geographic.area.state),] #dropping country and state level rows
pop<-pop[!grepl("Puerto Rico", pop$Geographic.area.state),] 

write.csv(pop[,c("Geographic.area.state", "Geographic.area.city", "Classification")],
                  "C:/Users/julie.wisch/Documents/RoeCollab/UrbanRuralbyCity.csv", row.names = FALSE)

demogs<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/DemogswithZip.csv")

##############################################################################
library(noncensus)
library(dplyr)
pop<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/UrbanRuralbyCity.csv")
pop <- mutate_all(pop, .funs=toupper)
data(zip_codes)
zip_codes<-mutate_all(zip_codes, .funs = toupper)

ABB<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/stateabbrevs.csv")
ABB<- mutate_all(ABB, .funs = toupper)

pop<-merge(pop, ABB[,c("State", "Code")], by.x = "Geographic.area.state", by.y = "State", all = FALSE)
#Need to drop CITY VILLAGE CDP TOWN from the city names in pop
pop$Geographic.area.city<-sub(" CITY", "", pop$Geographic.area.city)
pop$Geographic.area.city<-sub(" VILLAGE", "", pop$Geographic.area.city)
pop$Geographic.area.city<-sub(" CDP", "", pop$Geographic.area.city)
pop$Geographic.area.city<-sub(" TOWN", "", pop$Geographic.area.city)

zip_codes<-merge(zip_codes, pop[,c("Geographic.area.city", "Code", "Classification")], by.x = c("city", "state"),
                 by.y = c("Geographic.area.city", "Code"), all = FALSE)

hold<-zip_codes[complete.cases(zip_codes$zip) & complete.cases(zip_codes$Classification),]

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



idea<-merge(zip, pop, by.x = "GEOID", by.y = "Target.Geo.Id2", all = FALSE)
zip$ZAREALAND_sqmi<-zip$ZAREALAND/ 2590000 #converting from square meter to square mile
zip$Class<-zip$ZPOP / zip$ZAREALAND_sqmi
zip$Class<-as.factor(cut(zip$Class, c(0, 7, 1000, Inf)))
levels(zip$Class)<-c("HighlyRural", "Rural", "Urban")
