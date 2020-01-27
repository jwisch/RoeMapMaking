#From 2010 census
#https://data.world/nrippner/fips-to-zip-code-crosswalk/workspace/file?filename=explanation_zcta_county_rel_10.pdf

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

zip<-zip[,c("ZCTA5", "STATE", "Class")]

df<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/DemogswithZip.csv")
df<-df[,c("ID", "szipt")]
df<-df[complete.cases(df),]

df<-merge(df, zip, by.x = "szipt", by.y = "ZCTA5")
df<-df[,c("ID", "Class")]

device<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/DeviceMasterlist050719.csv")
colnames(device)[1:4]<-c("DeviceID", "DeviceID2", "DeviceID3", "ID")
device$Installation.date<-as.Date(device$Installation.date, format = "%m/%d/%Y")
device<-device[,c("ID", "DeviceID", "DeviceID2", "DeviceID3", "Installation.date")]

#Getting characteristics of urban and rural folks
demogs<-read.csv("C:/Users/julie.wisch/Documents/ADRC/Data/DR_demographics_20190122.csv")
device<-merge(device, demogs, by = "ID", all = FALSE)
rm(demogs)
device<-merge(device, df[,c("ID", "Class")], by = "ID", all = FALSE)
device<-device[!duplicated(device),]
device<-device[,c("ID", "Class", "DeviceID", "DeviceID2", "DeviceID3", "Installation.date", "BIRTH", "GENDER", "EDUC", "apoe", "race2")]
device$BIRTH<-as.Date(format(as.Date(device$BIRTH, "%d-%b-%y"), "19%y-%m-%d"))
device$AgeatInstall<-round(as.numeric(device$Installation.date - device$BIRTH)/365, 1)
device$apoe4<-ifelse(device$apoe == 24, 1, ifelse(device$apoe < 34, 0, 1))
device$Class<-droplevels(device$Class)

ADI<-read.csv("C:/Users/julie.wisch/Documents/ADRC/ADI/ADI_all.csv")
device<-merge(device, ADI[,c("id", "ADI_NATRANK")], by.x = "ID", by.y = "id", all.x = TRUE, all.y = FALSE) #missing ADI for 4 participatns
rm(ADI)

PIB<-read.csv("C:/Users/julie.wisch/Documents/ADRC/DR15/HASD_ACS_DR15_PIB.csv")
PIB<-PIB[PIB$Map %in% device$ID,]
length(unique(PIB[PIB$Map %in% device$ID,"Map"])) #115/174 have a PIB scan

AV45<-read.csv("C:/Users/julie.wisch/Documents/ADRC/DR15/HASD_ACS_DR15_AV45.csv")
AV45<-AV45[AV45$Map %in% device$ID,]
length(unique(AV45[AV45$Map %in% device$ID,"Map"])) #149/174 have an AV45 scan

CSF<-read.csv("C:/Users/julie.wisch/Documents/ADRC/DR15/OI_Schindler_FullNFL_2019_06_10.csv")
CSF<-CSF[complete.cases(CSF$E_ab42),]
CSF<-CSF[CSF$ELECSYS_BATCH == "1500",]
CSF<-CSF[CSF$MAP_ID %in% device$ID,] #130/174 have at least one LP

#Because of the data availability, will use AV45 as my classifier
rm(CSF, PIB)

AV45<-AV45[,c("Map", "PET_Date", "AV45_fSUVR_rsf_TOT_CORTMEAN")]
colnames(AV45)[1]<-"ID"
AV45<-AV45[complete.cases(AV45$AV45_fSUVR_rsf_TOT_CORTMEAN),]
AV45$PET_Date<-as.Date(AV45$PET_Date, format = "%m/%d/%Y")
AV45$Apos<-ifelse(AV45$AV45_fSUVR_rsf_TOT_CORTMEAN > 1.19, 1, 0)
hold<-AV45[AV45$ID %in% AV45$ID[duplicated(AV45$ID)],]
hold[with(hold, order(ID)),]
#Converts from A- to A+: 63041, 64715
#Converts from A+ to A-: 63984, 68015
rm(hold)

AV45once<-AV45[with(AV45, order(ID, PET_Date)),]
AV45once<-aggregate(AV45once, list(AV45once$ID), tail, 1) #keeping only most recent scan
AV45once<-AV45once[!(AV45once$ID == 63984 | AV45once$ID == 68015),] #Dropping the two weirdos who revert to amyloid negative

device<-merge(device, AV45once, by = "ID", all.x = TRUE, all.y = FALSE)
library(tableone)
myVars <- c("AgeatInstall", "GENDER", "EDUC", "apoe4", "race2", "ADI_NATRANK", "Apos")
catVars <- c("GENDER", "apoe4", "race2", "Apos")
CreateTableOne(vars = myVars, data = device, factorVars = catVars, strata = c("Class"))
#sort of surprised- our participants are pretty affluent regardless of urban/rural

CreateTableOne(vars = myVars, data = device, factorVars = catVars, strata = c("Class", "Apos"))
#LOOK AT ADI x RURAL Apos



sum<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/2015toApr2019.csv")
sum$MonYr<-paste(sum$MonYr, "01", sep = "")
sum$MonYr<-as.Date(sum$MonYr, format = "%Y%m%d")

hold<-merge(device, sum, by.x = "DeviceID3", by.y = "Driver_ID", all = FALSE)
hold2<-merge(device, sum, by.x = "DeviceID2", by.y = "Driver_ID", all = FALSE)
hold3<-merge(device, sum, by.x = "DeviceID", by.y = "Driver_ID", all = FALSE)

df<-data.frame(rbind(hold3, hold2, hold))
rm(hold, hold2, hold3)



library(lme4)
library(ggplot2)
#If you include an interaction wtih amyloid status, there are no significant amyloid status differences
#Or class by amyloid differences
#However, i think part of this is just a reduction in power as there are only 10 amyloid positive rural participants
#Can get nice clean differences in ways taht make sense if we only compare urban and rural drivers

summary(lmer(TripsPerMonth ~ Class*Apos + (1|ID), data = df))
ggplot(df, aes(x = Class, y = TripsPerMonth, fill = Class)) + geom_boxplot()
library(lmerTest)

#t significance for 174 participants = 1.974
#t significance for 241 participants and 21 tests is 3.083
anova(lmer(MilesPerMonth ~ Class*Apos + (1|ID), data = df)) #Significant, urban less; withstands bonferroni

anova(lmer(MonthlyHoursDriven ~ Class*Apos + (1|ID), data = df))#Significant, urban less
anova(lmer(MonthlyDaysDriven ~ Class*Apos + (1|ID), data = df))
anova(lmer(AvgTripMilesByMonth ~ Class*Apos + (1|ID), data = df))#Significant, urban less; withstands bonferroni
anova(lmer(MedianTripMilesByMonth ~ Class*Apos + (1|ID), data = df))#Significant, urban less; withstands bonferroni
anova(lmer(AvgTripDurationMinsByMonth ~ Class*Apos + (1|ID), data = df))#Significant, urban less
anova(lmer(MedianTripDurationMinsByMonth ~ Class*Apos + (1|ID), data = df))

df$PercTripsUnder1Mile<-df$MonthlyTripsUnder1Mile/df$TripsPerMonth
df$PercTrips1to5Miles<-df$MonthlyTrips1to5Miles/df$TripsPerMonth
df$PercTrips5to10Miles<-df$MonthlyTrips5to10Miles/df$TripsPerMonth
df$PercTrips10to20Miles<-df$MonthlyTrips10to20Miles/df$TripsPerMonth
df$PercTrips20MilesOrLonger<-df$MonthlyTrips20MilesOrLonger/df$TripsPerMonth


anova(lmer(MonthlyTripsUnder1Mile ~ Class*Apos + (1|ID), data = df))
anova(lmer(PercTrips1to5Miles ~ Class*Apos + (1|ID), data = df))#Significant, urban more; withstands bonferroni
anova(lmer(PercTrips5to10Miles ~ Class*Apos + (1|ID), data = df))
anova(lmer(PercTrips10to20Miles ~ Class*Apos + (1|ID), data = df))
anova(lmer(PercTrips20MilesOrLonger ~ Class*Apos + (1|ID), data = df))#Significant, urban less; withstands bonferroni


df$PercTripswithHardBraking<-df$MonthlyTripsHardBraking/df$TripsPerMonth
df$PercTripswithSuddenAcc<-df$MonthlyTripsSuddenAcc/df$TripsPerMonth
df$PercTimeSpeeding<-df$MonthlyMinsSpeeding/(df$MonthlyHoursDriven*60)
df$PercTripswithSomeAggression<-df$MonthlyTripsSomeAggression/df$TripsPerMonth
df$PercTripsInDaylight<-df$MonthlyTripsInDaylight/df$TripsPerMonth
df$PercTripsInDarkness<-df$MonthlyTripsInDarkness/df$TripsPerMonth
df$PercTripsAtDawn<-df$MonthlyTripsAtDawn/df$TripsPerMonth
df$PercTripsAtDusk<-df$MonthlyTripsAtDusk/df$TripsPerMonth
df$PercTripsAtNight<-df$MonthlyTripsAtNight/df$TripsPerMonth

anova(lmer(PercTripswithHardBraking ~ Class*Apos + (1|ID), data = df))
anova(lmer(PercTripswithSuddenAcc ~ Class*Apos + (1|ID), data = df))
anova(lmer(PercTimeSpeeding ~ Class*Apos + (1|ID), data = df)) #Significant, urban less; withstands bonferroni
anova(lmer(PercTripswithSomeAggression ~ Class*Apos + (1|ID), data = df))#Significant, urban less
anova(lmer(PercTripsInDaylight ~ Class*Apos + (1|ID), data = df))
anova(lmer(PercTripsInDarkness ~ Class*Apos + (1|ID), data = df))
anova(lmer(PercTripsAtDawn ~ Class*Apos + (1|ID), data = df)) #Significant, urban less; withstands bonferroni
anova(lmer(PercTripsAtDusk ~ Class*Apos + (1|ID), data = df)) #Significant, urban less; withstands bonferroni
anova(lmer(PercTripsAtNight ~ Class*Apos + (1|ID), data = df))

PLOT<-function(YVAL, YLAB){
  p<-ggplot(df, aes(x = Class, y = YVAL, fill = Class), alpha = 0.5, colour = "white") + geom_boxplot() + xlab("") +
    scale_fill_manual(values = c("#5B6C88",  "#A8D0CF")) + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size = 0.5, linetype = "solid",
                                   colour = "black"),
          rect = element_rect(fill = "white", colour = "black",
                              size = 0.5, linetype = 1),
          panel.background = element_rect(fill = 'white', colour = 'black'),
          # legend.background = element_rect(fill = "white", colour = "white"),
          # legend.title = element_blank()
          legend.position = "none") + ylab(YLAB)
  return(p)
}

p1<-PLOT(df$MilesPerMonth, "Miles Driven per Month") 
p2<-PLOT(df$AvgTripMilesByMonth, "Average number of miles per trip per month")
p3<-PLOT(df$PercTrips1to5Miles, "Percent of trips 1 - 5 miles long")
p4<-PLOT(df$PercTrips20MilesOrLonger, "Percent of trips 20 miles or longer")
p5<-PLOT(df$PercTimeSpeeding, "Percent of time spent speeding")
p6a<-PLOT(df$PercTripsAtDawn, "Percent of trips taken at dawn")
p6b<-PLOT(df$PercTripsAtDusk, "Percent of trips taken at dusk") + ylim(c(0, 0.27))
library(gridExtra)
library(lemon)
lay<-rbind(c(1, 1, 1, 2, 2, 2), c(3, 3, 3, 4, 4, 4), c(5, 5, 6, 6, 7, 7))
grid.arrange(p1, p2, p3, p4, p5, p6a, p6b, layout_matrix = lay)
grid_arrange_shared_legend(p1, p2, p3, p4, p5, p6a, p6b, nrow = 2, ncol = 4)
grid_arrange_shared_legend(p6a, p6b, nrow = 1, ncol = 2)

#Nothing super surprising, but nice that there are some real differences....

rm(p1, p2, p3, p4, p5, p6a, p6b)

write.csv(device, "C:/Users/julie.wisch/Documents/RoeCollab/ParticipantListwithDemogs.csv", row.names = FALSE)
