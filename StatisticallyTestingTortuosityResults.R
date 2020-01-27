df<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/DailyFiles/BreadCrumbSampleResults.csv")
df2<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/ParticipantListwithDemogs.csv")


hold1<-merge(df, df2, by.x = "Vehicle_Name", by.y = "DeviceID", all = FALSE)
hold1<-hold1[,c("ID", "Vehicle_Name", "Date", "StartTime", "Class", "Installation.date", "GENDER", "EDUC",
                "race2", "AgeatInstall", "apoe4", "ADI_NATRANK", "Group.1", "Apos", "Straightness", "Sinuosity", "meanTDC", "sdTDC")]
hold2<-merge(df, df2, by.x = "Vehicle_Name", by.y = "DeviceID2", all = FALSE)
hold2<-hold2[,c("ID", "Vehicle_Name", "Date", "StartTime", "Class", "Installation.date", "GENDER", "EDUC",
                "race2", "AgeatInstall", "apoe4", "ADI_NATRANK", "Group.1", "Apos", "Straightness", "Sinuosity", "meanTDC", "sdTDC")]

hold3<-merge(df, df2, by.x = "Vehicle_Name", by.y = "DeviceID3", all = FALSE)
hold3<-hold3[,c("ID", "Vehicle_Name", "Date", "StartTime", "Class", "Installation.date", "GENDER", "EDUC",
                "race2", "AgeatInstall", "apoe4", "ADI_NATRANK", "Group.1", "Apos", "Straightness", "Sinuosity", "meanTDC", "sdTDC")]


df<-data.frame(rbind(hold1, hold2))

library(lme4)
summary(lmer(Straightness ~ Class + (1|ID), data = df))

anova(lmer(Straightness ~ Class + (1|ID), data = df))
summary(lmer(Straightness ~ Apos + (1|ID), data = df))



summary(lmer(Sinuosity ~ Class*Apos + (1|ID), data = df))
summary(lmer(meanTDC ~ Class*Apos + (1|ID), data = df))


