CDR<-read.csv("C:/Users/julie.wisch/Documents/ADRC/Data/DR_clin_092618.csv")
CDR<-CDR[,c("ID", "CDR", "TESTDATE")]
CDR<-CDR[CDR$ID %in% df_hold$ID,]
unique(CDR[CDR$CDR == 0.5, "ID"])
CDR$TESTDATE<-as.Date(CDR$TESTDATE, format = "%d-%b-%y")

MCI<-CDR[CDR$CDR == 0.5,]
Norm<-CDR[!(CDR$ID %in% MCI$ID),]

#Keeping earliest date of CDR > 0
MCI<-MCI %>%
  group_by(ID) %>%
  arrange(TESTDATE) %>%
  slice(1L)

# #Randomly selecting one row per cognitively normal individual to use that as their simulated EYO date

# Assuming that there are multiple columns in the original dataset, we group by 'z', 
# sample 1 row from the sequence of rows per group, get the row index (.I), 
# extract the column with the row index ($V1) and use that to subset the rows of 'dt'.

Norm[Norm[ , .I[sample(.N,1)], by = ID]$ID]

Norm<-Norm %>%
  group_by(ID) %>%
  sample_n(1)

CDR<-data.frame(rbind(data.frame(Norm), data.frame(MCI)))
CDR<-merge(CDR, df_hold, by = "ID", all.x = TRUE, all.y = FALSE)
CDR$AgeatCog<-round(as.numeric(CDR$TESTDATE - CDR$Installation.date)/365, 1) + CDR$AgeatInstall
myVars <- c("AgeatInstall", "AgeatCog", "GENDER", "EDUC", "apoe4", "race2", "ADI_NATRANK", "Apos", "Class")
catVars <- c("GENDER", "apoe4", "race2", "Apos", "Class")
CreateTableOne(vars = myVars, data = CDR, factorVars = catVars, strata = c("CDR"))

CDR_share<-CDR[,c("ID", "TESTDATE", "CDR", "AgeatCog")]
colnames(CDR_share)[2]<-"SimulatedEYO"
