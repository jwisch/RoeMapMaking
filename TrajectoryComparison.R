library(lme4)

df<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/ParticipantListwithDemogs.csv")
traj<-read.csv("C:/Users/julie.wisch/Documents/RoeCollab/TrajRresults.csv")
traj<-traj[,-1]

hold1<-merge(traj, df[,c("ID", "Class", "DeviceID3", "Installation.date", "GENDER",
                         "EDUC", "race2", "AgeatInstall", "apoe4", "ADI_NATRANK", 
                         "Apos")], by.x = "Vehicle_Name", by.y = "DeviceID3", all = FALSE)
hold2<-merge(traj, df[,c("ID", "Class", "DeviceID2", "Installation.date", "GENDER",
                         "EDUC", "race2", "AgeatInstall", "apoe4", "ADI_NATRANK", 
                         "Apos")], by.x = "Vehicle_Name", by.y = "DeviceID2", all = FALSE)
hold3<-merge(traj, df[,c("ID", "Class", "DeviceID", "Installation.date", "GENDER",
                         "EDUC", "race2", "AgeatInstall", "apoe4", "ADI_NATRANK", 
                         "Apos")], by.x = "Vehicle_Name", by.y = "DeviceID", all = FALSE)

df<-data.frame(rbind(hold3, hold2, hold1))
rm(hold1, hold2, hold3, traj)
df$Date<-as.Date(df$Date, format = "%d%b%Y")
df$Installation.date<-as.Date(df$Installation.date, format = "%Y-%m-%d")

df$AgeatDrive<-round(as.numeric(df$Date - df$Installation.date)/365, 1) + df$AgeatInstall


#  Get months
df$Month <- months(df$Date)

#  Get years
df$Year <- format(df$Date,format="%Y")
df<-df[!is.na(df$Vehicle_Name),]
hold<-df[df$Straightness < 0.25,]
hold<-hold[!is.na(hold$Vehicle_Name),]
hold<-hold[!is.na(hold$Apos),]
hold[1:4, 1:3]

df_monthly<-df
#  Aggregate 'X2' on months and year and get mean
df_monthly<-aggregate( Straightness ~ Month + Year + ID, df_monthly , mean )
hold1<-aggregate( Sinuosity ~ Month + Year + ID, df, mean )
hold2<-aggregate( meanTDC ~ Month + Year + ID, df , mean )
hold3<-aggregate( sdTDC ~ Month + Year + ID, df , mean )
hold4<-aggregate( AgeatDrive ~ Month + Year + ID, df, mean )

df_monthly<-merge(df_monthly, hold1, by = c("Month", "Year", "ID"))
df_monthly<-merge(df_monthly, hold2, by = c("Month", "Year", "ID"))
df_monthly<-merge(df_monthly, hold3, by = c("Month", "Year", "ID"))
df_monthly<-merge(df_monthly, hold4, by = c("Month", "Year", "ID"))
rm(hold1, hold2, hold3, hold4)
df_hold<-df[!duplicated(df$ID),]
df_monthly<-merge(df_monthly, df_hold[,c("ID", "Class", "GENDER", "EDUC", "apoe4", "ADI_NATRANK", "Apos")], by = "ID", all = FALSE)

df_monthly$PlottingDate<-paste(df_monthly$Month, "-01-", df_monthly$Year, sep = "")
df_monthly$PlottingDate<-as.Date(df_monthly$PlottingDate, format = "%b-%d-%Y")
df_monthly$Apos<-as.factor(df_monthly$Apos)
levels(df_monthly$Apos)<-c("Amyloid-", "Amyloid+")

library(ggpubr)
p1<-ggboxplot(df_monthly, x = "Class", y = "Straightness",
          color = "Class", add = "jitter") + scale_color_manual(values = c("#5B6C88",  "#A8D0CF")) +
          stat_compare_means()+ theme(legend.title = element_blank()) 

p2<-ggboxplot(df_monthly[!is.na(df_monthly$Apos),], x = "Apos", y = "Straightness",
          color = "Apos", add = "jitter") + scale_color_manual(values = c("#5B6C88",  "#A8D0CF")) +
  stat_compare_means()+ theme(legend.title = element_blank()) 

grid.arrange(p1, p2, nrow = 1)


p1<-ggboxplot(df_monthly, x = "Class", y = "Sinuosity",
              color = "Class", add = "jitter") + scale_color_manual(values = c("#5B6C88",  "#A8D0CF")) +
  stat_compare_means()+ theme(legend.title = element_blank()) 

p2<-ggboxplot(df_monthly[!is.na(df_monthly$Apos),], x = "Apos", y = "Sinuosity",
              color = "Apos", add = "jitter") + scale_color_manual(values = c("#5B6C88",  "#A8D0CF")) +
  stat_compare_means()+ theme(legend.title = element_blank()) 

grid.arrange(p1, p2, nrow = 1)

# Directional change is measured as change in direction over time (Kitamura & Imafuku, 2015), 
#  incorporates the speed of change, indicating how frequently and how fast an animal changes its direction of movement. 
# The mean and standard deviation of directional change have been used to quantify nonlinearity and irregularity in 
# the trajectories of butterflies (Kitamura & Imafuku, 2015). 

p1<-ggboxplot(df_monthly, x = "Class", y = "meanTDC",
              color = "Class", add = "jitter") + scale_color_manual(values = c("#5B6C88",  "#A8D0CF")) +
  stat_compare_means()+ theme(legend.title = element_blank()) 

p2<-ggboxplot(df_monthly[!is.na(df_monthly$Apos),], x = "Apos", y = "meanTDC",
              color = "Apos", add = "jitter") + scale_color_manual(values = c("#5B6C88",  "#A8D0CF")) +
  stat_compare_means()+ theme(legend.title = element_blank()) 

grid.arrange(p1, p2, nrow = 1)

p3<-ggboxplot(df_monthly, x = "Class", y = "sdTDC",
              color = "Class", add = "jitter") + scale_color_manual(values = c("#5B6C88",  "#A8D0CF")) +
  stat_compare_means()+ theme(legend.title = element_blank()) 

p4<-ggboxplot(df_monthly[!is.na(df_monthly$Apos),], x = "Apos", y = "sdTDC",
              color = "Apos", add = "jitter") + scale_color_manual(values = c("#5B6C88",  "#A8D0CF")) +
  stat_compare_means()+ theme(legend.title = element_blank()) 

grid.arrange(p1, p2, p3, p4, nrow = 2)
rm(p1, p2, p3, p4)



coef.fcn = function(DATA) {
  coeffs = coef(lm(Straightness ~ Date, data=DATA))
  return(data.frame(Intercept=coeffs[1], Slope=coeffs[2]))
}

#Straightness rate of change over time
lm_coefs = df %>% 
  group_by(ID) %>%
  do(coef.fcn(.))


lm_coefs<-merge(lm_coefs, df_hold, by = "ID", all = FALSE)

model<-lm(Slope ~ Class + GENDER + EDUC+ AgeatInstall + apoe4*Apos + ADI_NATRANK, data = lm_coefs)
summary(model)


ggplot(lm_coefs, aes(x = ADI_NATRANK, y = Slope, colour = Class)) + geom_point() + 
  geom_smooth(aes(group = 1), method = "lm", colour = "grey37") + 
  xlab("Area Deprivation Index (ADI)\nLower Score indicates higher SES") + ylab("Individual Straightness Rate of Change") + 
  theme(legend.position = "bottom")
df$Intercept = rep(1, length(df$ID))
source("C:/Users/julie.wisch/Documents/mixTVEM.R")
df<-df[complete.cases(df$AgeatDrive),]
df<-df[complete.cases(df$Straightness),]
df<-df[complete.cases(df$Apos),]
df$ID<-as.factor(df$ID)

test<-df_monthly
df_monthly$Intercept<-rep(1, length(df_monthly$ID))
test<-df_monthly[,c("Straightness", "ID", "Intercept", "PlottingDate")]

df$fakebaseline<-df$Date
df$fakebaseline[df$Installation.date < as.Date( "2018-07-27")] <- as.Date( "2018-07-27")
df$TimeElapsed<-as.numeric(df$Date - df$fakebaseline)/365
df$Apos<-as.factor(df$Apos)

ggplot(test, aes(x = TimeElapsed, y = Straightness, group = ID, colour = Apos, shape = Apos)) +
  geom_line(alpha = 0.3) + geom_point(alpha = 0.6) +
  scale_color_manual("", values = c( "#FFB000", "#1437AD"))+ scale_shape_manual("", values = c(1, 17))+
  geom_smooth(aes(group = Apos))+
  xlab("Years from Chip Install")+ylab("Straightness")

library(mgcv)
test$apoe4<-as.factor(test$apoe4)
mod <- gam(Straightness ~ Class + s(TimeElapsed) + 
                 Apos + AgeatInstall + apoe4+ s(ID,bs="re"),
               data = df, method = "REML")
summary(mod)
gam.check(mod)


df$SuperCurvy<-ifelse(df$Straightness < mean(df$Straightness) - 2*sd(df$Straightness), 1, 0)
df$Trip<-1
df_monthly<-aggregate( SuperCurvy ~ Month + Year + ID, df , sum ) #count of super curvy drives/month by ID
hold<-aggregate(Trip ~ Month + Year + ID, df, sum)
df_monthly<-merge(df_monthly, hold, by = c("Month", "Year", "ID"))
rm(hold)
df_monthly$Proportion <-df_monthly$SuperCurvy/df_monthly$Trip
df_monthly<-merge(df_monthly, df_hold[,c("ID", "Class", "GENDER", "EDUC", "AgeatInstall", "apoe4", "ADI_NATRANK", "Apos")],
                  by = "ID", all.x = TRUE, all.y = FALSE)
m2 <- glmer(Proportion ~ Apos + AgeatInstall + Class + GENDER + EDUC + (1+Apos|ID), family=poisson, data=df_monthly)



# estimate the model and store results in m
m <- glmer(SuperCurvy ~ Apos + AgeatInstall + Class + GENDER + EDUC +
             (1 | ID), data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
# print the mod results without correlations among fixed effects
print(m, corr = FALSE)

se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 * se))
exp(tab)



test<-df[1:7000,]
model1<-TVEMMixNormal(dep = test$Straightness,
                      id = test$ID,
                      numInteriorKnots = 6,
                      numClasses = 2,
                      numStarts = 1,
                      tcov = test$Intercept,
                      time = test$TimeElapsed)

library(lqmm)
fit.lqmm <- lqmm(fixed = Straightness ~ AgeatDrive, random = ~ AgeatDrive, group = ID,
                 tau = c(0.1, 0.25, 0.5, 0.75, 0.9), control = lqmmControl(LP_tol_ll = 0.001, LP_max_iter = 1000), nK = 7, type = "normal", data = df_monthly)
summary(fit.lqmm)

fit.boot <- boot(fit.lqmm, R = 100, seed = 52, startQR = FALSE)
B <- extractBoot(fit.boot, "fixed")[, "AgeatDrive", c(1, 3)]
quantile(apply(B, 1, diff), probs = c(.025, 0.975))
hold<-predict(fit.lqmm, level = 0)
hold<-data.frame(hold, df_monthly$AgeatDrive)
ggplot(hold, aes(x = df_monthly.AgeatDrive)) + geom_smooth(aes(y = X0.10), method = "lm") +
  geom_smooth(aes(y = X0.25), method = "lm") +
  geom_smooth(aes(y = X0.50), method = "lm") +
  geom_smooth(aes(y = X0.75), method = "lm") +
  geom_smooth(aes(y = X0.90), method = "lm") +
  xlab("Age") + ylab("Projected Straightness of Route") + ggtitle("Quantile Regression\nBased on Monthly Driving Averages")


library(qrNLMM)
data(Soybean)
attach(Soybean)
#################################
#A full model (no covariate)
y = weight #response
x = Time #time
#Expression for the three parameter logistic curve
exprNL = expression((fixed[1]+random[1])/(1 + exp(((fixed[2]+random[2])- x)/(fixed[3]+random[3]))))
#Initial values for fixed effects
initial = c(max(y),0.6*max(y),0.73*max(y))
#A median regression (by default)
median_reg = QRNLMM(y,x,Plot,initial,exprNL)
#Assing the fit
fxd = median_reg$res$beta
nlmodel = median_reg$res$nlmodel
seqc = seq(min(x),max(x),length.out = 500)
group.plot(x = Time,y = weight,groups = Plot,type="l",
           main="Soybean profiles",xlab="time (days)",
           ylab="mean leaf weight (gr)",col="gray")
lines(seqc,nlmodel(x = seqc,fixed = fxd,random = rep(0,3)),
      lwd=2,col="blue")
#########################################
#A model for compairing the two genotypes
y = weight #response
x = Time #time
covar = c(Variety)-1 #factor genotype (0=Forrest, 1=Plan Introduction)
#Expression for the three parameter logistic curve with a covariate
exprNL = expression((fixed[1]+(fixed[4]*covar[1])+random[1])/
                      (1 + exp(((fixed[2]+random[2])- x)/(fixed[3]+random[3]))))
#Initial values for fixed effects
initial = c(max(y),0.6*max(y),0.73*max(y),3)
# A quantile regression for the three quartiles
box_reg = QRNLMM(y,x,Plot,initial,exprNL,covar,p=c(0.25,0.50,0.75))
#Assing the fit for the median (second quartile)
fxd = box_reg[[2]]$res$beta
nlmodel = box_reg[[2]]$res$nlmodel
seqc = seq(min(x),max(x),length.out = 500)
group.plot(x = Time[Variety=="P"],y = weight[Variety=="P"],
           groups = Plot[Variety=="P"],type="l",col="light blue",
           main="Soybean profiles by genotype",xlab="time (days)",
           ylab="mean leaf weight (gr)")
group.lines(x = Time[Variety=="F"],y = weight[Variety=="F"],
            groups = Plot[Variety=="F"],col="gray")
lines(seqc,nlmodel(x = seqc,fixed = fxd,random = rep(0,3),covar=1),
      lwd=2,col="blue")
lines(seqc,nlmodel(x = seqc,fixed = fxd,random = rep(0,3),covar=0),
      lwd=2,col="black")
## End(Not run)