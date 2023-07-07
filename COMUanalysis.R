library(readr) #for read_csv that inturprets dates for you
library(lubridate)
library(raster)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(ggplot2)
library(broom)
##library(openxlsx) 
##convertToDate(effor17$Date)

if(Sys.info()[["user"]]=="rachaelorben") {
  dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/COMU_TopDown_BottomUp/"
}
indicies<-readRDS(paste0(dir,"YHONA_tdbu_Analysis/processeddata/Indicies.rda"))
COMU<-readRDS(paste0(dir,"YHONA_tdbu_Analysis/processeddata/COMU_YHONA_1998to2018_prod_effort_dist.rda"))
COMU<-arrange(COMU, year)
head(COMU)
COMU<-rbind(COMU[1:12,],COMU[14:18,])

#COMU$Adultstd<-COMU$Adult/max(COMU$Adult,na.rm = TRUE)
#COMU$Eggstd<-COMU$Egg/max(COMU$Egg,na.rm = TRUE)
#COMU$Diststd<-COMU$disturbances/max(COMU$disturbances,na.rm = TRUE)



I<-indicies%>%filter(year>1997)%>%
  group_by(year,index,season)%>%
  summarise(Anomaly=mean(value))

#COMU<-left_join(COMU,Indicies,by=c("Year"))

modinfo<-NULL

# Repro_Success~Hatc_Success -------------------------------------------------------
colnames(COMU)
M0<-lm(Fledgling_Success~HatchingSuccess,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=HatchingSuccess,y=Fledgling_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=HatchingSuccess,y=Fledgling_Success))+
  geom_label(data=COMU,aes(x=HatchingSuccess+.05,y=Fledgling_Success+.05,label=year))+
  theme_classic()
modinfo<-data.frame(model="Repro_Success~Hatc_Success",glance(M0))

# Repro_Success~Egg -------------------------------------------------------
M1<-lm(Fledgling_Success~egg_hour,data=COMU)
anova(M1)
summary(M1)
plot(resid(M1)~fitted(M1))
COMU$ReproEgg_resid<-NA
resid(M1)
COMU$ReproEgg_resid[6:17]<-as.vector(resid(M1))

ggplot()+
  geom_smooth(data=COMU,aes(x=egg_hour,y=Fledgling_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=egg_hour,y=Fledgling_Success))+
  geom_label(data=COMU,aes(x=egg_hour+.1,y=Fledgling_Success+.05,label=year))+
  theme_classic()

modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~Egg",glance(M1)))

# Repro_Success~Adult -----------------------------------------------------
M2<-lm(Repro_Success~Adult,data=COMU)
anova(M2)
summary(M2)
plot(resid(M2)~fitted(M2))
COMU$ReproAdult_resid<-NA
COMU$ReproAdult_resid[10:20]<-as.vector(resid(M2))

ggplot()+
  geom_smooth(data=COMU,aes(x=Adult,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=Adult,y=Repro_Success))+
  geom_label(data=COMU,aes(x=Adult+.01,y=Repro_Success+.05,label=Year))+
  theme_classic()

modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~Adult",glance(M2)))

# Repro_Success~Chick -----------------------------------------------
M3<-lm(Repro_Success~Chick,data=COMU%>%filter(Chick>0))
anova(M3)
summary(M3)
plot(resid(M3)~fitted(M3))

ggplot()+
  geom_smooth(data=COMU%>%filter(Chick>0),aes(x=Chick,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU%>%filter(Chick>0),aes(x=Chick,y=Repro_Success))+
  geom_label(data=COMU%>%filter(Chick>0),aes(x=Chick+.01,y=Repro_Success+.05,label=Year))+
  theme_classic()

modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~Chick",glance(M3)))

# Repro_Success~disturbances -----------------------------------------------
M4<-lm(Repro_Success~disturbances,data=COMU)
anova(M4)
summary(M4)
plot(resid(M4)~fitted(M4))
COMU$ReproDist_resid<-NA
COMU$ReproDist_resid[10:20]<-as.vector(resid(M4))

ggplot()+
  geom_smooth(data=COMU,aes(x=disturbances,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=disturbances,y=Repro_Success))+
  geom_label(data=COMU,aes(x=disturbances+.01,y=Repro_Success+.05,label=Year))+
  theme_classic()

modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~disturbances",glance(M4)))

# Hatc_Success~Egg -------------------------------------------------------
M1a<-lm(Hatc_Success~Egg,data=COMU)
anova(M1a)
summary(M1a)
plot(resid(M1a)~fitted(M1a))
ggplot()+
  geom_smooth(data=COMU,aes(x=Egg,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=Egg,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=Egg+.1,y=Hatc_Success+.05,label=Year))+
  theme_classic()

modinfo<-bind_rows(modinfo,data.frame(model="Hatch_Success~Egg",glance(M1a)))

# Hatc_Success~Adult -----------------------------------------------------
M2a<-lm(Hatc_Success~Adult,data=COMU)
anova(M2a)
summary(M2a)
plot(resid(M2a)~fitted(M2a))
ggplot()+
  geom_smooth(data=COMU,aes(x=Adult,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=Adult,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=Adult+.01,y=Hatc_Success+.05,label=Year))+
  theme_classic()

modinfo<-bind_rows(modinfo,data.frame(model="Hatch_Success~Adult",glance(M2a)))

# Hatc_Success~Chick -----------------------------------------------
M3a<-lm(Hatc_Success~Chick,data=COMU%>%filter(Chick>0))
anova(M3a)
summary(M3a)
plot(resid(M3a)~fitted(M3a))
ggplot()+
  geom_smooth(data=COMU%>%filter(Chick>0),aes(x=Chick,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU%>%filter(Chick>0),aes(x=Chick,y=Hatc_Success))+
  geom_label(data=COMU%>%filter(Chick>0),aes(x=Chick+.01,y=Hatc_Success+.05,label=Year))+
  theme_classic()

modinfo<-bind_rows(modinfo,data.frame(model="Hatch_Success~Chick",glance(M3a)))

# Repro_Success~disturbances -----------------------------------------------
M4a<-lm(Hatc_Success~disturbances,data=COMU)
anova(M4a)
summary(M4a)
plot(resid(M4a)~fitted(M4a))
ggplot()+
  geom_smooth(data=COMU,aes(x=disturbances,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=disturbances,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=disturbances+.01,y=Hatc_Success+.05,label=Year))+
  theme_classic()

modinfo<-bind_rows(modinfo,data.frame(model="Hatch_Success~disturbances",glance(M4)))


# Indicies ----------------------------------------------------------------

colnames(Indicies)
M0<-lm(UWave~UWwin,data=Indicies)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=Indicies,aes(x=UWave,y=UWwin),method = "lm",color="black")+
  geom_point(data=Indicies,aes(x=UWave,y=UWwin))+
  #geom_label(data=Indicies,aes(x=UWave+.1,y=UWwin+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="UWave~UWwin",glance(M0)))

colnames(Indicies)
M0<-lm(MEI_APRMAY~MEI_MAYJUN,data=Indicies)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=Indicies,aes(x=MEI_APRMAY,y=MEI_MAYJUN),method = "lm",color="black")+
  geom_point(data=Indicies,aes(x=MEI_APRMAY,y=MEI_MAYJUN))+
  #geom_label(data=Indicies,aes(x=MEI_APRMAY+.1,y=MEI_MAYJUN+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="MEI_APRMAY~MEI_MAYJUN",glance(M0)))

M0<-lm(NOI_Ave~NOI_summer,data=Indicies)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=Indicies,aes(x=NOI_Ave,y=NOI_summer),method = "lm",color="black")+
  geom_point(data=Indicies,aes(x=NOI_Ave,y=NOI_summer))+
  #geom_label(data=Indicies,aes(x=NOI_Ave+.1,y=NOI_summer+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="NOI_Ave~NOI_summer",glance(M0)))

M0<-lm(PDO_ave~PDO_sum,data=Indicies)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=Indicies,aes(x=PDO_ave,y=PDO_sum),method = "lm",color="black")+
  geom_point(data=Indicies,aes(x=PDO_ave,y=PDO_sum))+
  #geom_label(data=Indicies,aes(x=PDO_ave+.1,y=PDO_sum+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="PDO_ave~PDO_sum",glance(M0)))


# COMU vs. Indicies -------------------------------------------------------


# PDO ---------------------------------------------------------------------
M0<-lm(Fledgling_Success~PDO_sum,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=PDO_sum,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=PDO_sum,y=Repro_Success))+
  geom_label(data=COMU,aes(x=PDO_sum+.1,y=Repro_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~PDO_sum",glance(M0)))

M0<-lm(Repro_Success~PDO_win,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=PDO_win,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=PDO_win,y=Repro_Success))+
  geom_label(data=COMU,aes(x=PDO_win+.1,y=Repro_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~PDO_win",glance(M0)))

M0<-lm(Repro_Success~PDO_ave,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=PDO_ave,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=PDO_ave,y=Repro_Success))+
  geom_label(data=COMU,aes(x=PDO_ave+.1,y=Repro_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~PDO_ave",glance(M0)))

M0<-lm(Hatc_Success~PDO_sum,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=PDO_sum,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=PDO_sum,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=PDO_sum+.1,y=Hatc_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Hatc_Success~PDO_sum",glance(M0)))

M0<-lm(Hatc_Success~PDO_ave,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=PDO_ave,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=PDO_ave,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=PDO_ave+.1,y=Hatc_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Hatc_Success~PDO_ave",glance(M0)))


# UW ----------------------------------------------------------------------

M0<-lm(Repro_Success~UWave,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=UWave,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=UWave,y=Repro_Success))+
  geom_label(data=COMU,aes(x=UWave+.1,y=Repro_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~UWave",glance(M0)))

M0<-lm(Repro_Success~UWwin,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=UWwin,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=UWwin,y=Repro_Success))+
  geom_label(data=COMU,aes(x=UWwin+.1,y=Repro_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~UWwin",glance(M0)))

M0<-lm(Hatc_Success~UWave,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=UWave,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=UWave,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=UWave+.1,y=Hatc_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Hatc_Success~UWave",glance(M0)))

M0<-lm(Hatc_Success~UWwin,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=UWwin,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=UWwin,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=UWwin+.1,y=Hatc_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Hatc_Success~UWwin",glance(M0)))



# MEI ----------------------------------------------------------------------
colnames(COMU)
M0<-lm(Repro_Success~MEI_APRMAY,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=MEI_APRMAY,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=MEI_APRMAY,y=Repro_Success))+
  geom_label(data=COMU,aes(x=MEI_APRMAY+.1,y=Repro_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~MEI_APRMAY",glance(M0)))

M0<-lm(Repro_Success~MEI_MAYJUN,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=MEI_MAYJUN,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=MEI_MAYJUN,y=Repro_Success))+
  geom_label(data=COMU,aes(x=MEI_MAYJUN+.1,y=Repro_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~MEI_MAYJUN",glance(M0)))

M0<-lm(Hatc_Success~MEI_APRMAY,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=MEI_APRMAY,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=MEI_APRMAY,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=MEI_APRMAY+.1,y=Hatc_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Hatc_Success~MEI_APRMAY",glance(M0)))

M0<-lm(Hatc_Success~MEI_MAYJUN,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=MEI_MAYJUN,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=MEI_MAYJUN,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=MEI_MAYJUN+.1,y=Hatc_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Hatc_Success~MEI_MAYJUN",glance(M0)))



# NOI ----------------------------------------------------------------------
colnames(COMU)
M0<-lm(Repro_Success~NOI_Ave,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=NOI_Ave,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=NOI_Ave,y=Repro_Success))+
  geom_label(data=COMU,aes(x=NOI_Ave+.1,y=Repro_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~NOI_Ave",glance(M0)))

M0<-lm(Repro_Success~NOI_summer,data=COMU%>%filter(Year>2010))
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=NOI_summer,y=Repro_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=NOI_summer,y=Repro_Success))+
  geom_label(data=COMU,aes(x=NOI_summer+.1,y=Repro_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~NOI_summer",glance(M0)))

M0<-lm(Hatc_Success~NOI_Ave,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=NOI_Ave,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=NOI_Ave,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=NOI_Ave+.1,y=Hatc_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Hatc_Success~NOI_Ave",glance(M0)))

M0<-lm(Hatc_Success~NOI_summer,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=NOI_summer,y=Hatc_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=NOI_summer,y=Hatc_Success))+
  geom_label(data=COMU,aes(x=NOI_summer+.1,y=Hatc_Success+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="Hatc_Success~NOI_summer",glance(M0)))



# Rediduals from Repro vs. Egg --------------------------------------------

colnames(I)
a<-I%>%dplyr::filter(index=="NOI" & season=="summer")%>%
  mutate(NOIsummer=Anomaly)

COMU<-left_join(COMU,a%>%dplyr::select(year, NOIsummer))

M0<-lm(ReproEgg_resid~NOIsummer,data=COMU%>%filter(year>2006))
anova(M0)
summary(M0)
max(resid(M0)) #2008
min(resid(M0)) #2017
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU%>%filter(year>2006),
              aes(x=NOIsummer,y=ReproEgg_resid),method = "lm",color="black")+
  geom_point(data=COMU%>%filter(year>2006),
             aes(x=NOIsummer,y=ReproEgg_resid))+
  geom_label(data=COMU%>%filter(year>2006),
             aes(x=NOIsummer+.1,y=ReproEgg_resid+.05,label=year))+
  theme_classic()

ggplot()+
  geom_line(data=COMU%>%filter(year>2005),aes(x=year,y=NOIsummer))+
  geom_point(data=COMU%>%filter(year>2005),aes(x=year,y=NOIsummer))+
  theme_classic()
ggplot()+
  geom_line(data=COMU%>%filter(year>2005),aes(x=year,y=ReproEgg_resid))+
  geom_point(data=COMU%>%filter(year>2005),aes(x=year,y=ReproEgg_resid))+
  theme_classic()

modinfo<-bind_rows(modinfo,data.frame(model="ReproEgg_resid~NOI_summer",glance(M0)))

M0<-lm(ReproAdult_resid~NOI_JAS,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU%>%filter(Year<2016)%>%filter(Year>2006),aes(x=NOI_summer,y=ReproAdult_resid),method = "lm",color="black")+
  geom_point(data=COMU%>%filter(Year<2016),aes(x=NOI_summer,y=ReproAdult_resid))+
  #geom_label(data=COMU%>%filter(Year>2006),aes(x=NOI_summer+.1,y=ReproEgg_resid+.05,label=Year))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="ReproAdult_resid~NOI_summer",glance(M0)))

M0<-lm(ReproDist_resid~NOI_JAS,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU%>%filter(Year<2016)%>%filter(Year>2006),aes(x=NOI_summer,y=ReproDist_resid),method = "lm",color="black")+
  geom_point(data=COMU%>%filter(Year<2016),aes(x=NOI_summer,y=ReproDist_resid))+
  theme_classic()
modinfo<-bind_rows(modinfo,data.frame(model="ReproAdult_resid~NOI_summer",glance(M0)))

ggplot()+
  geom_path(data=COMU%>%filter(Year>2005),aes(x=Year,y=NOI_summer))+
  #geom_path(data=COMU%>%filter(Year>2005),
  #        aes(x=Year,y=ReproEgg_resid),color="red")+
  #geom_path(data=COMU%>%filter(Year>2005),aes(x=Year,y=NOI_summer/mean(NOI_summer,na.rm=TRUE)))+
  theme_classic()

modinfo<-bind_rows(modinfo,data.frame(model="ReproEgg_resid~NOI_summer",glance(M0)))


colnames(COMU)
M0<-lm(ReproEgg_resid~NOI_MJJ,data=COMU)
anova(M0)
summary(M0)
plot(resid(M0)~fitted(M0))
ggplot()+
  geom_smooth(data=COMU,aes(x=NOI_summer,y=ReproEgg_resid),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=NOI_summer,y=ReproEgg_resid))+
  #geom_label(data=COMU,aes(x=NOI_summer+.1,y=ReproEgg_resid+.05,label=Year))+
  theme_classic()






ggplot()+
  geom_path(data=COMU,aes(x=Year,y=Repro_Success))+
  geom_path(data=COMU,aes(x=Year,y=Eggstd),color="blue")+
  theme_classic()

ggplot()+
  geom_path(data=COMU%>%filter(Year>2007),aes(x=Year,y=Repro_Success))+
  geom_path(data=COMU%>%filter(Year>2007),aes(x=Year,y=Adultstd),color="red")+
  geom_path(data=COMU%>%filter(Year>2007),aes(x=Year,y=Eggstd),color="blue")+
  geom_path(data=COMU%>%filter(Year>2007),aes(x=Year,y=Diststd),color="orange")+
  theme_classic()

ggplot()+
  geom_point(data=COMU,aes(x=Chick,y=Repro_Success))


