library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(gridExtra)

if(Sys.info()[["user"]]=="rachaelorben") {
  dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/COMU_TopDown_BottomUp/YHONA_tdbu_Analysis/processeddata"
  anadir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/COMU_TopDown_BottomUp/YHONA_tdbu_Analysis"
}

sum_prod<-readRDS(paste0(anadir,"/YHONA_COMU_ProductivitySummary_2007-2018.rda"))
effort<-readRDS(paste0(dir,"/COMU_YHONA_Effort_2007to2018.rda"))
disturb<-readRDS(paste0(anadir,"/processeddata/YHONA_COMU_Disturbance_2007-17.rda"))


timeframe<-NULL
for (i in 2007:2018){
  a<-sum_prod$minEgg[sum_prod$year==i]
  b<-disturb$minEgg_all[disturb$year==i]
  minEgg<-ymd(min(a,b))
  
  #a<-sum_prod$maxEgg[sum_prod$year==i]
  #b<-disturb$maxEgg_all[disturb$year==i]
  #maxEgg<-ymd(max(a,b))
  
  #a<-sum_prod$minChick[sum_prod$year==i]
  #b<-disturb$minChick_all[disturb$year==i]
  #minChick<-ymd(min(a,b))
  
  #a<-sum_prod$maxChick[sum_prod$year==i]
  #b<-disturb$maxChick_all[disturb$year==i]
  #maxChick<-ymd(max(a,b))
  
  tf<-data.frame(year=i,minEgg)#,maxEgg,minChick,maxChick)
  timeframe<-rbind(timeframe,tf)
}

E<-NULL
for  (i in 2007:2018){
  e<-effort%>%filter(year==i)
  tf=timeframe%>%filter(year==i)
  ee<-e%>%
    filter(Start1>=tf$minEgg)%>%
    filter(Start1<=date(paste0(i,"-08-05")))
  E<-rbind(E,ee)
}
effort<-E  

effsum<-effort%>%
  group_by(year)%>%
  filter(Disturbance=="Y")%>%
  summarise(sum_min=sum(dur,na.rm = TRUE), 
            days=n_distinct(date(Start1)),
            daymin=min(Start1,na.rm = TRUE),
            daymax=max(Start1,na.rm = TRUE))
effsum$sum_hr<-as.numeric(effsum$sum_min)/60
effsum  

summary(sum_prod)

COMU<-left_join(sum_prod%>%
                  dplyr::select(-minEgg,-sum_hr,-days,-n_Event,-egg_hour,
                                -chick_hour,-adult_hour,-dist_perday),disturb,by="year")
COMU<-left_join(COMU,effsum%>%dplyr::select(-daymin,-daymax,-sum_min),by=c("year"="year"))
COMU$sum_hr

COMU$egg_hour_all<-(COMU$Prim_Eggs_all+COMU$Sec_Eggs_all)/COMU$sum_hr
COMU$chick_hour_all<-(COMU$Prim_Chicks_all+COMU$Sec_Chicks_all)/COMU$sum_hr
COMU$adult_hour_all<-(COMU$Prim_Adults_all)/COMU$sum_hr

COMU$egg_hour_eagle<-(COMU$Prim_Eggs_eagle+COMU$Sec_Eggs_eagle)/COMU$sum_hr
COMU$chick_hour_eagle<-(COMU$Prim_Chicks_eagle+COMU$Sec_Chicks_eagle)/COMU$sum_hr
COMU$adult_hour_eagle<-(COMU$Prim_Adults_eagle)/COMU$sum_hr

saveRDS(COMU,paste0(dir,"/COMU_YHONA_1998to2018_prod_effort_dist.rda"))


names(COMU)

COMU$eagleperhour<-COMU$n_Event_eagle/COMU$sum_hr


COMU%>%dplyr::select(year, eagleperhour)%>%arrange(year)

COMU$yearGRP<-NA
COMU$yearGRP[COMU$year>2006]<-"high"
COMU$yearGRP[COMU$year>2010]<-"low"
COMU$yearGRP[COMU$year>2013]<-"HW"
COMU$yearGRP[COMU$year==2018]<-"low"

ggplot()+
  geom_line(data=COMU,aes(x=year,y=eagleperhour))+
  geom_point(data=COMU,aes(x=year,y=eagleperhour,color=yearGRP))+
  xlim(min=2007, max=2018)

COMU%>%group_by(yearGRP)%>%#filter(year!=2016)%>%
  summarise(uEH=mean(eagleperhour,na.rm=TRUE))

ggplot()+
  geom_line(data=COMU,aes(x=year,y=Sec_Eggs_all))+
  geom_point(data=COMU,aes(x=year,y=Sec_Eggs_all,color=yearGRP))+
  xlim(min=2007, max=2018)

COMU%>%group_by(yearGRP)%>%#filter(year!=2016)%>%
  summarise(uEH=mean(Sec_Eggs_all,na.rm=TRUE),
            sdEH=sd(Sec_Eggs_all,na.rm=TRUE))

COMU%>%group_by(yearGRP)%>%filter(year!=2018)%>%
  summarise(uHS=mean(HatchingSuccess,na.rm=TRUE),
            sdHS=sd(HatchingSuccess,na.rm=TRUE))

ggplot()+
  geom_line(data=COMU,aes(x=year,y=HatchingSuccess))+
  geom_point(data=COMU,aes(x=year,y=HatchingSuccess,color=yearGRP))+
  xlim(min=2007, max=2018)
