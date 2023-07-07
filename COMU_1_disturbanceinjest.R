library(readr) #for read_csv that inturprets dates for you
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(gridExtra)

if(Sys.info()[["user"]]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/GoogleDrive-orbenr@oregonstate.edu/.shortcut-targets-by-id/0B1NPKZM3lphEY0lEa19IcVJKT0E/";
  SOL<-"Seabird_Oceanography_Lab/YHONA_monitoring/YHONA_COMU_allyears";
  anadir<-"Seabird_Oceanography_Lab/COMU_TopDown_BottomUp/YHONA_tdbu_Analysis";
}


files<-list.files(paste0(usrdir,SOL),pattern = "Disturbance_",full.names = T,recursive = T)

disturb<-NULL
for (i in 1:length(files)){
  Ddat<-read.csv(files[i],stringsAsFactors = FALSE,na.strings="?")
  Ddat<-Ddat %>%
    mutate_all(as.character)
  disturb<-bind_rows(disturb,Ddat)
}

disturb$Sec_Pred_Eggs_Taken<-as.numeric(as.character(disturb$Sec_Pred_Eggs_Taken))
disturb$Sec_Pred_Chicks_Taken<-as.numeric(as.character(disturb$Sec_Pred_Chicks_Taken))

disturb$Prim_Pred_Adults_Taken<-as.numeric(as.character(disturb$Prim_Pred_Adults_Taken))
disturb$Prim_Pred_Eggs_Taken<-as.numeric(as.character(disturb$Prim_Pred_Eggs_Taken))
disturb$Prim_Pred_Chicks_Taken<-as.numeric(as.character(disturb$Prim_Pred_Chicks_Taken))

disturb$Date<-mdy(disturb$Date)
disturb$year<-year(disturb$Date)

unique(disturb$Event_Type)
unique(disturb$Primary_Predator)
disturb%>%filter(Primary_Predator=="TUVA")
disturb$Primary_Predator[disturb$Primary_Predator=="BAEA, PEFA"]<-"BAEA"

ggplot()+
  geom_bar(data=disturb%>%
             filter(Event_Type=="Disturbance")%>%
             filter(is.na(Primary_Predator)==FALSE),
           aes(x=Primary_Predator,fill=as.factor(year)),position = "dodge")


# choose analysis timeframe: eggs & chicks --------------------------------
sum_prod<-readRDS(paste0(usrdir,anadir,"/processeddata/YHONA_COMU_ProductivitySummary_2007-2018.rda"))
effort<-readRDS(paste0(usrdir,anadir,"/processeddata/COMU_YHONA_Effort_2007to2017.rda"))

timeframe<-NULL
for (i in 2007:2017){
  a<-sum_prod$minEgg[sum_prod$year==i]
  b<-disturb$minEgg_all[disturb$year==i]
  minEgg<-ymd(min(a,b))
  
  a<-sum_prod$maxEgg[sum_prod$year==i]
  b<-disturb$maxEgg_all[disturb$year==i]
  maxEgg<-ymd(max(a,b))
  
  a<-sum_prod$minChick[sum_prod$year==i]
  b<-disturb$minChick_all[disturb$year==i]
  minChick<-ymd(min(a,b))
  
  a<-sum_prod$maxChick[sum_prod$year==i]
  b<-disturb$maxChick_all[disturb$year==i]
  maxChick<-ymd(max(a,b))
  
  tf<-data.frame(year=i,minEgg,maxEgg,minChick,maxChick)
  timeframe<-rbind(timeframe,tf)
}


D<-NULL
for  (i in 2007:2017){
  d<-disturb%>%filter(year==i)
  tf=timeframe%>%filter(year==i)
  dd<-d%>%
    filter(Date>=tf$minEgg)%>%
    filter(Date<=date(paste0(i,"-08-05")))
  D<-rbind(D,dd)
}
disturb<-D  



# summarize disturbance events w/ and w/o eagles --------------------------

disturb_eagle<-disturb%>%filter(Event_Num>0)%>%
  filter(Event_Type=="Disturbance")%>%
  filter(Primary_Predator=="BAEA")%>%
  group_by(year)%>%
  summarise(n_Event_eagle=n_distinct(Event_Num),
            n_eagle=n(),
            Prim_Adults_eagle=sum(Prim_Pred_Adults_Taken,na.rm=TRUE),
            Prim_Eggs_eagle=sum(Prim_Pred_Eggs_Taken,na.rm=TRUE),
            Prim_Chicks_eagle=sum(Prim_Pred_Chicks_Taken,na.rm=TRUE),
            
            Sec_Eggs_eagle=sum(Sec_Pred_Eggs_Taken,na.rm=TRUE),
            Sec_Chicks_eagle=sum(Sec_Pred_Chicks_Taken,na.rm=TRUE),
            minEgg_eagle=min(Date[Sec_Pred_Eggs_Taken>0],na.rm=TRUE),
            maxEgg_eagle=max(Date[Sec_Pred_Eggs_Taken>0],na.rm=TRUE),
            minChick_eagle=min(Date[Prim_Pred_Chicks_Taken>0],na.rm=TRUE),
            maxChick_eagle=max(Date[Prim_Pred_Chicks_Taken>0],na.rm=TRUE),
            minDate_eagle=min(Date),
            maxDate_eagle=max(Date))

disturb_all<-disturb%>%filter(Event_Num>0)%>%
  filter(Event_Type=="Disturbance")%>%
  group_by(year)%>%
  summarise(n_Event_all=n_distinct(Event_Num),
            n_all=n(),
            Prim_Adults_all=sum(Prim_Pred_Adults_Taken,na.rm=TRUE),
            Prim_Eggs_all=sum(Prim_Pred_Eggs_Taken,na.rm=TRUE),
            Prim_Chicks_all=sum(Prim_Pred_Chicks_Taken,na.rm=TRUE),
            
            Sec_Eggs_all=sum(Sec_Pred_Eggs_Taken,na.rm=TRUE),
            Sec_Chicks_all=sum(Sec_Pred_Chicks_Taken,na.rm=TRUE),
            minEgg_all=min(Date[Sec_Pred_Eggs_Taken>0],na.rm=TRUE),
            maxEgg_all=max(Date[Sec_Pred_Eggs_Taken>0],na.rm=TRUE),
            minChick_all=min(Date[Prim_Pred_Chicks_Taken>0],na.rm=TRUE),
            maxChick_all=max(Date[Prim_Pred_Chicks_Taken>0],na.rm=TRUE))



disturb<-left_join(disturb_all,disturb_eagle,by="year")
saveRDS(disturb, paste0(anadir,"/processeddata/YHONA_COMU_Disturbance_2007-17.rda"))

a<-ggplot()+
  geom_line(data=disturb,aes(x=year,y=Prim_Adults_all),color="blue")+
  geom_line(data=disturb,aes(x=year,y=Prim_Adults_eagle),color="black")+
  theme_classic()+
  scale_x_continuous(breaks= pretty_breaks())
b<-ggplot()+
  geom_line(data=disturb,aes(x=year,y=n_Event_all),color="blue")+
  geom_line(data=disturb,aes(x=year,y=n_Event_eagle),color="black")+
  theme_classic()+
  scale_x_continuous(breaks= pretty_breaks())
c<-ggplot()+
  geom_line(data=disturb,aes(x=year,y=Sec_Eggs_all),color="blue")+
  geom_line(data=disturb,aes(x=year,y=Sec_Eggs_eagle),color="black")+
  theme_classic()+
  scale_x_continuous(breaks= pretty_breaks())

quartz()
grid.arrange(a,b,c)
quartz.save(paste0(usrdir,anadir,"/PLOTS/YHONA_COMU_Disturbance_2007-17_blueAll_blackjustEagles.png"))
