library(readr) #for read_csv that interprets dates for you
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(gridExtra)

if(Sys.info()[["user"]]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/GoogleDrive-orbenr@oregonstate.edu/.shortcut-targets-by-id/0B1NPKZM3lphEY0lEa19IcVJKT0E/";
  SOL<-"Seabird_Oceanography_Lab/YHONA_monitoring/ActivityWatch";
  anadir<-"Seabird_Oceanography_Lab/COMU_TopDown_BottomUp/YHONA_tdbu_Analysis";
}


act<-read.csv(paste0(usrdir,SOL,"/YHONA_ActivityWatchData_2010-16 2018 2019 2021.csv"))
head(act)

act$datetime<-ymd_hm(paste0(act$year,"-",act$month,"-",act$day," ",act$time), tz = "US/Pacific")

act<-act%>%group_by(year, date,Sub.Colony,plot,pair)%>%
  arrange(datetime,.by_group = TRUE)

unique(act$act)

unique(act$year)
names(act)



# Duration of any Breaks in Observations (fog) ----------------------------
interupts<-act%>%filter(act=="END" | act=="START")%>%select(-ID,-time,-notes,-linecheck)
unique(interupts$date)
interupts$ID<-paste(interupts$date,interupts$year,interupts$month,interupts$day, interupts$Sub.Colony,interupts$plot, interupts$pair)

interupts_S<-interupts %>%arrange(datetime)%>%
  filter(act=="START")%>%
  group_by(ID, grp = with(rle(act), rep(seq_along(lengths), lengths))) %>%
  mutate(Counter = seq_along(grp)) %>%
  ungroup() %>%
  select(-grp)

interupts_E<-interupts%>%arrange(datetime)%>%
  filter(act=="END")%>%
  group_by(ID, grp = with(rle(act), rep(seq_along(lengths), lengths))) %>%
  mutate(Counter = seq_along(grp)) %>%
  ungroup() %>%
  select(-grp)

interupts_count<-rbind(interupts_S,interupts_E)

info_interupts<-interupts_count%>%group_by(year,date,Sub.Colony, pair,Counter)%>%
  pivot_wider(names_from = act, values_from = datetime)
info_interupts$dur_break<-info_interupts$START-info_interupts$END

info_interupts%>%filter(dur_break<0) #checks for miss-matched END-STARTS
info_interupts%>%filter(is.na(dur_break)==TRUE) #check for missing END-STARTS

info_interuptsT<-info_interupts%>%group_by(date, year, month, day, Sub.Colony, plot, pair)%>%
  summarise(breakTotal=sum(dur_break))

# Duration of total observation period ------------------------------------

obs<-act%>%filter(act=="END_O" | act=="START_O")
unique(interupts$date)
info<-obs%>%group_by(year,date,Sub.Colony, pair)%>%select(-ID,-time, -notes,-linecheck)%>%
  pivot_wider(names_from = act, values_from = datetime)
info$act<-as.character(NA)

earlyends<-act%>%filter(act=="FELL" | act=="FLEDGE")%>%select(-ID,-time, -notes,-linecheck)
earlyends$END_O<-earlyends$datetime
earlyends<-earlyends%>%select(-datetime)

info_ends<-info %>% 
  dplyr::rows_update(earlyends, by = c("date","year","month","day","Sub.Colony","plot","pair"))

info_ends%>%filter(is.na(act)==FALSE)
info%>%filter(is.na(END_O)==TRUE | is.na(START_O)==TRUE )
info_ends%>%filter(is.na(END_O)==TRUE | is.na(START_O)==TRUE) #looks for rows missing times
info_ends$dur<-info_ends$END_O-info_ends$START_O #calculates duration from start to end


# Duration Observing (total - breaks) -------------------------------------
head(info_interuptsT)
head(info_ends)

info_DurBreak<-left_join(info_ends,info_interuptsT,by=c("date","year","month","day","Sub.Colony","plot","pair"))

info_DurBreak$breakTotal[is.na(info_DurBreak$breakTotal)==TRUE]<-0
info_DurBreak$DurObs<-info_DurBreak$dur-info_DurBreak$breakTotal

(info_DurBreak%>%filter(is.na(act)==FALSE)) #checks to see if breaks happened after chicks died/fledged
#7/22/14 plot 4, pair 12 is OK
info_DurBreak



# Chick Feeding Events ----------------------------------------------------
head(act)
unique(act$act)
act$act[act$act=='']<-NA

actC<-act%>%group_by(date,year,month,day,Sub.Colony,plot,pair)%>%
  filter(act!="END_O" & act!="START_O" & act!="END" & act!="START" )%>%
  select(-datetime,-notes,-linecheck)%>%
  count(act)%>%
  pivot_wider(names_from = act, values_from = n)

actC_dur<-left_join(actC, info_DurBreak%>%
            select(-START_O,-END_O,-act,-dur,-breakTotal),
          by=c("date","year","month","day","Sub.Colony","plot","pair"))

actC_dur$ChFd[is.na(actC_dur$ChFd)==TRUE]<-0

actC_dur$DurObs_hr<-as.numeric(actC_dur$DurObs)/60
actC_dur$ChFd_hr<-actC_dur$ChFd/actC_dur$DurObs_hr

#sample size of nests
nestsum<-act%>%group_by(year,date,Sub.Colony)%>%
  summarise(n=n_distinct(pair))

pairsum<-act%>%group_by(year,date,Sub.Colony, pair)%>%
  filter(act=="ChFd")%>%
  summarise(n=n())

actC_durS<-actC_dur%>%select(-FELL,-KILLED,-MF,-FST,-BL0,-BL2,-'NO feed',-FLEDGE)

write.csv(actC_durS,paste0(usrdir,SOL,"/ActivityDataSUM.csv"))

# Quik Viz ----------------------------------------------------------------
names(actC_dur)

ggplot()+
  geom_boxplot(data=actC_dur,
               aes(x=year,y=ChFd_hr, group=as.factor(year), fill=as.factor(year)))

ggplot()+
  geom_boxplot(data=actC_dur,
               aes(x=year,y=DurObs, group=as.factor(year), fill=as.factor(year)))


ggplot()+
  geom_point(data=actC_dur,
               aes(x=DurObs,y=ChFd_hr, group=as.factor(year), fill=as.factor(year)))


ggplot()+
  geom_boxplot(data=actC_dur%>%filter(DurObs>500),
               aes(x=year,y=ChFd_hr, group=as.factor(year), fill=as.factor(year)))

ggplot()+
  geom_boxplot(data=actC_dur%>%filter(DurObs>500),
               aes(x=year,y=ChFd_hr, group=as.factor(date)))+facet_wrap(~year, scales = "free_x")

actC_dur%>%group_by(year)%>%
  summarise(uDelivier=mean(ChFd_hr, na.rm=TRUE),
              sdDeli=sd(ChFd_hr, na.rm=TRUE),
            n=n(),
            nDays=n_distinct(date))
 



               