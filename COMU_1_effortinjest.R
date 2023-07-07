library(readr) #for read_csv that inturprets dates for you
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(gridExtra)

if(Sys.info()[["user"]]=="rachaelorben") {
  dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/YHONA_monitoring/YHONA_COMU_allyears/"
  anadir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/COMU_TopDown_BottomUp/YHONA_tdbu_Analysis"
}

files<-list.files(paste0(dir),pattern = "Effort_",full.names = T,recursive = T)
effort<-NULL
for (i in 1:length(files)){
  print(i)
  Edat<-read.csv(files[i])
  Edat$Start1<-mdy_hm(paste(Edat$Date,Edat$Start),tz="America/Los_Angeles")
  Edat$Stop1<-mdy_hm(paste(Edat$Date,Edat$Stop),tz="America/Los_Angeles")
  
  effort<-bind_rows(effort,Edat)
}
effort$dur<-effort$Stop1-effort$Start1
effort$year<-year(effort$Start1)

write.csv(effort,paste0(anadir,"/processeddata/COMU_YHONA_Effort_2007to",max(effort$year,na.rm=TRUE),".csv"))
saveRDS(effort,paste0(anadir,"/processeddata/COMU_YHONA_Effort_2007to",max(effort$year,na.rm=TRUE),".rda"))


# Calculate Hours ---------------------------------------------------------
effsum<-effort%>%
  group_by(year)%>%
  filter(Disturbance=="Y")%>%
  summarise(sum_min=sum(dur,na.rm = TRUE), 
            days=n_distinct(date(Start1)),
            daymin=min(Start1,na.rm = TRUE),
            daymax=max(Start1,na.rm = TRUE))
effsum$sum_hr<-as.numeric(effsum$sum_min)/60
effsum
#2007 = report
#2008 = report
#2009 ~= report (1 hour less)
#2010 ~= report (1 hour more)
#2011 372 report, 390 summary
#2012 264 report, 284 summary
#2013 = report
#2014 156 report, 151 summary
#2015 110 report, 94 summary
#2016 243 report, 128 summary (add prey taking time - only if disturbance events are looking/recording)
#        cross check with disturbances.
#2017 203 report, 129 summary (add prey taking time)


# plots -------------------------------------------------------------------

a<-ggplot()+
  geom_line(data=effsum,aes(x=year,y=sum_hr))+
  geom_hline(aes(yintercept=mean(effsum$sum_hr)),linetype = "dashed")+
  theme_classic()+
  scale_x_continuous(breaks= pretty_breaks())

b<-ggplot()+
  geom_line(data=effsum,aes(x=year,y=days))+
  geom_hline(aes(yintercept=mean(effsum$days)),linetype = "dashed")+
  theme_classic()+
  scale_x_continuous(breaks= pretty_breaks())

quartz()
grid.arrange(a,b)
quartz.save(paste0(anadir,"/PLOTS/YHONA_COMU_Effort_2007-18.png"))



# Extra stuff -------------------------------------------------------------


#personel
abr<-c("JP","SL","IJ","JD","RK","AM")
name<-c("Jessica Porquez","Stephanie Loredo","Isabel Justiniano",
        "Jane Dolliver","Rachael Kaplan","Ana Medina")

personal17<-data_frame(abr=abr,name=name)
personal17$year<-2017

#personal
abr<-c("JP","SL","DS","AM","JD","SD")
name<-c("Jessica Porquez","Stephanie Loredo","Denisse Silva",
        "Ali Melendez","Jane Dolliver","Sarah Driscoll")

personal16<-data_frame(abr=abr,name=name)
personal16$year<-2016

personnel<-rbind(personal17,personal16)
