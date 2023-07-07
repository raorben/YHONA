# Comparison between previous numbers and revisions -----------------------


saveRDS(COMU,paste0(dir,"COMU_YHONA_2007to2017.rda"))

COMU$egg_hour<-(COMU$Prim_Eggs+COMU$Sec_Eggs)/COMU$sum_hr
COMU$chick_hour<-(COMU$Prim_Chicks+COMU$Sec_Chicks)/COMU$sum_hr
COMU$adult_hour<-(COMU$Prim_Adults)/COMU$sum_hr


COMUold<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/COMU_TopDown_BottomUp/YHONA_COMU_1998_2018.csv",nrows=21)
head(COMUold)
COMUold<-COMUold[,1:14]
str(COMUold)
COMUold$Adultstd<-COMUold$Adult/max(COMUold$Adult,na.rm = TRUE)
COMUold$Eggstd<-COMUold$Egg/max(COMUold$Egg,na.rm = TRUE)
COMUold$Diststd<-COMUold$disturbances/max(COMUold$disturbances,na.rm = TRUE)
colnames(COMU)
colnames(COMUold)

a<-ggplot()+
  geom_point(data=COMU,aes(x=year,y=HatchingSuccess),color="blue")+
  geom_path(data=COMU,aes(x=year,y=HatchingSuccess),color="blue")+
  geom_point(data=COMUold%>%filter(Year>2006),
             aes(x=Year,y=Hatc_Success),color="black",alpha=.5,shape=10)+
  geom_path(data=COMUold%>%filter(Year>2006),
            aes(x=Year,y=Hatc_Success),color="black",alpha=.5)+
  scale_x_continuous(breaks= pretty_breaks())

b<-ggplot()+
  geom_point(data=COMU,aes(x=year,y=Fledgling_Success),color="blue")+
  geom_path(data=COMU,aes(x=year,y=Fledgling_Success),color="blue")+
  geom_point(data=COMUold%>%filter(Year>2006),
             aes(x=Year,y=Repro_Success),color="black",alpha=.5,shape=10)+
  geom_path(data=COMUold%>%filter(Year>2006),
            aes(x=Year,y=Repro_Success),color="black",alpha=.5)+
  scale_x_continuous(breaks= pretty_breaks())

c<-ggplot()+
  geom_point(data=COMU,aes(x=year,y=egg_hour),color="blue")+
  geom_path(data=COMU,aes(x=year,y=egg_hour),color="blue")+
  geom_point(data=COMUold%>%filter(Year>2006),
             aes(x=Year,y=Egg),color="black",alpha=.5,shape=10)+
  geom_path(data=COMUold%>%filter(Year>2006),
            aes(x=Year,y=Egg),color="black",alpha=.5)+
  scale_x_continuous(breaks= pretty_breaks())


d<-ggplot()+
  geom_point(data=COMU,aes(x=year,y=adult_hour),color="blue")+
  geom_path(data=COMU,aes(x=year,y=adult_hour),color="blue")+
  geom_point(data=COMUold%>%filter(Year>2006),
             aes(x=Year,y=Adult),color="black",alpha=.5,shape=10)+
  geom_path(data=COMUold%>%filter(Year>2006),
            aes(x=Year,y=Adult),color="black",alpha=.5)+
  scale_x_continuous(breaks= pretty_breaks())

e<-ggplot()+
  geom_point(data=COMU,aes(x=year,y=chick_hour),color="blue")+
  geom_path(data=COMU,aes(x=year,y=chick_hour),color="blue")+
  geom_point(data=COMUold%>%filter(Year>2006),
             aes(x=Year,y=Chick),color="black",alpha=.5,shape=10)+
  geom_path(data=COMUold%>%filter(Year>2006),
            aes(x=Year,y=Chick),color="black",alpha=.5)+
  scale_x_continuous(breaks= pretty_breaks())

f<-ggplot()+
  geom_point(data=COMU,aes(x=year,y=n_Event),color="blue")+
  geom_path(data=COMU,aes(x=year,y=n_Event),color="blue")+
  geom_point(data=COMUold%>%filter(Year>2006),
             aes(x=Year,y=disturbances),color="black",alpha=.5,shape=10)+
  geom_path(data=COMUold%>%filter(Year>2006),
            aes(x=Year,y=disturbances),color="black",alpha=.5)+
  scale_x_continuous(breaks= pretty_breaks())+
  labs(y="Disturbance #")

g<-ggplot()+
  geom_point(data=COMU,aes(x=year,y=days),color="blue")+
  geom_path(data=COMU,aes(x=year,y=days),color="blue")+
  geom_point(data=COMUold%>%filter(Year>2006),
             aes(x=Year,y=Days),color="black",alpha=.5,shape=10)+
  geom_path(data=COMUold%>%filter(Year>2006),
            aes(x=Year,y=Days),color="black",alpha=.5)+
  scale_x_continuous(breaks= pretty_breaks())+
  labs(y="Days")

h<-ggplot()+
  geom_point(data=COMU,aes(x=year,y=sum_hr),color="blue")+
  geom_path(data=COMU,aes(x=year,y=sum_hr),color="blue")+
  geom_point(data=COMUold%>%filter(Year>2006),
             aes(x=Year,y=Hours),color="black",alpha=.5,shape=10)+
  geom_path(data=COMUold%>%filter(Year>2006),
            aes(x=Year,y=Hours),color="black",alpha=.5)+
  scale_x_continuous(breaks= pretty_breaks())+
  labs(y="Observtion Hours")

i<-ggplot()+
  geom_point(data=COMU,aes(x=year,y=n_plots),color="blue")+
  geom_path(data=COMU,aes(x=year,y=n_plots),color="blue")+
  geom_point(data=COMUold%>%filter(Year>2006),
             aes(x=Year,y=num_plots),color="black",alpha=.5,shape=10)+
  geom_path(data=COMUold%>%filter(Year>2006),
            aes(x=Year,y=num_plots),color="black",alpha=.5)+
  scale_x_continuous(breaks= pretty_breaks())+
  labs(y="Number of Plots")

quartz()
grid.arrange(a,b,c,d,e,f)
quartz.save(paste0(dir,"YHONA_COMU_allyears/ReportValues_RAOrevised.jpg"))
grid.arrange(g,h,i)
quartz.save(paste0(dir,"YHONA_COMU_allyears/ReportValues_RAOrevised_effort.jpg"))


colnames(COMU)
M1<-lm(Fledgling_Success~egg_hour,data=COMU%>%dplyr::filter(year!=2014))
anova(M1)
summary(M1)
plot(resid(M1)~fitted(M1))
data$ReproEgg_resid<-NA
data$ReproEgg_resid<-as.vector(resid(M1))

ggplot()+
  geom_smooth(data=COMU,aes(x=egg_hour,y=Fledgling_Success),method = "lm",color="black")+
  geom_point(data=COMU,aes(x=egg_hour,y=Fledgling_Success))+
  geom_label(data=COMU,aes(x=egg_hour+.1,y=Fledgling_Success+.05,label=year))+
  theme_classic()+
  labs(y="Reproductive Success (chicks fledged / egg)")

modinfo<-bind_rows(modinfo,data.frame(model="Repro_Success~Egg",glance(M1)))

ggplot()+
  geom_path(data=data,aes(x=year,y=ReproEgg_resid),color="black")+
  geom_point(data=data,aes(x=year,y=ReproEgg_resid))+
  theme_classic()+
  scale_x_continuous(breaks= pretty_breaks())+
  labs(y="Reproductive Success (chicks fledged / egg)")

data.table::setnames(COMUold, old=c("Year","Hatc_Success","Repro_Success","num_plots","disturbances",
                                    "Egg","Chick","Adult","Hours","Days"), 
                     new=c("year", "HatchingSuccess","Fledgling_Success","n_plots","n_Event",
                           "egg_hour","chick_hour","adult_hour","sum_hr","days"))
colnames(COMUold)
colnames(COMU)

C<-COMUold%>%filter(year<2003 | year==2018)  
COMU<-bind_rows(COMU,C)
saveRDS(COMU,paste0(dir,"YHONA_COMU_allyears/COMU_YHONA_1998to2018.rda"))
