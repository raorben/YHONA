library(readr) #for read_csv that inturprets dates for you
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
##library(openxlsx) 
##convertToDate(effor17$Date)

if(Sys.info()[["user"]]=="rachaelorben") {
  dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/YHONA_monitoring/YHONA_COMU_allyears"
  anadir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/COMU_TopDown_BottomUp/YHONA_tdbu_Analysis/"
}

#2007: CR05 & CR06 combined in 1 plot
#2008: CR05 & CR06 combined in 1 plot
#2009: CR02 & CR03 combined in 1 plot; CR05 & CR06 combined in 1 plot
#2010: CR05 & CR06 combined in 1 plot
#2011: CR05 & CR06 combined in 1 plot
#2012: CR05 & CR06 in 2 plots, both 0 hatching success<- RAO:combined into 1
#2013: CR05 & CR06 in 2 plots<- RAO:combined into 1
#2014: CR05 & CR06 maybe not included, both 0, should be combined into 1 plot: 7 nests; 
    #FT06 only one nest, combine with FT05?<- RAO:combined into 1
#2015: CR05 & CR06: no eggs
#2016: CR03 only 2 eggs, CR05 no eggs

files<-list.files(paste0(dir),pattern = "Productivity_Summary",full.names = T,recursive = T)
prod<-NULL
#options(warn=2)
#options(warn=1)
for (i in 1:length(files)){
    Edat<-read.csv(files[i])
    Edat$year<-as.numeric(strsplit(str_split(files[i],"/")[[1]][8],"_")[[1]][3])
    Edat$Nest_No<-as.character(Edat$Nest_No)
    Edat$Obs_Laying<-str_replace(Edat$Obs_Laying,"No","N")
    Edat$Obs_Pipping_Hatching<-str_replace(Edat$Obs_Pipping_Hatching,"No","N")
    
    Edat$First_Egg_Obs<-mdy(Edat$First_Egg_Obs)
    Edat$Last_Egg_Obs<-mdy(Edat$Last_Egg_Obs)#error!
    
    Edat$First_Chick_Obs<-mdy(Edat$First_Chick_Obs)
    Edat$Last_Chick_Obs<-mdy(Edat$Last_Chick_Obs)
    
    if (Edat$year[1]==2009){
      Edat$Plot<-str_replace(Edat$Plot,"CR02","CR02/03")
      Edat$Plot<-str_replace(Edat$Plot,"CR03","CR02/03")}
    if (Edat$year[1]==2014){
      Edat$Plot<-str_replace(Edat$Plot,"FT5","FT05/06")
      Edat$Plot<-str_replace(Edat$Plot,"FT6","FT05/06")}
    if (Edat$year[1]==2018){
      Edat$Plot<-str_replace(Edat$Plot,"WRWSC","WRW")
      Edat$Plot<-str_replace(Edat$Plot,"WRWSE","WRW")
      Edat$Plot<-str_replace(Edat$Plot,"WRWSB","WRW")
      Edat$Plot<-str_replace(Edat$Plot,"WRWSF","WRW")
      Edat$Plot<-str_replace(Edat$Plot,"WRWSH","WRW")
      Edat$Plot<-str_replace(Edat$Plot,"WRWSG","WRW")
      Edat$Plot<-str_replace(Edat$Plot,"WRWSA","WRW")
      Edat<-Edat%>%filter(Analysis.Notes=="Y")}
    Edat$Plot<-as.character(Edat$Plot)
    Edat$Comments<-as.character(Edat$Comments)
  prod<-bind_rows(prod,Edat)
}
unique(prod$Plot)

prod$Incub_Period<-as.numeric(prod$First_Chick_Obs-prod$First_Egg_Obs)
prod$Chick_Age<-as.numeric(prod$Last_Chick_Obs-prod$First_Chick_Obs)

prod<-prod%>%mutate(Total_Duration = ifelse(Chick_Age>0, Incub_Period+Chick_Age, NA))


prod%>%filter(Plot=="CR05/06")%>%
  group_by(Plot,year)%>%summarise(n=n())

prod$Plot<-str_replace(prod$Plot,"CR1","CR01")
prod$Plot<-str_replace(prod$Plot,"CR2","CR02")
prod$Plot<-str_replace(prod$Plot,"CR3","CR03")
prod$Plot<-str_replace(prod$Plot,"CR4","CR04")
prod$Plot<-str_replace(prod$Plot,"CR5","CR05")
prod$Plot<-str_replace(prod$Plot,"CR6","CR06")

prod$Plot<-str_replace(prod$Plot,"FT1","FT01")
prod$Plot<-str_replace(prod$Plot,"FT2","FT02")
prod$Plot<-str_replace(prod$Plot,"FT3","FT03")
prod$Plot<-str_replace(prod$Plot,"FT4","FT04")
prod$Plot<-str_replace(prod$Plot,"FT5","FT05")
prod$Plot<-str_replace(prod$Plot,"FT6","FT06")
prod$Plot<-str_replace(prod$Plot,"FT7","FT07")
unique(prod$Plot)

a<-prod%>%filter(Plot=="CR05" | Plot=="CR06")%>%
  group_by(Plot,year)%>%summarise(n=n())

for (i in 1:nrow(prod)){
if (prod$year[i]!=2018 & prod$year[i]!=2012 & prod$year[i]!=2013){
prod$Plot[i]<-str_replace(prod$Plot[i],"CR05","CR05/06")
prod$Plot[i]<-str_replace(prod$Plot[i],"CR06","CR05/06")}}

for (i in 1:nrow(prod)){
  if (prod$year[i]==2018){
    prod$Plot[i]<-str_replace(prod$Plot[i],"FT01","FT01/02/03/06")
    prod$Plot[i]<-str_replace(prod$Plot[i],"FT02","FT01/02/03/06")
    prod$Plot[i]<-str_replace(prod$Plot[i],"FT03","FT01/02/03/06")
    prod$Plot[i]<-str_replace(prod$Plot[i],"FT06","FT01/02/03/06")}}

for (i in 1:nrow(prod)){
  if (prod$year[i]==2017){
    prod$Plot[i]<-str_replace(prod$Plot[i],"FT03","FT03/04/05/06")
    prod$Plot[i]<-str_replace(prod$Plot[i],"FT04","FT03/04/05/06")
    prod$Plot[i]<-str_replace(prod$Plot[i],"FT05","FT03/04/05/06")
    prod$Plot[i]<-str_replace(prod$Plot[i],"FT06","FT03/04/05/06")
    prod$Plot[i]<-str_replace(prod$Plot[i],"CR04","CR04/5/6")
    prod$Plot[i]<-str_replace(prod$Plot[i],"CR05/06","CR04/5/6")
    }}

for (i in 1:nrow(prod)){
  if (prod$year[i]==2016){
    prod$Plot[i]<-str_replace(prod$Plot[i],"CR02","CR02/03")
    prod$Plot[i]<-str_replace(prod$Plot[i],"CR03","CR02/03")
    prod$Plot[i]<-str_replace(prod$Plot[i],"CR04","CR04/5/6")
    prod$Plot[i]<-str_replace(prod$Plot[i],"CR05/06","CR04/5/6")
    prod$Plot[i]<-str_replace(prod$Plot[i],"CR04/5/6","CR04/05/06")
    }}

for (i in 1:nrow(prod)){
  if (prod$year[i]==2015){
    prod$Plot[i]<-str_replace(prod$Plot[i],"CR03","Cnew")
    prod$Plot[i]<-str_replace(prod$Plot[i],"CR04","Cnew")
    prod$Plot[i]<-str_replace(prod$Plot[i],"CR05/06","Cnew")
    prod$Plot[i]<-str_replace(prod$Plot[i],"Cnew","CR03/04/05/06")
    prod$Plot[i]<-str_replace(prod$Plot[i],"FT05","FT05/06")
    prod$Plot[i]<-str_replace(prod$Plot[i],"FT06","FT05/06")
  }}



plotnumbers<-prod%>%group_by(year,Plot)%>%summarise(n_nests=n())
plotnumbers%>%filter(n_nests<4)
plotnumbers%>%filter(year==2017)
min(plotnumbers$n_nests)
plotnumbers%>%group_by(year)%>%summarise(n_plots=n())

write.csv(prod,paste0(anadir,"/processeddata/COMU_YHONA_NestProd_2007to",max(prod$year,na.rm=TRUE),".csv"))
saveRDS(prod,paste0(anadir,"/processeddata/COMU_YHONA_NestProd_2007to",max(prod$year,na.rm=TRUE),".rda"))


# Productivity Summary ----------------------------------------------------
#hatching success
a<-prod%>%
  dplyr::filter(is.na(First_Egg_Obs)==FALSE)%>%
  group_by(year, Plot)%>%
  summarise(eggs=n())
b<-prod%>%
  dplyr::filter(is.na(First_Chick_Obs)==FALSE)%>%
  group_by(year, Plot)%>%
  summarise(chicks=n())
aa<-left_join(a,b,by=c("year","Plot"))
aa$chicks[is.na(aa$chicks)==TRUE]<-0
aa$Hatching_Success<-aa$chicks/aa$eggs

aa<-left_join(aa,prod%>%group_by(year, Plot)%>%dplyr::filter(Chick_Age>=9)%>%summarise(chick10=n()),by=c("year","Plot"))
aa<-left_join(aa,prod%>%group_by(year, Plot)%>%dplyr::filter(Chick_Age>=15)%>%summarise(chick15=n()),by=c("year","Plot"))
aa<-left_join(aa,prod%>%group_by(year, Plot)%>%dplyr::filter(Chick_Age>=17)%>%summarise(chick17=n()),by=c("year","Plot"))
aa<-left_join(aa,prod%>%group_by(year, Plot)%>%dplyr::filter(Chick_Age>=21)%>%summarise(chick21=n()),by=c("year","Plot"))

aa$chick10[is.na(aa$chick10)==TRUE]<-0
aa$chick15[is.na(aa$chick15)==TRUE]<-0
aa$chick17[is.na(aa$chick17)==TRUE]<-0
aa$chick21[is.na(aa$chick21)==TRUE]<-0

aa$Chick_10days_per_Egg<-aa$chick10/aa$eggs
aa$Chick_15days_per_Egg<-aa$chick15/aa$eggs
aa$Chick_17days_per_Egg<-aa$chick17/aa$eggs
aa$Chick_21days_per_Egg<-aa$chick21/aa$eggs

aa$Chick_10days_per_Chick<-aa$chick10/aa$chicks
aa$Chick_15days_per_Chick<-aa$chick15/aa$chicks
aa$Chick_17days_per_Chick<-aa$chick17/aa$chicks
aa$Chick_21days_per_Chick<-aa$chick21/aa$chicks


ggplot()+
  geom_line(data=aa,
            aes(x=year,y=Chick_15days_per_Egg,group=Plot,color=as.factor(Plot)))
ggplot()+
  geom_line(data=aa,aes(x=year,y=Hatching_Success,group=Plot,color=as.factor(Plot)))
mean(aa$Chick_15days_per_Egg,na.rm=TRUE)

a2018<-aa%>%filter(year==2018)
mean(a2018$Hatching_Success)
aa%>%filter(year==2011)
a2018<-aa%>%filter(year==2015)%>%filter(Hatching_Success>0)

sum_prod<-aa%>%group_by(year)%>%
  summarise(HatchingSuccess=mean(Hatching_Success),
            HatchingSuccess_sd=sd(Hatching_Success),
            Fledging_Success15=mean(Chick_15days_per_Chick,na.rm=TRUE),
            Fledging_Success15_sd=sd(Chick_15days_per_Chick,na.rm=TRUE),
            Reproductive_Success15=mean(Chick_15days_per_Egg,na.rm=TRUE),
            Reproductive_Success15_sd=sd(Chick_15days_per_Egg,na.rm=TRUE),
                                   n_plots=n_distinct(Plot),
            total_eggs=sum(eggs),
            total_chicks=sum(chicks))
sum_prod$HatchingSuccess_se=sum_prod$HatchingSuccess_sd/sqrt(sum_prod$n_plots)
sum_prod$Reproductive_Success15_se=sum_prod$HatchingSuccess_sd/sqrt(sum_prod$n_plots)
sum_prod

chickplots<-aa%>%group_by(year)%>%
  filter(Chick_15days_per_Egg>0)%>%
  summarise(n_plots_chicks=n_distinct(Plot))

ec_timing<-prod%>%group_by(year)%>%
  summarise(first_hatch=min(First_Chick_Obs,na.rm=TRUE),
            Med_hatch=median(First_Chick_Obs,na.rm=TRUE))

sum_prod<-left_join(sum_prod,chickplots)
sum_prod<-left_join(sum_prod,ec_timing,by="year")
sum_prod$Reproductive_Success15_se=sum_prod$Reproductive_Success15_sd/sqrt(sum_prod$n_plots)
sum_prod

minegg<-prod%>%group_by(year)%>%
  summarise(minEgg=min(First_Egg_Obs,na.rm=TRUE))
sum_prod<-left_join(sum_prod,minegg,by="year")

COMUold<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/COMU_TopDown_BottomUp/YHONA_COMU_1998_2018.csv",nrows=21)
head(COMUold)
COMUold<-COMUold[,1:14]
str(COMUold)
COMUold$first_hatch<-as.character(COMUold$first_hatch)
COMUold$Med_hatch<-as.character(COMUold$Med_hatch )
COMUold$Adultstd<-COMUold$Adult/max(COMUold$Adult,na.rm = TRUE)
COMUold$Eggstd<-COMUold$Egg/max(COMUold$Egg,na.rm = TRUE)
COMUold$Diststd<-COMUold$disturbances/max(COMUold$disturbances,na.rm = TRUE)
COMUold$first_hatch<-ydm(paste0(COMUold$Year,"-",COMUold$first_hatch))
COMUold$Med_hatch <-ydm(paste0(COMUold$Year,"-",COMUold$Med_hatch ))

data.table::setnames(COMUold, old=c("Year","Hatc_Success","Repro_Success","num_plots","disturbances",
                                    "Egg","Chick","Adult","Hours","Days"), 
                     new=c("year", "HatchingSuccess","Reproductive_Success15","n_plots","n_Event",
                           "egg_hour","chick_hour","adult_hour","sum_hr","days"))

C<-COMUold%>%filter(year<2003)  
sum_prod<-bind_rows(sum_prod,C)


saveRDS(sum_prod,paste0(anadir,"/YHONA_COMU_ProductivitySummary_2007-",max(sum_prod$year,na.rm=TRUE),".rda"))

unique(aa$Plot)
aa$PlotGroup<-"ColonyRock"
aa$PlotGroup[aa$Plot=="FT01" | aa$Plot=="FT02" | aa$Plot=="FT03" |
               aa$Plot=="FT04" | aa$Plot=="FT05" | aa$Plot=="FT06" |
               aa$Plot=="FT05/06" | aa$Plot=="FT07" |
               aa$Plot=="FT01/02/03/06" | aa$Plot=="FT03/04/05/06"]<-"FlatTop"
aa$PlotGroup[aa$Plot=="LCR" | aa$Plot=="SRA" | aa$Plot=="SRB" |
               aa$Plot=="LH" | aa$Plot=="WRW" ]<-"Satelite"
unique(aa%>%filter(PlotGroup=="ColonyRock")%>%ungroup()%>%select(Plot))
unique(aa%>%filter(PlotGroup=="FlatTop")%>%ungroup()%>%select(Plot))
unique(aa%>%filter(PlotGroup=="FlatTop")%>%ungroup()%>%select(Plot))

pg.repro<-aa%>%group_by(year,PlotGroup)%>%
  summarise(uHatching_Success=mean(Hatching_Success, na.rm=TRUE),
            uChick_15days_per_Egg=mean(Chick_15days_per_Egg, na.rm=TRUE),
            uChick_15days_per_Chick=mean(Chick_15days_per_Chick, na.rm=TRUE))
ggplot()+
  geom_point(data=pg.repro, aes(x=year,y=uHatching_Success, color=PlotGroup))
ggplot()+
  geom_point(data=pg.repro, aes(x=year,y=uChick_15days_per_Egg, color=PlotGroup))
ggplot()+
  geom_point(data=pg.repro, aes(x=year,y=uChick_15days_per_Chick, color=PlotGroup))

pg.repro%>%filter(PlotGroup=="ColonyRock")
pg.repro%>%filter(PlotGroup=="FlatTop")
