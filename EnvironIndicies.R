library(readr) #for read_csv that inturprets dates for you
library(lubridate)

library(dplyr)
library(tidyr)
library(readxl)

library(broom)
library(stringr)

library(scales)
library(ggplot2)
##library(openxlsx) 
##convertToDate(effor17$Date)

if(Sys.info()[["user"]]=="rachaelorben") {
  dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/COMU_TopDown_BottomUp/"
}


# UW 45N ------------------------------------------------------------------
UW<-read_excel(path = paste0(dir,"environmental_data_Yaquina_seabirds.xlsx"),sheet="UW_45")
UW<-UW[,1:13]%>%gather(month,value,Jan:Dec)
UW<-data.table::setnames(UW,"Year","year")
UW<-UW%>%mutate(dateP=dmy(paste0(15,"-",month,"-",year)))
UW$Date = as.POSIXct(UW$dateP, format = "%Y-%m-%d")
UW$index<-"UW_45"
UW$lat<-"45N"
UW$month<-month(UW$Date)
UW

# MEI ---------------------------------------------------------------------
#I think the JUNJUL + JULAUG + AUGSEP will be a good place to start for a summer MEI value. 
#You could step the season forward one month and see if that makes a major difference, 
#though I doubt it will since the MEI is so autocorrelated.
MEI<-read_excel(path = paste0(dir,"environmental_data_Yaquina_seabirds.xlsx"),sheet="MEI")
MEI<-MEI[,1:13]%>%gather(monthlong,value,DECJAN:NOVDEC)
MEI$month<-str_sub(MEI$monthlong,start=-3)
MEI<-MEI%>%mutate(dateP=dmy(paste0(15,"-",month,"-",YEAR)))
MEI<-data.table::setnames(MEI,"YEAR","year")
MEI$Date = as.POSIXct(MEI$dateP, format = "%Y-%m-%d")
MEI$index<-"MEI"
MEI$month<-month(MEI$Date)
MEI<-MEI%>%select(-monthlong)
MEI

# PDO ---------------------------------------------------------------------
##NCEI PDO index is based on NOAA's extended reconstruction of SSTs (ERSST Version 5).
##https://www.ncdc.noaa.gov/teleconnections/pdo/
##slightly different than PDO tab see ERSST_PDO tab
PDO<-read_excel(path = paste0(dir,"environmental_data_Yaquina_seabirds.xlsx"),sheet="PDO")
PDO<-PDO[,1:13]%>%gather(month,value,JAN:DEC)
PDO<-PDO%>%mutate(dateP=dmy(paste0(15,"-",month,"-",YEAR)))
PDO<-data.table::setnames(PDO,"YEAR","year")
PDO$Date = as.POSIXct(PDO$dateP, format = "%Y-%m-%d")
PDO$index<-"PDO"
PDO$month<-month(PDO$Date)
PDO

# NOI ---------------------------------------------------------------------
NOI<-read_excel(path = paste0(dir,"environmental_data_Yaquina_seabirds.xlsx"),sheet="NOI")
NOI<-NOI[,1:13]%>%gather(month,value,Jan:Dec)
NOI<-NOI%>%mutate(dateP=dmy(paste0(15,"-",month,"-",year)))
NOI$Date = as.POSIXct(NOI$dateP, format = "%Y-%m-%d")
NOI$index<-"NOI"
NOI$month<-month(NOI$Date)
NOI

# CUTI --------------------------------------------------------------------
#http://mjacox.com/upwelling-indices/
CUTI<-read_csv(paste0(dir,"CUTI_monthly.csv"))
CUTI<-CUTI[,1:19]%>%gather(lat,value,`47N`:`31N`)
CUTI<-CUTI%>%mutate(dateP=dmy(paste0(15,"-",month,"-",year)))
CUTI$Date = as.POSIXct(CUTI$dateP, format = "%Y-%m-%d")
CUTI$index<-"CUTI"
CUTI<-CUTI%>%filter(lat=="45N")
str(CUTI)
CUTI

# BEUTI --------------------------------------------------------------------
#http://mjacox.com/upwelling-indices/
BEUTI<-read_csv(paste0(dir,"BEUTI_monthly.csv"))
BEUTI<-BEUTI[,1:19]%>%gather(lat,value,`47N`:`31N`)
BEUTI<-BEUTI%>%mutate(dateP=dmy(paste0(15,"-",month,"-",year)))
BEUTI$Date = as.POSIXct(BEUTI$dateP, format = "%Y-%m-%d")
BEUTI$index<-"BEUTI"
BEUTI<-BEUTI%>%filter(lat=="45N")
str(BEUTI)
BEUTI

# Combo INDICIES ----------------------------------------------------------
indicies<-bind_rows(PDO,NOI,UW,MEI,CUTI,BEUTI)

indicies$season<-NA
indicies$month<-month(indicies$Date)
indicies$season[indicies$month==1| indicies$month==2 | indicies$month==3]<-"winter"
indicies$season[indicies$month==4 | indicies$month==5 | indicies$month==6]<-"spring"
indicies$season[indicies$month==7 | indicies$month==8 | indicies$month==9]<-"summer"
indicies$season[indicies$month==10 | indicies$month==11 | indicies$month==12]<-"autumn"

indicies$season<-factor(indicies$season, ordered = TRUE, 
                   levels = c("winter", "spring", "summer", "autumn"))

head(indicies)
saveRDS(indicies,paste0(dir,"YHONA_tdbu_Analysis/processeddata/Indicies.rda"))

# PLOTs -------------------------------------------------------------------
ggplot()+
  geom_line(data=indicies,
            aes(x=Date,y=value,group=index,color=index))+
  facet_wrap(~index,scales = "free")+
  theme_bw()

ggplot()+
  geom_line(data=indicies%>%filter(year>2005),
            aes(x=Date,y=value,group=index,color=index))+
  facet_wrap(~index,scales = "free_y")+
  theme_bw()

ggplot()+
  geom_line(data=indicies%>%filter(year>2000)%>%
              group_by(year,index,season)%>%
              summarise(Anomaly=mean(value))%>%filter(season=="summer"),
            aes(x=year,y=Anomaly,group=index,color=index))+
  geom_point(data=indicies%>%filter(year>2000)%>%
              group_by(year,index,season)%>%
              summarise(Anomaly=mean(value))%>%filter(season=="summer"),
            aes(x=year,y=Anomaly,group=index,color=index))+
  facet_wrap(~index,scales = "free")+
  theme_bw()+ scale_x_continuous(breaks= pretty_breaks())
ggsave(paste0(dir,"SummerIndicies.png"))

ggplot()+
  geom_line(data=indicies%>%filter(year>2000)%>%
              group_by(year,index,season)%>%
              summarise(Anomaly=mean(value))%>%filter(season=="spring"),
            aes(x=year,y=Anomaly,group=index,color=index))+
  geom_point(data=indicies%>%filter(year>2000)%>%
               group_by(year,index,season)%>%
               summarise(Anomaly=mean(value))%>%filter(season=="spring"),
             aes(x=year,y=Anomaly,group=index,color=index))+
  facet_wrap(~index,scales = "free")+
  theme_bw()+ scale_x_continuous(breaks= pretty_breaks())
ggsave(paste0(dir,"SpringIndicies.png"))

ggplot()+
  geom_line(data=indicies%>%filter(year>1998)%>%
              group_by(year,index,season)%>%
              summarise(Anomaly=mean(value))%>%filter(season=="summer"),
            aes(x=year,y=Anomaly,group=index,color=index))+
  facet_wrap(~index,scales = "free_y")+
  theme_bw()+ scale_x_continuous(breaks= pretty_breaks())


Sumindicies<-indicies%>%filter(year>1998)%>%
  group_by(year,index,season)%>%
  summarise(Anomaly=mean(value))%>%filter(season=="summer")
Sumindicies

str(Sumindicies)

Sumindicies_wd<-spread(Sumindicies,index,Anomaly)%>%select(-season)
Sumindicies_wd<-data.frame(Sumindicies_wd)
str(Sumindicies_wd)

library(corrplot)
M <- cor(Sumindicies_wd[1:12,2:7])
corrplot(M, method = "circle")
corrplot(M, method="number", diag=F)
corrplot(cor(Sumindicies_wd[1:12,2:7]), method="number", order="hclust", addrect=3, diag=F)

cor.test.mat <- function(mat){
  n <- ncol(mat)
  pmat <- matrix(0, nrow = n, ncol = n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      pmat[i,j] <- cor.test(mat[,i], mat[,j], method="pearson")$p.value
    }
  }
  pmat[lower.tri(pmat)] <- t(pmat)[lower.tri(pmat)] #fill lower triangle with upper triangle
  return(pmat)
}  

#compute matrix of p-values
pvals <- cor.test.mat(Sumindicies_wd[1:12,2:7])

corrplot(cor(Sumindicies_wd[1:12,2:7]), method="number", order="hclust", addrect=2, diag=F)
corrplot(cor(Sumindicies_wd[1:12,2:7]), p.mat = pvals, sig.level=0, insig = "p-value", method="ellipse", order="hclust", 
         type="upper", addrect=2, tl.pos = "n", cl.pos="n", diag=F, add=T)

