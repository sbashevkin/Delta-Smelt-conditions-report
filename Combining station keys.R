#Combining Station ID sheets
require(tidyverse)
require(readxl)

FMWT<-read_excel("Data/FMWT Station.xlsx")%>%
  select(Station=StationCode, Lat, Long#, Lat2=`WGS84 Lat`, Long2=`WGS84 Long`
         )%>%
  separate(Lat, into=c("Lat_d", "Lat_m", "Lat_s"), sep="[ ]{1,}", convert=T)%>%
  separate(Long, into=c("Long_d", "Long_m", "Long_s"), sep="[ ]{1,}", convert=T)%>%
  mutate(Latitude=Lat_d+Lat_m/60+Lat_s/3600,
         Longitude=Long_d-Long_m/60-Long_s/3600,
         Source="FMWT",
         StationID=paste(Source, Station),
  #       Lat2=parse_double(Lat2),
  #       Long2=parse_double(Long2)
  )%>%
  #mutate(Latitude=if_else(is.na(Latitude), Lat2, Latitude),
  #       Longitude=if_else(is.na(Longitude), -1*Long2, Longitude))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

STN<-read_excel("Data/STN Station.xlsx")%>%
  select(Station=StationCodeSTN, LatD, LatM, LatS, LonD, LonM, LonS)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1,
         Source="STN",
         StationID=paste(Source, Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

Zoopxl<-read_excel("Data/zoop_stations.xlsx")%>%
  rename(Source=Project)%>%
  mutate(StationID=paste(Source, Station))%>%
  drop_na()

WQ<-read_csv("Data/wq_stations.csv")%>%
  select(Station=site, Latitude=lat, Longitude=long)%>%
  mutate(Source="EMP",
         StationID=paste(Source, Station))%>%
  drop_na()

Stations<-bind_rows(
  FMWT, 
  STN, 
  Zoopxl%>%
    filter(!(StationID%in%unique(FMWT$StationID)) & !(StationID%in%unique(STN$StationID))),
  WQ%>%
    filter(!(StationID%in%unique(Zoopxl$StationID))))

write_csv(Stations, "Data/Master station key.csv")
