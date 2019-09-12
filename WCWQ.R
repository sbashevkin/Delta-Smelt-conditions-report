WCWQer<-function(){
  
  
  # Setup -------------------------------------------------------------------
  
  
  require(sf)
  require(rgdal)
  require(raster)
  require(tidyverse)
  require(readxl)
  require(lubridate)
  require(RColorBrewer)
  
  
  # Load and combine data ---------------------------------------------------
  
  FMWT<-read_excel("Data/FMWT 1967-2018 Catch Matrix_updated.xlsx", sheet="FlatFile", guess_max=30000)%>%
    select(Date, Station, Temperature=`Top Temperature (°C)`, Conductivity=`Top EC (µS/cm)`, Secchi=`Secchi (m)`, Microcystis)%>%
    mutate(Source="FMWT",
           Secchi=Secchi*100,
           Microcystis=if_else(Microcystis==6, 2, Microcystis))
  
  STN<-read_excel("Data/Townet_Data_1959-2018.xlsx", sheet="CatchPerStation", guess_max=10000)%>%
    select(Date=SampleDate, Station=StationCode, Secchi, Temperature=`TemperatureTop`, Conductivity=`ConductivityTop`)%>%
    mutate(Source="TNS")
  
  EDSM<-read_csv("Data/EDSM_20mm.csv", guess_max=9000)%>%
    select(Date, Latitude=StartLat, Longitude=StartLong, Conductivity=TopEC, Temperature=TopTemp, Secchi=Scchi)%>%
    bind_rows(read_csv("Data/EDSM_KDTR.csv", guess_max=30000)%>%
                select(Date, Latitude=StartLat, Longitude=StartLong, Conductivity=EC, Temperature=Temp, Secchi=Scchi))%>%
    mutate(Secchi=Secchi*100)%>%
    mutate(Station=paste(Latitude, Longitude),
           Source="EDSM",
           Date=parse_date_time(Date, "%m/%d/%Y"))%>%
    select(-Conductivity) #Methods in EDI metadata say they do not know if their data were corrected for temperature so I will not use this data
  
  
  Fieldfiles <- list.files(path = "Data/Water quality", full.names = T, pattern="Field")
  
  Labfiles <- list.files(path = "Data/Water quality", full.names = T, pattern="Lab")
  
  Widefiles <- list.files(path = "Data/Water quality", full.names = T, pattern="EMP")
  
  WQ<-sapply(Fieldfiles, function(x) read_excel(x, guess_max = 5e4))%>%
    bind_rows()%>%
    select(Date=SampleDate, Station=StationCode, Parameter=AnalyteName, Value=Result, Notes=TextResult)%>%
    filter(Parameter%in%c("Temperature", "Secchi Depth", "Conductance (EC)"))%>%
    group_by(Date, Station, Parameter, Notes)%>%
    summarise(Value=mean(Value, na.rm=T))%>%
    ungroup()%>%
    bind_rows(sapply(Labfiles, function(x) read_excel(x, guess_max = 5e4))%>%
                bind_rows()%>%
                select(Station=StationCode, Date=SampleDate, Parameter=ConstituentName, Value=Result, Notes=LabAnalysisRemarks)%>%
                filter(Parameter=="Chlorophyll a")%>%
                group_by(Date, Station, Parameter, Notes)%>%
                summarise(Value=mean(Value, na.rm=T))%>%
                ungroup())%>%
    spread(key=Parameter, value=Value)%>%
    rename(Chla=`Chlorophyll a`, Secchi_depth=`Secchi Depth`, Conducivity=`Conductance (EC)`)%>%
    bind_rows(sapply(Widefiles, function(x) read_excel(x)%>%
                       select(Station=`Station Name`, Date=`Sample Date`, Chla=`Chlorophyll a µg/L`, Notes=`Field Notes`, Secchi_depth=`Secchi Depth Centimeters`, Temperature=`Water Temperature °C`, Conducivity=`Specific Conductance uS/cm@25degC`)%>%
                       {if(is.character(.$Chla)){
                         mutate(., Chla=parse_double(ifelse(Chla%in%c("<0.05", "<0.5"), 0, Chla), na = c("", "NA", "N/A", "<R.L.")))
                       } else{
                         .
                       }}%>%
                       {if(is.character(.$Secchi_depth)){
                         mutate(., Secchi_depth=parse_double(Secchi_depth, na = c("", "NA", "N/A", "Too dark")))
                       } else{
                         .
                       }}%>%
                       {if(is.character(.$Temperature)){
                         mutate(., Temperature=parse_double(Temperature, na = c("", "NA", "N/A")))
                       } else{
                         .
                       }}%>%
                       {if(is.character(.$Conducivity)){
                         mutate(., Conducivity=parse_double(Conducivity, na = c("", "NA", "N/A")))
                       } else{
                         .
                       }}, 
                     simplify=FALSE) %>% 
                bind_rows())%>%
    mutate(Source="EMP")%>%
    bind_rows(FMWT, EDSM%>%
                select(-Latitude, -Longitude))%>%
    mutate(MonthYear=floor_date(Date, unit = "month"),
           Year=year(Date),
           Salinity=((0.36966/(((Conducivity*0.001)^(-1.07))-0.00074))*1.28156))
  
  # Add regions and summarise -------------------------------------------------------------
  
  Stations<-read_csv("Data/Master station key.csv",
                     col_types = "cddcc")%>%
    select(-StationID)%>%
    bind_rows(EDSM%>%
                select(Latitude, Longitude, Station, Source))%>%
    drop_na()
  
  #Load delta regions shapefile from Morgan
  Deltaregions<-st_read("Data/Delta regions", quiet=T)
  Deltaregions<-as(Deltaregions, "Spatial")
  
  #Match each unique region from zoop dataset to a region from the shapefile
  Locations<-Stations
  coordinates(Locations) <- ~Longitude+Latitude
  proj4string(Locations) <- CRS("+proj=longlat +datum=NAD83")
  Locations <- spTransform(Locations, proj4string(Deltaregions))
  Locations<-Locations %over% Deltaregions
  Stations<-Stations%>%
    bind_cols(Locations%>%
                dplyr::select(Region=Stratum))
  
  #Add regions and lat/long to zoop dataset
  WQsum<-WQ%>%
    filter(year(MonthYear)>1991)%>%
    left_join(Stations, by=c("Source", "Station"))%>%
    filter(!is.na(Region))%>% 
    group_by(Region, Year, MonthYear)%>%
    summarise(Temperature=mean(Temperature, na.rm=T), Chla=mean(Chla, na.rm=T), Secchi_depth=mean(Secchi_depth, na.rm=T), Salinity=mean(Salinity, na.rm=T), N_Microcystis=length(which(!is.na(Microcystis))), Microcystis1=length(which(Microcystis==1))/N_Microcystis, Microcystis2=length(which(Microcystis==2))/N_Microcystis, Microcystis3=length(which(Microcystis==3))/N_Microcystis, Microcystis4=length(which(Microcystis==4))/N_Microcystis, Microcystis5=length(which(Microcystis==5))/N_Microcystis)%>%
    ungroup()
  
  Microsum<-WQ%>%
    filter(year(MonthYear)>1991 & !is.na(Microcystis))%>%
    left_join(Stations, by=c("Source", "Station"))%>%
    filter(!is.na(Region))%>% 
    group_by(Region, Year)%>%
    summarise(N_Microcystis=length(which(!is.na(Microcystis))), Microcystis1=length(which(Microcystis==1))/N_Microcystis, Microcystis2=length(which(Microcystis==2))/N_Microcystis, Microcystis3=length(which(Microcystis==3))/N_Microcystis, Microcystis4=length(which(Microcystis==4))/N_Microcystis, Microcystis5=length(which(Microcystis==5))/N_Microcystis)%>%
    ungroup()
  
  WQsumTemp<-WQsum%>%
    mutate(Season=ifelse(month(MonthYear)<7, "Winter/Spring", "Summer/Fall"))%>%
    group_by(Season, Year, Region)%>%
    summarise(Temperature=max(Temperature))
  
  WQsalrange<-WQsum%>%
    filter(!is.na(Salinity))%>%
    group_by(Region)%>%
    summarise(Salrange=paste0("min: ", round(min(Salinity), 2), "\nmax: ", round(max(Salinity), 2)))
  
  WQchlrange<-tibble(xmin=min(WQsum$MonthYear), xmax=max(WQsum$MonthYear), Region=unique(as.character(filter(WQsum, !is.na(Chla))$Region)))
  WQchlrange<-WQchlrange%>%
    mutate(ymin=0, ymax=10, Quality="Bad")%>%
    bind_rows(WQchlrange%>%
                mutate(ymin=10, ymax=max(WQsum$Chla, na.rm=T), Quality="Good"))  
  
  # Plot --------------------------------------------------------------------
  
  plotWQ<-function(Parameter, ylabel){
    Parameter<-enquo(Parameter)
    ggplot()+
      geom_line(data=filter(WQsum, !is.na(!!Parameter)), aes(x=MonthYear, y=!!Parameter))+
      coord_cartesian(expand=0)+
      facet_wrap(~Region)+
      ylab(ylabel)+
      xlab("Date")+
      theme_bw()+
      theme(panel.grid=element_blank(), strip.background = element_blank())
  }
  
  #TempShades<-expand.grid(Region=unique(WQsum$Region), Quality=c("Good", "OK", "Bad"))%>%
  #  mutate(xmin=min(WQsum$MonthYear),
  #         xmax=max(WQsum$MonthYear),
  #         ymin=case_when(
  #           Quality=="Good" ~ min(WQsum$Temperature),
  #           Quality=="OK" ~ 20,
  #           Quality=="Bad" ~ 26
  #         ),
  #         ymax=case_when(
  #           Quality=="Good" ~ 20,
  #           Quality=="OK" ~ 26,
  #           Quality=="Bad" ~ max(WQsum$Temperature)
  #         ))
  
  pTemp<-ggplot()+
    geom_line(data=WQsumTemp, aes(x=Year, y=Temperature, color=Season))+
    coord_cartesian(expand=0)+
    facet_wrap(~Region)+
    ylab("Maximum monthly mean temperature (°C)")+
    xlab("Date")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank())
  
  
  pSecchi<-plotWQ(Secchi_depth, "Secchi depth (cm)")
  pChla<-plotWQ(Chla, "Chlorophyll a (µg/L)")+geom_rect(data=WQchlrange, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=Quality), alpha=0.3)+scale_fill_manual(guide="none", values=c("#fc8d62", "#66c2a5"))
  
  pSal<-plotWQ(Salinity, "Salinity")+geom_label(data=WQsalrange, aes(x=as.POSIXct("1997-01-01"), y=23, label=Salrange), alpha=0.5, size=2.5)
  
  ###LOOKS LIKE MICRO ONLY MEASURED IN SUMMER?
  pMicro<-Microsum%>%
    filter(N_Microcystis>0)%>%
    gather(key="Severity", value="Frequency", Microcystis1, Microcystis2, Microcystis3, Microcystis4, Microcystis5)%>%
    mutate(Severity=recode_factor(factor(Severity), "Microcystis1"="1", "Microcystis2"="2", "Microcystis3"="3", "Microcystis4"="4", "Microcystis5"="5", .ordered=T))%>%
    mutate(Severity=factor(Severity, levels=c(5,4,3,2,1)))%>%
    ggplot(aes(x=Year, y=Frequency, fill=Severity))+
    geom_bar(stat="identity")+
    scale_fill_brewer(type="div", palette="RdYlBu")+
    coord_cartesian(expand=0)+
    facet_wrap(~Region)+
    ylab("Relative frequency")+
    xlab("Date")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank())
  
  plots<-list(Temperature=pTemp, Secchi=pSecchi, Chlorophyll=pChla, Salinity=pSal, Microcystis=pMicro)
  
  sapply(1:length(plots), function(x) ggsave(plots[[x]], filename=paste0("Figures/", names(plots[x]), ".png"), device = "png", width = 7.5, height=5, units="in"))
  
  return(plots)
  
}