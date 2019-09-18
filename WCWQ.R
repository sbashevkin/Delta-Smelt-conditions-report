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
    select(Date, Station, Conductivity=starts_with("Top EC"), Secchi=`Secchi (m)`, Microcystis, Temperature=starts_with("Top Temperature"))%>%
    mutate(Source="FMWT",
           Secchi=Secchi*100,
           Microcystis=if_else(Microcystis==6, 2, Microcystis))
  
  STN<-read_excel("Data/STN Sample.xlsx", guess_max=10000)%>%
    select(Date=SampleDate, Station=StationCode, Secchi, Temperature=`TemperatureTop`, Conductivity=`ConductivityTop`, Microcystis
    )%>%
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
    rename(Chlorophyll=`Chlorophyll a`, Secchi_depth=`Secchi Depth`, Conductivity=`Conductance (EC)`)%>%
    bind_rows(read_excel("Data/EMP WQ Combined_2000-2018.xlsx", na=c("N/A", "<R.L.", "Too dark"), col_types = c(rep("text", 3), "date", rep("text", 37)))%>%
                select(Station=`Station Name`, Date, Chlorophyll=starts_with("Chlorophyll"), Latitude=`North Latitude Degrees (d.dd)`, Longitude=`West Longitude Degrees (d.dd)`, Microcystis=`Microcystis aeruginosa`, Secchi_depth=`Secchi Depth Centimeters`, Temperature=starts_with("Water Temperature"), Conductivity=starts_with("Specific Conductance"))%>%
                mutate(Chlorophyll=parse_double(ifelse(Chlorophyll%in%c("<0.05", "<0.5"), 0, Chlorophyll)),
                       Latitude=parse_double(Latitude),
                       Longitude=parse_double(Longitude),
                       Microcystis=parse_double(Microcystis),
                       Secchi_depth=parse_double(Secchi_depth),
                       Temperature=parse_double(Temperature),
                       Conductivity=parse_double(Conductivity),
                       Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station))%>%
                mutate(Microcystis=round(Microcystis))%>% #EMP has some 2.5 and 3.5 values
                select(-Latitude, -Longitude))%>%
    mutate(Source="EMP")%>%
    bind_rows(FMWT, EDSM%>%
                select(-Latitude, -Longitude))%>%
    mutate(MonthYear=floor_date(Date, unit = "month"),
           Year=year(Date),
           Salinity=((0.36966/(((Conductivity*0.001)^(-1.07))-0.00074))*1.28156))
    
    # Add regions and summarise -------------------------------------------------------------
  
  Stations<-read_csv("Data/Master station key.csv",
                     col_types = "ccddc")%>%
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
    filter(year(MonthYear)>=1991)%>%
    left_join(Stations, by=c("Source", "Station"))%>%
    filter(!is.na(Region))%>% 
    group_by(Region, Year, MonthYear)%>%
    summarise(Temperature=mean(Temperature, na.rm=T), Chlorophyll=mean(Chlorophyll, na.rm=T), Secchi_depth=mean(Secchi_depth, na.rm=T), Salinity=mean(Salinity, na.rm=T), N_Microcystis=length(which(!is.na(Microcystis))), Microcystis1=length(which(Microcystis==1))/N_Microcystis, Microcystis2=length(which(Microcystis==2))/N_Microcystis, Microcystis3=length(which(Microcystis==3))/N_Microcystis, Microcystis4=length(which(Microcystis==4))/N_Microcystis, Microcystis5=length(which(Microcystis==5))/N_Microcystis)%>%
    ungroup()
  
  Microsum<-WQ%>%
    filter(year(MonthYear)>=1991 & !is.na(Microcystis))%>%
    left_join(Stations, by=c("Source", "Station"))%>%
    filter(!is.na(Region))%>% 
    group_by(Region, Year)%>%
    summarise(N_Microcystis=length(which(!is.na(Microcystis))), Microcystis1=length(which(Microcystis==1))/N_Microcystis, Microcystis2=length(which(Microcystis==2))/N_Microcystis, Microcystis3=length(which(Microcystis==3))/N_Microcystis, Microcystis4=length(which(Microcystis==4))/N_Microcystis, Microcystis5=length(which(Microcystis==5))/N_Microcystis)%>%
    ungroup()%>%
    filter(N_Microcystis>0)%>%
    gather(key="Severity", value="Frequency", Microcystis1, Microcystis2, Microcystis3, Microcystis4, Microcystis5)%>%
    mutate(Severity=recode(Severity, "Microcystis1"="Absent", "Microcystis2"="Low", "Microcystis3"="Medium", "Microcystis4"="High", "Microcystis5"="Very high"))%>%
    mutate(missing="na")%>%
    complete(Year, Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))
  
  Micromissing<-Microsum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Microsum<-Microsum%>%
    filter(is.na(missing))%>%
    select(-missing)%>%
    mutate(Severity=factor(Severity, levels=c("Very high", "High", "Medium", "Low", "Absent")))
  
  WQsumTemp<-WQsum%>%
    mutate(Season=ifelse(month(MonthYear)<7, "Winter/Spring", "Summer/Fall"))%>%
    group_by(Season, Year, Region)%>%
    summarise(Temperature=max(Temperature))%>%
    complete(Year, Region, Season)
  
  WQsalrange<-WQsum%>%
    filter(!is.na(Salinity))%>%
    group_by(Region)%>%
    summarise(Salrange=paste0("min: ", round(min(Salinity), 2), ", max: ", round(max(Salinity), 2)))
  
  WQchlrange<-tibble(xmin=min(WQsum$MonthYear), xmax=max(WQsum$MonthYear), Region=unique(as.character(filter(WQsum, !is.na(Chlorophyll))$Region)))
  WQchlrange<-WQchlrange%>%
    mutate(ymin=0, ymax=10, Quality="Bad")%>%
    bind_rows(WQchlrange%>%
                mutate(ymin=10, ymax=max(WQsum$Chlorophyll, na.rm=T), Quality="Good"))  
  
  # Plot --------------------------------------------------------------------
  
  plotWQ<-function(Parameter, ylabel, Title){
    Parameter<-enquo(Parameter)
    ggplot()+
      geom_line(data=filter(WQsum, !is.na(!!Parameter)), aes(x=MonthYear, y=!!Parameter), color="firebrick3")+
      coord_cartesian(expand=0)+
      facet_wrap(~Region)+
      ylab(ylabel)+
      xlab("Date")+
      ggtitle(Title)+
      theme_bw()+
      theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20))
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
    scale_color_discrete(guide=guide_legend(title=NULL))+
    ylab(bquote(Maximum~monthly~mean~temperature~"("*degree*C*")"))+
    ggtitle("Temperature")+
    scale_x_continuous(breaks = seq(1990, 2020, by=5))+
    xlab("Date")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20), legend.position=c(0.80,0.15), legend.background=element_rect(fill="white", color="black"))
  
  
  pSecchi<-plotWQ(Secchi_depth, "Secchi depth (cm)", "Secchi depth")
  pChla<-plotWQ(Chlorophyll, bquote(Chlorophyll~a~"("*mu*g*"/L)"), "Chlorophyll")+geom_rect(data=WQchlrange, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=Quality), alpha=0.3)+scale_fill_manual(guide="none", values=c("#fc8d62", "#66c2a5"))
  
  pSal<-plotWQ(Salinity, "Salinity", "Salinity")+geom_label(data=WQsalrange, aes(x=as.POSIXct("2000-01-01"), y=21, label=Salrange), alpha=0.5, size=2.5)
  
  ###LOOKS LIKE MICRO ONLY MEASURED IN SUMMER?
  pMicro<-ggplot()+
    geom_bar(data=Microsum, aes(x=Year, y=Frequency, fill=Severity), stat="identity")+
    geom_vline(data=Micromissing, aes(xintercept=Year), linetype=2)+
    scale_fill_brewer(type="div", palette="RdYlBu", guide=guide_legend(keyheight=0.8, title=NULL))+
    coord_cartesian(expand=0)+
    facet_wrap(~Region)+
    ylab("Relative frequency")+
    xlab("Date")+
    ggtitle("Microcystis")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20), legend.position=c(0.83, 0.13), legend.background=element_rect(fill="white", color="black"), legend.text = element_text(size=8))
  
  plots<-list(Temperature=pTemp, Secchi=pSecchi, Salinity=pSal, Chlorophyll=pChla, Microcystis=pMicro)
  
  sapply(1:length(plots), function(x) ggsave(plots[[x]], filename=paste0("Figures/", names(plots[x]), ".png"), device = "png", width = 7.5, height=4, units="in"))
  
  return(plots)
  
}