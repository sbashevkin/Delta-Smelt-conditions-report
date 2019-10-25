WCWQer<-function(Start_year=2002, End_year=2018, Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"), Temp_season="Summer", Secchi_season="Fall", Salinity_season="Fall", Chl_season="Summer", Micro_season="Summer"){
  
  
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
    select(-Conductivity)%>% #Methods in EDI metadata say they do not know if their data were corrected for temperature so I will not use this data
    distinct()
  
  Fieldfiles <- list.files(path = "Data/Water quality", full.names = T, pattern="Field")
  
  Labfiles <- list.files(path = "Data/Water quality", full.names = T, pattern="Lab")
  
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
    rename(Chlorophyll=`Chlorophyll a`, Secchi=`Secchi Depth`, Conductivity=`Conductance (EC)`)%>%
    bind_rows(read_excel("Data/EMP WQ Combined_2000-2018.xlsx", na=c("N/A", "<R.L.", "Too dark"), col_types = c(rep("text", 3), "date", rep("text", 37)))%>%
                select(Station=`Station Name`, Date, Chlorophyll=starts_with("Chlorophyll"), Latitude=`North Latitude Degrees (d.dd)`, Longitude=`West Longitude Degrees (d.dd)`, Microcystis=`Microcystis aeruginosa`, Secchi=`Secchi Depth Centimeters`, Temperature=starts_with("Water Temperature"), Conductivity=starts_with("Specific Conductance"))%>%
                mutate(Chlorophyll=parse_double(ifelse(Chlorophyll%in%c("<0.05", "<0.5"), 0, Chlorophyll)),
                       Latitude=parse_double(Latitude),
                       Longitude=parse_double(Longitude),
                       Microcystis=parse_double(Microcystis),
                       Secchi=parse_double(Secchi),
                       Temperature=parse_double(Temperature),
                       Conductivity=parse_double(Conductivity),
                       Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station))%>%
                mutate(Microcystis=round(Microcystis))%>% #EMP has some 2.5 and 3.5 values
                select(-Latitude, -Longitude))%>%
    mutate(Source="EMP")%>%
    bind_rows(FMWT, STN, EDSM%>%
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
    filter(year(MonthYear)>=Start_year)%>%
    left_join(Stations, by=c("Source", "Station"))%>%
    filter(Region%in%Regions)%>%
    mutate(Month=month(MonthYear))
  
  Secchisum<-WQsum%>%
    select(Month, Region, Secchi, Year)%>%
    filter(!is.na(Secchi))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )%>%
    filter(Season%in%Secchi_season)%>%
    droplevels()%>%
    group_by(Region, Year)%>%
    summarise(Secchi=mean(Secchi, na.rm=T))%>%
    ungroup()%>%
    mutate(missing="na")%>%
    complete(Year, Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))%>%
    mutate(Region=factor(Region, levels=Regions))
  
  Secchimissing<-Secchisum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Secchisum<-Secchisum%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  Salsum<-WQsum%>%
    select(Month, Region, Salinity, Year)%>%
    filter(!is.na(Salinity))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )%>%
    filter(Season%in%Salinity_season)%>%
    droplevels()%>%
    group_by(Region, Year)%>%
    summarise(Salinity=mean(Salinity, na.rm=T))%>%
    ungroup()%>%
    mutate(missing="na")%>%
    complete(Year, Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))%>%
    mutate(Region=factor(Region, levels=Regions))
  
  Salmissing<-Salsum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Salsum<-Salsum%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  Chlsum<-WQsum%>%
    select(Month, Region, Chlorophyll, Year)%>%
    filter(!is.na(Chlorophyll))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )%>%
    filter(Season%in%Chl_season)%>%
    droplevels()%>%
    group_by(Region, Year)%>%
    summarise(Chlorophyll=mean(Chlorophyll, na.rm=T))%>%
    ungroup()%>%
    mutate(missing="na")%>%
    complete(Year, Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))%>%
    mutate(Region=factor(Region, levels=Regions))
  
  Chlmissing<-Chlsum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Chlsum<-Chlsum%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  Microsum<-WQsum%>%
    select(Month, Region, Microcystis, Year)%>%
    filter(!is.na(Microcystis))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )%>%
    filter(Season%in%Micro_season)%>%
    droplevels()%>%
    group_by(Region, Year)%>%
    summarise(N_Microcystis=length(which(!is.na(Microcystis))), Microcystis1=length(which(Microcystis==1))/N_Microcystis, Microcystis2=length(which(Microcystis==2))/N_Microcystis, Microcystis3=length(which(Microcystis==3))/N_Microcystis, Microcystis4=length(which(Microcystis==4))/N_Microcystis, Microcystis5=length(which(Microcystis==5))/N_Microcystis)%>%
    ungroup()%>%
    filter(N_Microcystis>0)%>%
    gather(key="Severity", value="Frequency", Microcystis1, Microcystis2, Microcystis3, Microcystis4, Microcystis5)%>%
    mutate(Severity=recode(Severity, "Microcystis1"="Absent", "Microcystis2"="Low", "Microcystis3"="Medium", "Microcystis4"="High", "Microcystis5"="Very high"))%>%
    mutate(missing="na")%>%
    complete(Year, Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"),
           Region=factor(Region, levels=Regions))
  
  Micromissing<-Microsum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Microsum<-Microsum%>%
    filter(is.na(missing))%>%
    select(-missing)%>%
    mutate(Severity=factor(Severity, levels=c("Very high", "High", "Medium", "Low", "Absent")))
  
  Tempsum<-WQsum%>%
    select(Month, Region, Temperature, Year)%>%
    filter(!(Region%in%c("Cache Slough/Liberty Island", "Sac Deep Water Shipping Channel")))%>%
    droplevels()%>%
    group_by(Month, Year, Region)%>%
    summarise(Temperature=mean(Temperature, na.rm=T))%>%
    ungroup()%>%
    complete(Month, Year, Region)%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )%>%
    filter(Season%in%Temp_season)%>%
    group_by(Year, Region)%>%
    summarise(Temperature_max=max(Temperature), Temperature_min=min(Temperature), Temperature_med=median(Temperature), Temperature_mean=mean(Temperature))%>%
    ungroup()%>%
    mutate(Region=factor(Region, levels=Regions))
  
  Salrange<-Salsum%>%
    filter(!is.na(Salinity))%>%
    group_by(Region)%>%
    summarise(Salrange=paste0("min: ", round(min(Salinity), 2), ", max: ", round(max(Salinity), 2)))
  
  Chlrange<-tibble(xmin=min(Chlsum$Year), xmax=max(Chlsum$Year), Region=unique(as.character(filter(Chlsum, !is.na(Chlorophyll))$Region)))
  Chlrange<-Chlrange%>%
    mutate(ymin=0, ymax=10, Quality="Bad")%>%
    bind_rows(Chlrange%>%
                mutate(ymin=10, ymax=max(Chlsum$Chlorophyll, na.rm=T), Quality="Good"))%>%
    mutate(Region=factor(Region, levels=Regions)) 
  
  # Plot --------------------------------------------------------------------
  
  plotWQ<-function(Data, Parameter, ylabel, Title){
    Parameter<-enquo(Parameter)
    ggplot()+
      geom_line(data=Data, aes(x=Year, y=!!Parameter), color="firebrick3")+
      geom_point(data=filter(Data, Year==End_year), aes(x=Year, y=!!Parameter), color="firebrick3", size=3)+
      scale_y_continuous(expand = expand_scale(0,0))+
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
    geom_ribbon(data=Tempsum, aes(x=Year, ymin=Temperature_min, ymax=Temperature_max), alpha=0.4)+
    geom_point(data=filter(Tempsum, Year==End_year), aes(x=Year, y=Temperature_mean), color="firebrick3", size=3)+
    geom_line(data=Tempsum, aes(x=Year, y=Temperature_mean))+
    coord_cartesian(expand=0)+
    facet_wrap(~Region)+
    ggtitle(paste(Temp_season, "temperature", collapse=", "))+
    ylab(bquote(Temperature~"("*degree*c*")"))+
    scale_x_continuous(breaks = seq(2000, 2020, by=5))+
    xlab("Date")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20), legend.background=element_rect(fill="white", color="black"))
  
  
  pSecchi<-plotWQ(Secchisum, Secchi, "Secchi depth (cm)", paste(Secchi_season, "secchi depth", collapse=", "))+
    geom_vline(data=Secchimissing, aes(xintercept=Year), linetype=2)
  pChla<-plotWQ(Chlsum, Chlorophyll, bquote(Chlorophyll~a~"("*mu*g*"/L)"), paste(Chl_season, "chlorophyll", collapse=", "))+
    geom_vline(data=Chlmissing, aes(xintercept=Year), linetype=2)
  
  pSal<-plotWQ(Salsum, Salinity, "Salinity", paste(Salinity_season, "salinity", collapse=", "))+
    geom_label(data=Salrange, aes(x=2006, y=11, label=Salrange), alpha=0.5, size=2.5)+
    geom_vline(data=Salmissing, aes(xintercept=Year), linetype=2)
  
  ###LOOKS LIKE MICRO ONLY MEASURED IN SUMMER?
  pMicro<-ggplot()+
    geom_bar(data=filter(Microsum, Year!=End_year), aes(x=Year, y=Frequency, fill=Severity), stat="identity", alpha=0.7)+
    geom_bar(data=filter(Microsum, Year==End_year), aes(x=Year, y=Frequency, fill=Severity), stat="identity", alpha=1)+
    geom_vline(data=Micromissing, aes(xintercept=Year), linetype=2)+
    scale_fill_brewer(type="div", palette="RdYlBu", guide=guide_legend(keyheight=0.8, title=NULL, direction="horizontal", label.position="top", reverse=TRUE))+
    coord_cartesian(expand=0)+
    facet_wrap(~Region)+
    ylab("Relative frequency")+
    xlab("Date")+
    ggtitle(paste(Micro_season, "Microcystis", collapse=", "))+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20), legend.position=c(0.63, 0.13), legend.background=element_rect(fill="white", color="black"), legend.text = element_text(size=8))
  
  plots<-list(Temperature=pTemp, Secchi=pSecchi, Salinity=pSal, Chlorophyll=pChla, Microcystis=pMicro)
  
  sapply(1:length(plots), function(x) ggsave(plots[[x]], filename=paste0("Figures/", names(plots[x]), ".png"), device = "png", width = 7.5, height=4, units="in"))
  
  return(plots)
  
}