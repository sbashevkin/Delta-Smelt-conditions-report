WCWQer<-function(){
  
  
  # Setup -------------------------------------------------------------------
  
  
  require(sf)
  require(rgdal)
  require(raster)
  require(tidyverse)
  require(readxl)
  require(lubridate)
  require(RColorBrewer)
  
  insert_minor <- function(major_labs, n_minor) {labs <- 
    c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  labs[1:(length(labs)-n_minor)]}
  
  
  # Download data -----------------------------------------------------------
  
  
  
  # Load and combine data ---------------------------------------------------
  
  Fieldfiles <- list.files(path = "Data/Water quality", full.names = T, pattern="Field")
  
  Labfiles <- list.files(path = "Data/Water quality", full.names = T, pattern="Lab")
  
  Widefiles <- list.files(path = "Data/Water quality", full.names = T, pattern="EMP")
  
  WQ<-sapply(Fieldfiles, function(x) read_excel(x, guess_max = 5e4))%>%
    bind_rows()%>%
    select(Date=SampleDate, Station=StationCode, Parameter=AnalyteName, Value=Result, Notes=TextResult)%>%
    filter(Parameter%in%c("Temperature", "Secchi Depth"))%>%
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
    rename(Chla=`Chlorophyll a`, Secchi_depth=`Secchi Depth`)%>%
    bind_rows(sapply(Widefiles, function(x) read_excel(x)%>%
                       select(Station=`Station Name`, Date=`Sample Date`, Chla=`Chlorophyll a µg/L`, Notes=`Field Notes`, Secchi_depth=`Secchi Depth Centimeters`, Temperature=`Water Temperature °C`)%>%
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
                       }}, 
                     simplify=FALSE) %>% 
                bind_rows())%>%
    mutate(Year=year(Date))
  
  # Add regions and summarise -------------------------------------------------------------
  
  Stations<-read_csv("Data/wq_stations.csv",
                     col_types = "cddc")%>%
    select(Station=site, Latitude=lat, Longitude=long)
  
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
    left_join(Stations, by="Station")%>%
    filter(!is.na(Region))%>% 
    group_by(Region, Year)%>%
    summarise(Temperature=mean(Temperature, na.rm=T), Chla=mean(Chla, na.rm=T), Secchi_depth=mean(Secchi_depth, na.rm=T))%>%
    ungroup()
  
  
  # Plot --------------------------------------------------------------------
  
  plotWQ<-function(Parameter, ylabel){
    Parameter<-enquo(Parameter)
    WQsum%>%
    filter(Year>1991)%>%
    ggplot(aes(x=Year, y=!!Parameter))+
    geom_line()+
    scale_x_continuous(labels=insert_minor(seq(1990, 2020, by=5), 4), breaks = 1990:2020)+
    coord_cartesian(expand=0)+
    facet_wrap(~Region)+
    ylab(ylabel)+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank())
  }
    
    pTemp<-plotWQ(Temperature, "Temperature (°C)")
    pSecchi<-plotWQ(Secchi_depth, "Secchi depth (cm)")
    pChla<-plotWQ(Chla, "Chlorophyll a (µg/L)")
  
  return(list(Temperature=pTemp, Secchi=pSecchi, Chlorophyll=pChla))
  
}
