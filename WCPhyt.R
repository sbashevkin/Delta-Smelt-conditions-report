#MISSING STATION LAT LONGS FOR SOME STATIONS

WCPhyter<-function(Download=F){
  
  
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

  # Load and combine data ---------------------------------------------------
  
  Phyto<-read_csv("Data/Phytoplankton_Algal_Type_Data_1975_-2016.csv",
                  col_types = "ccddddddddddddddddddd")%>%
    rename(Date=SampleDate, Station=StationCode)%>%
    mutate(Date=parse_date_time(Date, "mdy"))%>%
    gather(key="Taxa", value="CPUE", -Date, -Station)%>%
    mutate(Taxa=case_when(Taxa%in%c("Centric Diatoms", "Pennate Diatoms") ~ "Diatoms",
                          Taxa%in%c("Other flagellate", "Unknown Flagellates") ~ "Other flagellates",
                          Taxa%in%c("Cryptophytes", "Green Algae", "Chrysophytes", "Dinoflagellates") ~ Taxa,
                          TRUE ~ "Other taxa"))%>%
    mutate(Year=year(Date),
           MonthYear=floor_date(Date, unit = "month"))
  
  
  # Add regions and summarise -------------------------------------------------------------
  
  Stations<-read_csv("Data/Master station key.csv",
                     col_types = "cddcc")%>%
    drop_na()%>%
    filter(Project=="EMP")
  
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
  
  #Add regions and lat/long to phyto dataset
  Phytosum<-Phyto%>%
    left_join(Stations, by="Station")%>%
    filter(!is.na(Region))%>% 
    group_by(Region, Year, Taxa)%>%
    summarise(CPUE=mean(CPUE, na.rm=T))%>%
    ungroup()%>%
    filter(Year>1991)%>%
    mutate(Taxa=factor(Taxa, levels=c("Diatoms", "Cryptophytes", "Green Algae", "Chrysophytes", "Dinoflagellates", "Other flagellates", "Other taxa")),
           missing="na",
           Region=as.character(Region))%>%
    complete(Year, Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))
  
  Phytomissing<-Phytosum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Phytosum<-Phytosum%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  Peak<-tibble(Region=Phytosum$Region[which.max(Phytosum$CPUE)], Year=Phytosum$Year[which.max(Phytosum$CPUE)], label=paste0("Peak CPUE: ", format(round(max(Phytosum$CPUE)), big.mark=",")))
  
  
  # Plot --------------------------------------------------------------------
  
  p<-ggplot()+
    geom_bar(data=Phytosum, aes(x=Year, y=CPUE, fill=Taxa), stat="identity")+
    geom_vline(data=Phytomissing, aes(xintercept=Year), linetype=2)+
    geom_label(data=Peak, aes(x=Year-7, y=35000, label=label), size=3)+
    #scale_x_continuous(labels=insert_minor(seq(1990, 2020, by=5), 4), breaks = 1990:2020)+
    scale_fill_manual(values=brewer.pal(7, "BrBG"))+
    xlab("Date")+
    coord_cartesian(expand=0, ylim=c(0,40000))+
    facet_wrap(~Region)+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank())
  
  p
  
  return(p)
  
}
