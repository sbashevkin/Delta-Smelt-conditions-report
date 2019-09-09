WCBivalver<-function(Download=F){
  
  
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
  
  Biv<-read_excel("Data/1975-18 CPUE bivalves only, 2019Sept9.xlsx",
                     sheet = "75-18 CPUE per m2", skip=1)%>%
    select(Date, Station=StationCode, `Potamocorbula amurensis`, `Corbicula fluminea`)%>%
    gather(key="Taxa", value="CPUE", -Station, -Date)%>%
    mutate(Year=year(Date),
           MonthYear=floor_date(Date, unit = "month"))
  
  
  # Add regions and summarise -------------------------------------------------------------
  
  Stations<-read_excel("Data/1975-18 CPUE bivalves only, 2019Sept9.xlsx",
                       sheet = "75-17 station locations", skip=1)%>%
    select(Station=Site_Code, Latitude, Longitude)
  
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
  Bivsum<-Biv%>%
    left_join(Stations, by="Station")%>%
    filter(!is.na(Region))%>% 
    group_by(Region, Year, Taxa)%>%
    summarise(CPUE=mean(CPUE, na.rm=T))%>%
    ungroup()
  
  
  # Plot --------------------------------------------------------------------
  
  p<-Bivsum%>%
    filter(Year>1991)%>%
    ggplot(aes(x=Year, y=CPUE, fill=Taxa))+
    geom_area()+
    scale_x_continuous(labels=insert_minor(seq(1990, 2020, by=5), 4), breaks = 1990:2020)+
    xlab("Date")+
    scale_fill_manual(values=c("#d8b365", "#5ab4ac"))+
    coord_cartesian(expand=0)+
    facet_wrap(~Region)+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), legend.position = c(0.85, 0.2))
  
  return(p)
  
}
