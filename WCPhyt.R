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
  
  
  # Download data -----------------------------------------------------------
  
  
  #CB
  if (Download) {
    download.file("https://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/Phytoplankton_Algal_Type_Data_1975_-2016.csv", 
                  "Data/Phytoplankton_Algal_Type_Data_1975_-2016.csv", mode="wb")
  }
  
  
  # Load and combine data ---------------------------------------------------
  
  
  #**********Only including OTHCYCAD from CB because biomass indicates they're large, and only including small cyclopoids from pump sample******#
  
  Phyto<-read_csv("Data/Phytoplankton_Algal_Type_Data_1975_-2016.csv",
                  col_types = "ccddddddddddddddddddd")%>%
    rename(Date=SampleDate, Station=StationCode)%>%
    mutate(Date=parse_date_time(Date, "mdy"))%>%
    gather(key="Taxa", value="CPUE", -Date, -Station)%>%
    mutate(Taxa=case_when(Taxa%in%c("Centric Diatoms", "Pennate Diatoms") ~ "Diatoms",
                          Taxa%in%c("Other flagellate", "Unknown Flagellates") ~ "Other flagellates",
                          Taxa%in%c("Cryptophytes", "Green Algae", "Chrysophytes", "Dinoflagellates") ~ Taxa,
                          TRUE ~ "Other taxa"))%>%
    mutate(Year=year(Date))
  
  
  # Add regions and summarise -------------------------------------------------------------
  
  Stations<-read_csv("Data/zoop_stations.csv",
                     col_types = "cdcdddddddd")%>%
    mutate(Latitude=lat_deg+lat_min/60+lat_sec/3600,
           Longitude=(long_deg+long_min/60+long_sec/3600)*(-1))%>%
    select(Station=station, Latitude, Longitude)%>%
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
                dplyr::select(Region=Stratum))%>%
    mutate(Region=replace_na(as.character(Region), "San Pablo Bay"))
  
  #Add regions and lat/long to zoop dataset
  Phytosum<-Phyto%>%
    left_join(Stations, by="Station")%>%
    filter(!is.na(Region))%>% 
    group_by(Region, Year, Taxa)%>%
    summarise(CPUE=mean(CPUE, na.rm=T))%>%
    ungroup()
  
  
  # Plot --------------------------------------------------------------------
  
  p<-Phytosum%>%
    filter(Year>1991)%>%
    mutate(Taxa=factor(Taxa, levels=c("Diatoms", "Cryptophytes", "Green Algae", "Chrysophytes", "Dinoflagellates", "Other flagellates", "Other taxa")))%>%
    ggplot(aes(x=Year, y=CPUE, fill=Taxa))+
    geom_area()+
    scale_x_continuous(labels=insert_minor(seq(1990, 2020, by=5), 4), breaks = 1990:2020)+
    scale_fill_manual(values=brewer.pal(4, "BrBG"))+
    coord_cartesian(expand=0)+
    facet_wrap(~Region)+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank())
  
  return(p)
  
}
