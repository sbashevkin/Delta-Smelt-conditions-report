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
  
  
  # Download data -----------------------------------------------------------
  
  
  #CB
  if (Download) {
    download.file("https://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/Phytoplankton_Algal_Type_Data_1975_-2016.csv", 
                  "Data/Phytoplankton_Algal_Type_Data_1975_-2016.csv", mode="wb")
  }
  
  
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
  
  Stations<-read_csv("Data/wq_stations.csv",
                     col_types = "cddc")%>%
    select(Station=site, Latitude=lat, Longitude=long)%>%
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
  
  #Add regions and lat/long to phyto dataset
  Phytosum<-Phyto%>%
    left_join(Stations, by="Station")%>%
    filter(!is.na(Region))%>% 
    group_by(Region, Year, MonthYear, Taxa)%>%
    summarise(CPUE=mean(CPUE, na.rm=T))%>%
    ungroup()
  
  
  # Plot --------------------------------------------------------------------
  
  p<-Phytosum%>%
    filter(Year>1991)%>%
    mutate(Taxa=factor(Taxa, levels=c("Diatoms", "Cryptophytes", "Green Algae", "Chrysophytes", "Dinoflagellates", "Other flagellates", "Other taxa")))%>%
    ggplot(aes(x=MonthYear, y=CPUE, fill=Taxa))+
    geom_area()+
    #scale_x_continuous(labels=insert_minor(seq(1990, 2020, by=5), 4), breaks = 1990:2020)+
    scale_fill_manual(values=brewer.pal(7, "BrBG"))+
    xlab("Date")+
    coord_cartesian(expand=0, ylim=c(0,200000))+
    facet_wrap(~Region)+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank())
  
  return(p)
  
}
