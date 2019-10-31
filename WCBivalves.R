WCBivalver<-function(Start_year=2002, End_year=2018, Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"), Seasons="Fall"){
  
  
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
    filter(Year>=Start_year)%>%
    left_join(Stations, by="Station")%>%
    filter(Region%in%Regions)%>%
    mutate(Month=month(MonthYear))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )%>%
    filter(Season%in%Seasons)%>%
    droplevels()%>%
    group_by(Region, Year, Taxa)%>%
    summarise(CPUE=mean(CPUE, na.rm=T))%>%
    ungroup()%>%
    mutate(missing="na",
           Region=as.character(Region))%>%
    complete(Year=Start_year:(End_year), Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))%>%
    mutate(Region=factor(Region, levels=Regions))
  
  Bivmissing<-Bivsum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Bivsum<-Bivsum%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  
  
  # Plot --------------------------------------------------------------------
  
  p<-ggplot()+
    geom_vline(data=Bivmissing, aes(xintercept=Year), linetype=2)+
    geom_bar(data=Bivsum, aes(x=Year, y=CPUE, fill=Taxa), stat="identity")+
    geom_bar(data=Bivsum%>%filter(Year==End_year)%>%group_by(Region, Year)%>%summarise(CPUE=sum(CPUE)), aes(x=Year, y=CPUE), stat="identity", color="firebrick3", fill=NA, size=1)+
    scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year-1,End_year+1), expand=expand_scale(0,0))+
    scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","), expand=expand_scale(0,0))+
    xlab("Date")+
    scale_fill_manual(values=c("#d8b365", "#5ab4ac"), guide=guide_legend(title=NULL))+
    facet_wrap(~Region, scales="free_x")+
    ggtitle(paste(Seasons, "invasive bivalve abundance", collapse=", "))+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), legend.position = c(0.85, 0.2), plot.title = element_text(hjust = 0.5, size=20), legend.background=element_rect(fill="white", color="black"))
  
  ggsave(p, filename="Figures/Bivalves.png", device = "png", width = 7.5, height=4, units="in")
  return(p)
  
}
