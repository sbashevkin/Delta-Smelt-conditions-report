#MISSING STATION LAT LONGS FOR SOME STATIONS

WCPhyter<-function(Download=F, Start_year=2002, End_year=2018, Regions=c("Cache Slough/Liberty Island", "Suisun Marsh", "Lower Sacramento River", "Suisun Bay", "Lower Joaquin River", "Southern Delta", "Sac Deep Water Shipping Channel"), Seasons="Summer"){
  
  
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
    bind_rows(read_excel("Data/Phytoplankton 2017.xlsx")%>%
                rename(Station=`Station Code`, Cryptophytes=Cryptomonads))%>%
    gather(key="Taxa", value="CPUE", -Date, -Station)%>%
    mutate(Taxa=case_when(Taxa%in%c("Centric Diatoms", "Pennate Diatoms") ~ "Diatoms",
                          Taxa%in%c("Other flagellate", "Unknown Flagellates") ~ "Other flagellates",
                          Taxa%in%c("Cryptophytes", "Green Algae", "Chrysophytes", "Dinoflagellates", "Cyanobacteria") ~ Taxa,
                          TRUE ~ "Other taxa"))%>%
    mutate(Year=year(Date),
           MonthYear=floor_date(Date, unit = "month"),
           Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station))%>%
    filter(Year>=2008)
  
  
  # Add regions and summarise -------------------------------------------------------------
  
  Stations<-read_csv("Data/Master station key.csv",
                     col_types =  "ccddc")%>%
    drop_na()%>%
    filter(Source=="EMP")
  
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
    filter(Region%in%Regions)%>% 
    group_by(Taxa, Region, Year, Date, Station)%>%
    summarise(CPUE=sum(CPUE, na.rm=T))%>%
    ungroup()%>%
    mutate(Month=month(Date))%>%
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
    filter(Year>=Start_year)%>%
    mutate(Taxa=factor(Taxa, levels=c("Diatoms", "Green Algae", "Cryptophytes", "Chrysophytes", "Cyanobacteria", "Dinoflagellates", "Other flagellates", "Other taxa")),
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
  
  Peak<-tibble(Region=filter(Phytosum, Taxa!="Cyanobacteria")$Region[which.max(filter(Phytosum, Taxa!="Cyanobacteria")$CPUE)], Year=filter(Phytosum, Taxa!="Cyanobacteria")$Year[which.max(filter(Phytosum, Taxa!="Cyanobacteria")$CPUE)], label=paste0("Peak CPUE: ", format(round(max(filter(Phytosum, Taxa!="Cyanobacteria")$CPUE)), big.mark=",")))
  
  
  # Plot --------------------------------------------------------------------
  
  pphyto<-ggplot()+
    geom_bar(data=filter(Phytosum, Taxa!="Cyanobacteria" & Year!=End_year), aes(x=Year, y=CPUE, fill=Taxa), stat="identity", alpha=0.7)+
    geom_bar(data=filter(Phytosum, Taxa!="Cyanobacteria" & Year==End_year), aes(x=Year, y=CPUE, fill=Taxa), stat="identity", alpha=1)+
    geom_vline(data=Phytomissing, aes(xintercept=Year), linetype=2)+
    geom_label(data=Peak, aes(x=Year-2, y=30000, label=label), size=3)+
    scale_fill_brewer(type="div", palette="BrBG", guide=guide_legend(keyheight=0.6, title=NULL), direction=-1)+
    xlab("Date")+
    coord_cartesian(expand=0, ylim=c(0,35000))+
    scale_x_continuous(breaks = seq(1990, 2020, by=5))+
    scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","))+
    facet_wrap(~Region)+
    ggtitle(paste(Seasons, "phytoplankton", collapse=", "))+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20), legend.position = c(0.85,0.2), legend.background=element_rect(fill="white", color="black"), legend.text = element_text(size=8))

  pcyano<-ggplot()+
    geom_bar(data=filter(Phytosum, Taxa=="Cyanobacteria" & Year!=End_year), aes(x=Year, y=CPUE), fill="chartreuse4", stat="identity", alpha=0.7)+
  geom_bar(data=filter(Phytosum, Taxa=="Cyanobacteria" & Year==End_year), aes(x=Year, y=CPUE), fill="chartreuse4", stat="identity", alpha=1)+
    geom_vline(data=Phytomissing, aes(xintercept=Year), linetype=2)+
    xlab("Date")+
    scale_x_continuous(breaks = seq(1990, 2020, by=5))+
    scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","))+
    coord_cartesian(expand=0)+
    facet_wrap(~Region)+
    ggtitle(paste(Seasons, "cyanobacteria", collapse=", "))+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20))

  ggsave(pphyto, filename="Figures/Phytoplankton.png", device = "png", width = 7.5, height=4, units="in")
  ggsave(pcyano, filename="Figures/Cyanobacteria.png", device = "png", width = 7.5, height=4, units="in")
  return(list(Phytoplankton=pphyto, Cyanobacteria=pcyano))
  
}
