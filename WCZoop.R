WCZooper<-function(Download=F){
  

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
    download.file("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2018CBMatrix.xlsx", 
                  "Data/1972-2018CBMatrix.xlsx", mode="wb")
  }
  
  if (Download) {
    download.file("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2018Pump Matrix.xlsx", 
                  "Data/1972-2018Pump Matrix.xlsx", mode="wb")
  }
   

# Load and combine data ---------------------------------------------------

  
  #**********Only including OTHCYCAD from CB because biomass indicates they're large, and only including small cyclopoids from pump sample******#
  
  ZoopCB<-read_excel("Data/1972-2018CBMatrix.xlsx", 
                     sheet = "CB CPUE Matrix 1972-2018", 
                     col_types = c("numeric","numeric", "numeric", "numeric", "date", 
                                   "text", "text", "text", "numeric", "text", "text",
                                   "text", rep("numeric", 62)))%>%
    select(Date, Station, ACARTELA, ACARTIA, DIAPTOM, EURYTEM, OTHCALAD, PDIAPFOR, PDIAPMAR, SINOCAL, TORTANUS, AVERNAL, OTHCYCAD, BOSMINA, DAPHNIA, DIAPHAN, OTHCLADO)%>%
    gather(key="Taxa", value="CPUE", -Date, -Station)
  
  
  ZoopPump<-read_excel("Data/1972-2018Pump Matrix.xlsx", 
                       sheet = " Pump CPUE Matrix 1972-2018", 
                       col_types = c("numeric","numeric", "numeric", "numeric", "date", 
                                     "text", "text", "text", "numeric", 
                                     "text", rep("numeric", 36)))%>%
    select(Date=SampleDate, Station, LIMNOSPP, LIMNOSINE, LIMNOTET, OITHDAV, OITHSIM, OITHSPP)%>%
    gather(key="Taxa", value="CPUE",-Date, -Station)
  
  
  
  ZoopMysid<-read_excel("Data/EMPMysidBPUEMatrixAug2019.xlsx",
                        sheet="MysidBPUEMatrix1972-2018",
                        col_types = c(rep("numeric", 4), "date", "text", "text", "numeric", "numeric", "text", "text", rep("numeric", 16)))%>%
    #mutate(Date=parse_date_time(Date, orders="mdy"))%>%
    select(Date, Station, `Acanthomysis aspera`:`Unidentified mysid`)%>%
    mutate(Mysida=rowSums(select(., -Date, -Station), na.rm=T))%>%
    gather(key="Taxa", value="BPUE", -Date, -Station)%>%
    mutate(BPUE=BPUE*1000,
           Taxa="Mysida") # Convert to ug
  
  Zoopmass<-read_csv("Data/zoop_individual_mass.csv", col_types = "cd")
  
  Zoop<-bind_rows(ZoopCB, ZoopPump)%>%
    left_join(Zoopmass, by=c("Taxa"="taxon"))%>%
    mutate(BPUE=CPUE*mass_indiv_ug,
           Taxa=case_when(
             Taxa%in%c("ACARTELA", "ACARTIA", "DIAPTOM", "EURYTEM", "OTHCALAD", "PDIAPFOR", "PDIAPMAR", "SINOCAL", "TORTANUS") ~ "Calanoida",
             Taxa%in%c("AVERNAL", "LIMNOSPP", "LIMNOSINE", "LIMNOTET", "OITHDAV", "OITHSIM", "OITHSPP", "OTHCYCAD") ~ "Cyclopoida",
             Taxa%in%c("BOSMINA", "DAPHNIA", "DIAPHAN", "OTHCLADO") ~ "Cladocera"))%>%
    select(-CPUE, -mass_indiv_ug)%>%
    bind_rows(ZoopMysid)%>%
    mutate(Year=year(Date),
           MonthYear=floor_date(Date, unit = "month"))


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
                dplyr::select(Region=Stratum))
  
  #Add regions and lat/long to zoop dataset
  Zoopsum<-Zoop%>%
    left_join(Stations, by="Station")%>%
    filter(!is.na(Region))%>% 
    group_by(Region, Year, Taxa)%>%
    summarise(BPUE=mean(BPUE, na.rm=T))%>%
    ungroup()
  

# Plot --------------------------------------------------------------------

  p<-Zoopsum%>%
    filter(Year>1991)%>%
    mutate(Taxa=factor(Taxa, levels=c("Calanoida", "Cyclopoida", "Cladocera", "Mysida")))%>%
    ggplot(aes(x=Year, y=BPUE, fill=Taxa))+
    geom_area()+
    scale_x_continuous(breaks = seq(1990, 2020, by=5))+
    scale_fill_manual(values=brewer.pal(4, "BrBG"))+
    coord_cartesian(expand=0)+
    xlab("Date")+
    facet_wrap(~Region)+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank())
  p
  return(p)
  
  }
