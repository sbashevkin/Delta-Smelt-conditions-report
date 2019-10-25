WCZooper<-function(Start_year=2002, End_year=2018, Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Lower Joaquin River", "Southern Delta"), Seasons="Fall"){
  

# Setup -------------------------------------------------------------------

  
  require(sf)
  require(rgdal)
  require(raster)
  require(tidyverse)
  require(readxl)
  require(lubridate)
  require(RColorBrewer)


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
  
  #Add regions and lat/long to zoop dataset
  Zoopsum<-Zoop%>%
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
    summarise(BPUE=mean(BPUE, na.rm=T))%>%
    ungroup()%>%
    filter(Year>=Start_year)%>%
    droplevels()%>%
    mutate(missing="na")%>%
    complete(Year, Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))%>%
    mutate(Region=factor(Region, levels=Regions))
  
  Zoopmissing<-Zoopsum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Zoopsum<-Zoopsum%>%
    filter(is.na(missing))%>%
    select(-missing)%>%
    mutate(Taxa=factor(Taxa, levels=c("Calanoida", "Cyclopoida", "Cladocera", "Mysida")))
  

# Plot --------------------------------------------------------------------

  p<-ggplot()+
    geom_bar(data=filter(Zoopsum, Year!=End_year), aes(x=Year, y=BPUE, fill=Taxa), stat="identity", alpha=0.7)+
    geom_bar(data=filter(Zoopsum, Year==End_year), aes(x=Year, y=BPUE, fill=Taxa), stat="identity", alpha=1)+
    geom_vline(data=Zoopmissing, aes(xintercept=Year), linetype=2)+
    scale_x_continuous(breaks = seq(1990, 2020, by=5))+
    scale_fill_brewer(type="div", palette="BrBG", guide=guide_legend(title=NULL, keyheight=0.8))+
    scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","))+
    coord_cartesian(expand=0)+
    xlab("Date")+
    ggtitle(paste(Seasons, "zooplankton", collapse=", "))+
    facet_wrap(~Region)+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20), legend.position = c(0.85,0.2), legend.text=element_text(size=8), legend.background=element_rect(fill="white", color="black"))
  
  ggsave(p, filename="Figures/Zooplankton.png", device = "png", width = 7.5, height=4, units="in")
  
  return(p)
  
  }
