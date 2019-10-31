WCMetadater<-function(Start_year=2002, Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta")){
  
  require(sf)
  require(rgdal)
  require(raster)
  require(tidyverse)
  require(readxl)
  require(lubridate)
  require(RColorBrewer)
  require(ggthemes)
  
  
  # Water quality data ------------------------------------------------------
  
  
  
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
    mutate(Month=month(MonthYear))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )%>%
    select(Region, Season, Year, Source, Chlorophyll, Salinity, Secchi, Temperature, Microcystis)%>%
    pivot_longer(c(Chlorophyll, Salinity, Secchi, Temperature, Microcystis), names_to="Parameter", values_to = "Value")%>%
    filter(!is.na(Value))%>%
    select(-Value)


# Bivalves ----------------------------------------------------------------

  Biv<-read_excel("Data/1975-18 CPUE bivalves only, 2019Sept9.xlsx",
                  sheet = "75-18 CPUE per m2", skip=1)%>%
    select(Date, Station=StationCode, `Potamocorbula amurensis`, `Corbicula fluminea`)%>%
    gather(key="Taxa", value="CPUE", -Station, -Date)%>%
    mutate(Year=year(Date),
           MonthYear=floor_date(Date, unit = "month"))
  
  
  # Add regions and summarise -------------------------------------------------------------
  
  BivStations<-read_excel("Data/1975-18 CPUE bivalves only, 2019Sept9.xlsx",
                       sheet = "75-17 station locations", skip=1)%>%
    select(Station=Site_Code, Latitude, Longitude)
  
  #Match each unique region from zoop dataset to a region from the shapefile
  Locations<-BivStations
  coordinates(Locations) <- ~Longitude+Latitude
  proj4string(Locations) <- CRS("+proj=longlat +datum=NAD83")
  Locations <- spTransform(Locations, proj4string(Deltaregions))
  Locations<-Locations %over% Deltaregions
  BivStations<-BivStations%>%
    bind_cols(Locations%>%
                dplyr::select(Region=Stratum))
  
  #Add regions and lat/long to zoop dataset
  Bivsum<-Biv%>%
    filter(Year>=Start_year)%>%
    left_join(BivStations, by="Station")%>%
    filter(!is.na(Region))%>%
    mutate(Month=month(MonthYear))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )%>%
    select(Region, Season, Year)%>%
    mutate(Source="EMP",
           Parameter="Bivalves")
  

# Zooplankton -------------------------------------------------------------

  # Load and combine data ---------------------------------------------------
  
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
  
  
  #Add regions and lat/long to zoop dataset
  Zoopsum<-Zoop%>%
    select(-Taxa, -BPUE)%>%
    distinct()%>%
    left_join(filter(Stations, Source=="EMP"), by="Station")%>%
    filter(!is.na(Region))%>%
    mutate(Month=month(MonthYear))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )%>%
    filter(year(MonthYear)>=Start_year)%>%
    select(Region, Season, Year)%>%
    mutate(Source="EMP",
           Parameter="Zooplankton")
  

# Phytoplankton -----------------------------------------------------------

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
  
  #Add regions and lat/long to phyto dataset
  Phytosum<-Phyto%>%
    left_join(Stations, by="Station")%>%
    filter(!is.na(Region))%>%
    mutate(Month=month(Date))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )%>%
    select(Region, Season, Year)%>%
    mutate(Source="EMP",
           Parameter="Phytoplankton")
  

# Combine all datasets ----------------------------------------------------

sum<-bind_rows(WQsum, Bivsum, Zoopsum, Phytosum)%>%
    group_by(Parameter)%>%
    mutate(Years=length(unique(Year)))%>%
    ungroup()%>%
    group_by(Region, Season, Source, Parameter, Years)%>%
    summarise(N=n())%>%
    ungroup()%>%
    filter(Region%in%Regions)%>%
    mutate(Region=factor(Region, levels=Regions),
           Yearly_samples=N/Years,
           Season=recode(Season, Winter="Winter\nDec - Feb", Spring="Spring\nMar - May", Summer="Summer\nJun - Aug", Fall="Fall\nSep - Nov"))
  

# Plot --------------------------------------------------------------------

  p<-ggplot(sum, aes(x=Region, y=Yearly_samples, fill=Source))+
    geom_bar(stat="identity")+
    facet_grid(Parameter~Season, scales = "free_y")+
    scale_fill_colorblind()+
    scale_y_continuous(expand = c(0,0), limits=c(0,NA))+
    ylab("Average number of data points per year")+
    theme_bw()+
    theme(axis.text.x = element_text(angle=45, hjust=1), panel.grid=element_blank(), strip.background = element_blank(), text=element_text(size=12), plot.margin = margin(0,0,0,35), strip.text.y = element_text(angle=0, hjust=0), panel.spacing.y = unit(0.5, "lines"))
  
  return(p)
  
  ggsave("Figures/Metadata figure.png", p, device="png", width=9, height=7)  
  
}