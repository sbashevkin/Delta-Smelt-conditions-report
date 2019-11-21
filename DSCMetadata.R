DSCMetadater<-function(Data, Start_year=2002, Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta")){
  
  require(tidyverse)
  require(lubridate)
  require(RColorBrewer)
  require(ggthemes)
  
  
  # Water quality data ------------------------------------------------------
  
  
  
  # Load and combine data ---------------------------------------------------
  
  
  WQsum<-Data$Water_quality%>%
    select(Region, Season, Year, Source, Chlorophyll, Salinity, Secchi, Temperature, Microcystis)%>%
    pivot_longer(c(Chlorophyll, Salinity, Secchi, Temperature, Microcystis), names_to="Parameter", values_to = "Value")%>%
    filter(!is.na(Value))%>%
    select(-Value)


# Bivalves ----------------------------------------------------------------

  Bivsum<-Data$Bivalves%>%
    select(Region, Season, Year)%>%
    mutate(Source="EMP",
           Parameter="Bivalves")
  

# Zooplankton -------------------------------------------------------------

  Zoopsum<-Data$Zooplankton%>%
    select(-Taxa, -BPUE)%>%
    distinct()%>%
    select(Region, Season, Year)%>%
    mutate(Source="EMP",
           Parameter="Zooplankton")
  

# Phytoplankton -----------------------------------------------------------

  Phytosum<-Data$Phytoplankton%>%
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
           Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")),
           Season=recode(Season, Winter="Winter\nDec - Feb", Spring="Spring\nMar - May", Summer="Summer\nJun - Aug", Fall="Fall\nSep - Nov"),
           Source=recode(Source, TNS="STN"),
           Parameter=factor(Parameter, levels=c("Temperature", "Secchi", "Salinity", "Chlorophyll", "Phytoplankton", "Microcystis", "Zooplankton", "Bivalves")))
  

# Plot --------------------------------------------------------------------

  p<-ggplot(sum, aes(x=Region, y=Yearly_samples, fill=Source))+
    geom_bar(stat="identity")+
    facet_grid(Parameter~Season, scales = "free_y")+
    scale_fill_colorblind(guide=guide_legend(direction="horizontal"))+
    scale_y_continuous(expand = c(0,0), limits=c(0,NA))+
    ylab("Average number of data points per year")+
    theme_bw()+
    theme(axis.text.x = element_text(angle=45, hjust=1), panel.grid=element_blank(), strip.background = element_blank(), text=element_text(size=12), plot.margin = margin(35,0,0,35), strip.text.y = element_text(angle=0, hjust=0), panel.spacing.y = unit(0.5, "lines"), legend.position=c(0.5, 1.1), legend.background = element_rect(color="black"))
  
  return(p)
  
  ggsave("Figures/Metadata figure.png", p, device="png", width=9, height=7)  
  
}