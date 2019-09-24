WCFisher<-function(){
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
  
  
  #**********Only including OTHCYCAD from CB because biomass indicates they're large, and only including small cyclopoids from pump sample******#
  
  IEP_Indices<-read_excel("Data/FMWT DS index.xlsx")%>%
    select(Year, Index=Total)%>%
    mutate(Source="FMWT")%>%
    bind_rows(
      read_excel("Data/STN DS index.xlsx")%>%
        mutate(Source="STN"),
      read_excel("Data/SKT DS index.xlsx")%>%
        mutate(Source="SKT"),
      read_excel("Data/20mm DS index.xlsx")%>%
        mutate(Source="20mm"))%>%
    filter(Year>=1991)
  
  EDSM<-read_csv("Data/edsm_abund_estimates_2019-09-17.csv")%>%
    mutate(Stratum=recode(Stratum, "Cache Slough LI"="Cache Slough/Liberty Island", "Sac DW Ship Channel"="Sac Deep Water Shipping Channel"),
           Date=WeekStartDate+ceiling((WeekEndDate-WeekStartDate)/2))%>%
    filter(Stratum%in%c("Cache Slough/Liberty Island", "Lower Sacramento", "Lower San Joaquin", "Sac Deep Water Shipping Channel", "Southern Delta", "Suisun Bay", "Suisun Marsh", "Upper Sacramento", "Western Delta"))%>%
    select(Region=Stratum, Date, Abundance=nHat, lowCI, uppCI)%>%
    mutate(Abundance_l=log10(Abundance+1),
           lowCI_l=log10(lowCI+1),
           uppCI_l=log10(uppCI+1))%>%
    filter(!(Region%in%c("Upper Sacramento", "Southern Delta"))) #No DS ever caught in these regions so removing them
  
  
  # Add regions and summarise -------------------------------------------------------------
  
  
  # Plot --------------------------------------------------------------------
  
  p<-list()
  
  p$IEP<-ggplot(IEP_Indices, aes(x=Year, y=Index, color=Source))+
    geom_line(size=1)+
    geom_point()+
    coord_cartesian(expand=0)+
    facet_grid(Source~., scales = "free_y")+
    scale_color_brewer(type="div", palette="RdYlBu", guide="none")+
    scale_x_continuous(labels=insert_minor(seq(1990, 2020, by=5), 4), breaks = 1990:2020)+
    ylab("Index value")+
    xlab("Date")+
    ggtitle("IEP Delta smelt index values")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20))
  
  p$EDSM<-ggplot(data=EDSM, aes(x=Date, y=Abundance_l))+
    geom_point(color="darkorchid4")+
    geom_errorbar(aes(ymin=lowCI_l, ymax=uppCI_l), alpha=0.6)+
    coord_cartesian(expand=0)+
    facet_wrap(~Region)+
    ylab("log(Delta Smelt abundance+1)")+
    xlab("Date")+
    ggtitle("EDSM Delta Smelt Abundance")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20))
  
  
  
  
  ggsave(p$IEP, filename="Figures/IEP Fish.png", device = "png", width = 7.5, height=4, units="in")
  ggsave(p$EDSM, filename="Figures/EDSM Fish.png", device = "png", width = 7.5, height=4, units="in")
  return(p)
  
}