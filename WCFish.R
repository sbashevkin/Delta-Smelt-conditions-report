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
        mutate(Source="20mm"))
  
  
  # Add regions and summarise -------------------------------------------------------------
  
  
  # Plot --------------------------------------------------------------------
  
  p<-ggplot(IEP_Indices, aes(x=Year, y=Index, color=Source))+
    geom_line(size=1)+
    geom_point()+
    coord_cartesian(expand=0)+
    facet_grid(Source~., scales = "free_y")+
    scale_color_brewer(type="div", palette="RdYlBu", guide="none")+
    scale_x_continuous(labels=insert_minor(seq(1960, 2020, by=10), 9), breaks = 1960:2020)+
    ylab("Index value")+
    xlab("Date")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank())
  p
  #ggsave(p, filename="Figures/Fish.png", device = "png", width = 7.5, height=5, units="in")
  return(p)
  
}