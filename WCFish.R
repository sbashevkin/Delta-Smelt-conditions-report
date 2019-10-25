WCFisher<-function(Start_year=2002, End_year=2018, EDSM_regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River")){
  
  #No DS caught in southern delta, so removing that region
  
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
    filter(Year>=Start_year)
  
  EDSM<-read_csv("Data/edsm_abund_estimates_2019-09-17.csv")%>%
    mutate(Stratum=recode(Stratum, "Cache Slough LI"="Cache Slough/Liberty Island", "Sac DW Ship Channel"="Sac Deep Water Shipping Channel",
                          "Lower Sacramento"="Lower Sacramento River", "Lower San Joaquin"="Lower Joaquin River"),
           Date=WeekStartDate+ceiling((WeekEndDate-WeekStartDate)/2))%>%
    select(Region=Stratum, Date, Abundance=nHat, lowCI, uppCI, Variance=nVar)%>% 
    mutate(MonthYear=floor_date(Date, unit = "month"))%>%
    filter(!is.na(Abundance))%>%
    mutate(Variance=replace_na(Variance, 0))%>%
    group_by(Region, MonthYear)%>%
    summarise(Abundance=mean(Abundance, na.rm=T), Abundance_CV=sqrt((1/n()^2)*sum(Variance))/Abundance)%>%
    ungroup()%>%
    mutate(l95=qlnorm(0.025, meanlog=log(Abundance/sqrt(1+Abundance_CV^2)), sdlog=log(1+Abundance_CV^2)),
           u95=qlnorm(0.975, meanlog=log(Abundance/sqrt(1+Abundance_CV^2)), sdlog=log(1+Abundance_CV^2)))%>%
    mutate(Abundance_l=log10(Abundance+1),
           l95_l=log10(l95),
           u95_l=log10(u95))%>%
    filter(Region%in%EDSM_regions)%>%
    mutate(missing="na")%>%
    complete(MonthYear, Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))%>%
    mutate(Region=factor(Region, levels=EDSM_regions))
  
  EDSMmissing<-EDSM%>%
    filter(missing=="n.d.")%>%
    select(MonthYear, Region)
  
  EDSM<-EDSM%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  
  # Add regions and summarise -------------------------------------------------------------
  
  
  # Plot --------------------------------------------------------------------
  
  p<-list()
  
  p$IEP<-ggplot()+
    geom_line(data=IEP_Indices, aes(x=Year, y=Index, color=Source), size=1)+
    geom_point(data=IEP_Indices, aes(x=Year, y=Index, color=Source))+
    geom_point(data=filter(IEP_Indices, Year==End_year), aes(x=Year, y=Index, color=Source), size=3, color="firebrick3")+
    coord_cartesian(expand=0)+
    facet_grid(Source~., scales = "free_y")+
    scale_color_brewer(type="div", palette="RdYlBu", guide="none")+
    scale_x_continuous(labels=insert_minor(seq(1990, 2020, by=5), 4), breaks = 1990:2020)+
    ylab("Index value")+
    xlab("Date")+
    ggtitle("IEP Delta smelt index values")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20))
  
  p$EDSM<-ggplot()+
    geom_line(data=EDSM, aes(x=MonthYear, y=Abundance_l), color="darkorchid4")+
    geom_errorbar(data=EDSM, aes(x=MonthYear, ymax=u95_l, ymin=l95_l))+
    geom_point(data=filter(EDSM, year(MonthYear)!=End_year), aes(x=MonthYear, y=Abundance_l), color="darkorchid4")+
    geom_point(data=filter(EDSM, year(MonthYear)==End_year), aes(x=MonthYear, y=Abundance_l), color="firebrick3", size=2.3)+
    geom_vline(data=EDSMmissing, aes(xintercept=MonthYear), linetype=2)+
    coord_cartesian(ylim=c(2,5.7))+
    facet_wrap(~Region)+
    scale_x_date(date_labels = "%b %y")+
    scale_y_continuous(labels = function(x) format(10^x, scientific=F, big.mark=","))+
    ylab("Delta Smelt abundance")+
    xlab("Date")+
    ggtitle("EDSM Delta Smelt Abundance")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20))
  
  
  
  
  ggsave(p$IEP, filename="Figures/IEP Fish.png", device = "png", width = 7.5, height=4, units="in")
  ggsave(p$EDSM, filename="Figures/EDSM Fish.png", device = "png", width = 7.5, height=4, units="in")
  return(p)
  
}