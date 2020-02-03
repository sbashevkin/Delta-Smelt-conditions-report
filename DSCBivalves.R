DSCBivalver<-function(Data, Start_year=2002, End_year=2018, Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"), Seasons="Fall"){
  
  
  # Setup -------------------------------------------------------------------
  
  require(tidyverse)
  require(lubridate)
  require(RColorBrewer)
  
  insert_minor <- function(major_labs, n_minor) {labs <- 
    c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  labs[1:(length(labs)-n_minor)]}

  
  # Load and combine data ---------------------------------------------------
  
  Bivsum<-Data%>%
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
    ylab(bquote(Number~of~clams~"/"~m^2))+
    scale_fill_manual(values=c("#d8b365", "#5ab4ac"), guide=guide_legend(title=NULL))+
    facet_wrap(~Region, scales="free_x")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), legend.position = c(0.85, 0.2), plot.title = element_text(hjust = 0.5, size=20), legend.background=element_rect(fill="white", color="black"))
  
  Data_out <- Bivsum%>%
    mutate(CPUE=round(CPUE,2))%>%
    rename(`Count per unit effort` = CPUE)
  
  #ggsave(p, filename="Figures/Bivalves.png", device = "png", width = 7.5, height=4, units="in")
  return(list(Plot = p, Data = Data_out))
  
}
