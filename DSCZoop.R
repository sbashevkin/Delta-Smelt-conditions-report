DSCZooper<-function(Data, Start_year=2002, End_year=2018, Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Lower Joaquin River", "Southern Delta"), Seasons="Fall"){
  

# Setup -------------------------------------------------------------------

  require(tidyverse)
  require(readxl)
  require(lubridate)
  require(RColorBrewer)
  
  insert_minor <- function(major_labs, n_minor) {labs <- 
    c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  labs[1:(length(labs)-n_minor)]}


# Load and combine data ---------------------------------------------------

  
  Zoopsum<-Data%>%
    filter(Season%in%Seasons & !(Region%in%c("Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island")))%>%
    droplevels()%>% 
    group_by(Region, Year, Taxa)%>%
    summarise(BPUE=mean(BPUE, na.rm=T))%>%
    ungroup()%>%
    droplevels()%>%
    mutate(missing="na")%>%
    complete(Year=Start_year:End_year, Region, fill=list(missing="n.d."))%>%
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
    geom_bar(data=Zoopsum, aes(x=Year, y=BPUE, fill=Taxa), stat="identity")+
    geom_bar(data=Zoopsum%>%filter(Year==End_year)%>%group_by(Region, Year)%>%summarise(BPUE=sum(BPUE)), aes(x=Year, y=BPUE), stat="identity", color="firebrick3", fill=NA, size=1)+
    geom_vline(data=Zoopmissing, aes(xintercept=Year), linetype=2)+
    scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year-1,End_year+1), expand=expand_scale(0,0))+
    scale_fill_brewer(type="div", palette="BrBG", guide=guide_legend(title=NULL, keyheight=0.8))+
    scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","), expand=expand_scale(0,0))+
    xlab("Date")+
    ylab(bquote(Biomass~"("*mu*g*") /"~m^3))+
    facet_wrap(~Region, scales="free_x")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20), legend.position = c(0.85,0.2), legend.text=element_text(size=8), legend.background=element_rect(fill="white", color="black"))
  
  #ggsave(p, filename="Figures/Zooplankton.png", device = "png", width = 7.5, height=4, units="in")
  
  return(p)
  
  }
