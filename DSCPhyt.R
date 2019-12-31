#MISSING STATION LAT LONGS FOR SOME STATIONS

DSCPhyter<-function(Data, Start_year=2002, End_year=2018, Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"), Seasons="Summer"){
  
  
  # Setup -------------------------------------------------------------------
  
  require(tidyverse)
  require(lubridate)
  require(RColorBrewer)
  
  insert_minor <- function(major_labs, n_minor) {labs <- 
    c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  labs[1:(length(labs)-n_minor)]}

  # Load and combine data ---------------------------------------------------
  
  Phytosum<-Data%>%
    filter(Season%in%Seasons)%>%
    droplevels()%>%
    group_by(Region, Year, Taxa)%>%
    summarise(CPUE=mean(CPUE, na.rm=T))%>%
    ungroup()%>%
    filter(Year>=Start_year)%>%
    mutate(Taxa=factor(Taxa, levels=c("Diatoms", "Cryptophytes", "Green Algae", "Chrysophytes", "Cyanobacteria", "Dinoflagellates", "Other flagellates", "Other taxa")),
           missing="na",
           Region=as.character(Region))%>%
    complete(Year=Start_year:End_year, Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))%>%
    mutate(Region=factor(Region, levels=Regions))
  
  Phytomissing<-Phytosum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Phytosum<-Phytosum%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  Peak<-tibble(Region=filter(Phytosum, Taxa!="Cyanobacteria")$Region[which.max(filter(Phytosum, Taxa!="Cyanobacteria")$CPUE)], Year=filter(Phytosum, Taxa!="Cyanobacteria")$Year[which.max(filter(Phytosum, Taxa!="Cyanobacteria")$CPUE)], label=paste0("Peak CPUE: ", format(round(max(filter(Phytosum, Taxa!="Cyanobacteria")$CPUE)), big.mark=",")))%>%
    mutate(Region=factor(Region, levels=Regions))
  
  
  # Plot --------------------------------------------------------------------
  
  pphyto<-ggplot()+
    geom_bar(data=filter(Phytosum, Taxa!="Cyanobacteria"), aes(x=Year, y=CPUE, fill=Taxa), stat="identity", alpha=1)+
    {if(End_year%in%unique(Phytosum$Year)){
      geom_bar(data=Phytosum%>%filter(Taxa!="Cyanobacteria" & Year==End_year)%>%group_by(Region, Year)%>%summarise(CPUE=sum(CPUE))%>%ungroup()%>%droplevels(), aes(x=Year, y=CPUE), stat="identity", color="firebrick3", fill=NA, size=1)
    }}+
    geom_vline(data=Phytomissing, aes(xintercept=Year), linetype=2)+
    geom_label(data=Peak, aes(x=Year-2, y=30000, label=label), size=3)+
    scale_fill_brewer(type="div", palette="BrBG", guide=guide_legend(keyheight=0.6, title=NULL), direction=-1)+
    xlab("Date")+
    ylab("Number of cells, colonies, or filaments / ml")+
    coord_cartesian(expand=0, ylim=c(0,35000))+
    scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year-1,End_year+1), expand=expand_scale(0,0))+
    scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","))+
    facet_wrap(~Region, scales="free_x")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20), legend.position = c(0.85,0.2), legend.background=element_rect(fill="white", color="black"), legend.text = element_text(size=8))

  pcyano<-ggplot()+
    geom_bar(data=filter(Phytosum, Taxa=="Cyanobacteria" & Year!=End_year), aes(x=Year, y=CPUE), fill="chartreuse4", stat="identity", alpha=0.7)+
  geom_bar(data=filter(Phytosum, Taxa=="Cyanobacteria" & Year==End_year), aes(x=Year, y=CPUE), fill="chartreuse4", stat="identity", alpha=1)+
    geom_vline(data=Phytomissing, aes(xintercept=Year), linetype=2)+
    xlab("Date")+
    ylab("Number of cells, colonies, or filaments / ml")+
    scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year-1,End_year+1), expand=expand_scale(0,0))+
    scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","), expand=expand_scale(0,0))+
    facet_wrap(~Region, scales="free_x")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20))

  #ggsave(pphyto, filename="Figures/Phytoplankton.png", device = "png", width = 7.5, height=4, units="in")
  #ggsave(pcyano, filename="Figures/Cyanobacteria.png", device = "png", width = 7.5, height=4, units="in")
  return(list(Phytoplankton=pphyto, Cyanobacteria=pcyano))
  
}
