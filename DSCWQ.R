DSCWQer<-function(Data, Start_year=2002, End_year=2018, Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"), Temp_season="Summer", Secchi_season="Fall", Salinity_season="Fall", Chl_season="Summer", Micro_season="Summer"){
  
  
  # Setup -------------------------------------------------------------------
  
  require(tidyverse)
  require(lubridate)
  require(RColorBrewer)
  
  insert_minor <- function(major_labs, n_minor) {labs <- 
    c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  labs[1:(length(labs)-n_minor)]}
  
  
  WQsum <- Data
  
  Secchisum<-WQsum%>%
    select(Month, Region, Secchi, Year, Season)%>%
    filter(!is.na(Secchi))%>%
    filter(Season%in%Secchi_season)%>%
    droplevels()%>%
    group_by(Region, Year)%>%
    summarise(SD=sd(Secchi, na.rm=T), Secchi=mean(Secchi, na.rm=T))%>%
    ungroup()%>%
    mutate(missing="na")%>%
    complete(Year=Start_year:(End_year), Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))%>%
    mutate(Region=factor(Region, levels=Regions))
  
  Secchimissing<-Secchisum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Secchisum<-Secchisum%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  Salsum<-WQsum%>%
    select(Month, Region, Salinity, Year, Season)%>%
    filter(!is.na(Salinity))%>%
    filter(Season%in%Salinity_season)%>%
    droplevels()%>%
    group_by(Region, Year)%>%
    summarise(SD=sd(Salinity, na.rm=T), Salinity=mean(Salinity, na.rm=T))%>%
    ungroup()%>%
    mutate(missing="na")%>%
    complete(Year=Start_year:(End_year), Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))%>%
    mutate(Region=factor(Region, levels=Regions))
  
  Salmissing<-Salsum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Salsum<-Salsum%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  Chlsum<-WQsum%>%
    select(Month, Region, Chlorophyll, Year, Season)%>%
    filter(!is.na(Chlorophyll))%>%
    filter(Season%in%Chl_season)%>%
    droplevels()%>%
    group_by(Region, Year)%>%
    summarise(SD=sd(Chlorophyll, na.rm=T), Chlorophyll=mean(Chlorophyll, na.rm=T))%>%
    ungroup()%>%
    mutate(missing="na")%>%
    complete(Year=Start_year:(End_year), Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"))%>%
    mutate(Region=factor(Region, levels=Regions))
  
  Chlmissing<-Chlsum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Chlsum<-Chlsum%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  Microsum<-WQsum%>%
    select(Month, Region, Microcystis, Year, Season)%>%
    filter(!is.na(Microcystis))%>%
    filter(Season%in%Micro_season)%>%
    droplevels()%>%
    group_by(Region, Year)%>%
    summarise(N_Microcystis=length(which(!is.na(Microcystis))), Microcystis1=length(which(Microcystis==1))/N_Microcystis, Microcystis2=length(which(Microcystis==2))/N_Microcystis, Microcystis3=length(which(Microcystis==3))/N_Microcystis, Microcystis4=length(which(Microcystis==4))/N_Microcystis, Microcystis5=length(which(Microcystis==5))/N_Microcystis)%>%
    ungroup()%>%
    filter(N_Microcystis>0)%>%
    gather(key="Severity", value="Frequency", Microcystis1, Microcystis2, Microcystis3, Microcystis4, Microcystis5)%>%
    mutate(Severity=recode(Severity, "Microcystis1"="Absent", "Microcystis2"="Low", "Microcystis3"="Medium", "Microcystis4"="High", "Microcystis5"="Very high"))%>%
    mutate(missing="na")%>%
    complete(Year=Start_year:(End_year), Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"),
           Region=factor(Region, levels=Regions))
  
  Micromissing<-Microsum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Microsum<-Microsum%>%
    filter(is.na(missing))%>%
    select(-missing)%>%
    filter(Severity!="Absent")%>%
    mutate(Severity=factor(Severity, levels=c("Very high", "High", "Medium", "Low", "Absent")))
  
  Tempsum<-WQsum%>%
    select(Month, Region, Temperature, Year, Season)%>%
    filter(Season%in%Temp_season & !is.na(Temperature))%>%
    droplevels()%>%
    group_by(Year, Region)%>%
    mutate(Nmonths = n_distinct(Month))%>%
    filter(Nmonths>=3)%>%
    summarise(SD=sd(Temperature, na.rm=T), Temperature=mean(Temperature, na.rm=T))%>%
    ungroup()%>%
    mutate(missing="na")%>%
    complete(Year=Start_year:(End_year), Region, fill=list(missing="n.d."))%>%
    mutate(missing=na_if(missing, "na"),
           Region=factor(Region, levels=Regions))
  
  Tempmissing<-Tempsum%>%
    filter(missing=="n.d.")%>%
    select(Year, Region)
  
  Tempsum<-Tempsum%>%
    filter(is.na(missing))%>%
    select(-missing)
  
  Salrange<-Salsum%>%
    filter(!is.na(Salinity))%>%
    group_by(Region)%>%
    summarise(Salrange=paste0("min: ", round(min(Salinity), 2), ", max: ", round(max(Salinity), 2)))
  
  Chlrange<-tibble(xmin=min(Chlsum$Year), xmax=max(Chlsum$Year), Region=unique(as.character(filter(Chlsum, !is.na(Chlorophyll))$Region)))
  Chlrange<-Chlrange%>%
    mutate(ymin=0, ymax=10, Quality="Bad")%>%
    bind_rows(Chlrange%>%
                mutate(ymin=10, ymax=max(Chlsum$Chlorophyll, na.rm=T), Quality="Good"))%>%
    mutate(Region=factor(Region, levels=Regions)) 
  
  # Plot --------------------------------------------------------------------
  
  plotWQ<-function(Data, Parameter, ylabel){
    Parameter<-enquo(Parameter)
    ggplot()+
      geom_line(data=Data, aes(x=Year, y=!!Parameter), color="firebrick3")+
      geom_ribbon(data=Data, aes(x=Year, ymin=!!Parameter-SD, ymax=!!Parameter+SD), alpha=0.4, fill="gray")+
      geom_point(data=filter(Data, Year==End_year), aes(x=Year, y=!!Parameter), color="firebrick3", size=3)+
      scale_y_continuous(expand = expand_scale(0,0))+
      scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year,End_year+1), expand=expand_scale(0,0))+
      facet_wrap(~Region, scales = "free_x")+
      ylab(ylabel)+
      xlab("Date")+
      theme_bw()+
      theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20))
  }
  
  TempShades<-expand.grid(Region=unique(Tempsum$Region), Quality=c("Good", "Marginal", "Bad"))%>%
    mutate(xmin=Start_year,
           xmax=End_year+1,
           ymin=case_when(
             Quality=="Good" ~ min(Tempsum$Temperature-Tempsum$SD),
             Quality=="Marginal" ~ 20,
             Quality=="Bad" ~ 22
           ),
           ymax=case_when(
             Quality=="Good" ~ 20,
             Quality=="Marginal" ~ 22,
             Quality=="Bad" ~ max(Tempsum$Temperature+Tempsum$SD)
           ))
  
  
  pTemp<-plotWQ(Tempsum, Temperature, bquote(Temperature~"("*degree*c*")"))+
    geom_vline(data=Tempmissing, aes(xintercept=Year), linetype=2)+
    geom_rect(data=TempShades, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=Quality), alpha=0.2)+
    scale_fill_brewer(type="div", palette = "RdYlBu", direction=-1)+
    theme(legend.position = "none")
  
  pSecchi<-plotWQ(Secchisum, Secchi, "Secchi depth (cm)")+
    geom_vline(data=Secchimissing, aes(xintercept=Year), linetype=2)
  
  pChla<-plotWQ(Chlsum, Chlorophyll, bquote(Chlorophyll~a~"("*mu*g*"/L)"))+
    geom_vline(data=Chlmissing, aes(xintercept=Year), linetype=2)
  
  pSal<-plotWQ(Salsum, Salinity, "Salinity")+
    geom_label(data=Salrange, aes(x=2006, y=15, label=Salrange), alpha=0.5, size=2.5)+
    geom_vline(data=Salmissing, aes(xintercept=Year), linetype=2)+
    coord_cartesian(ylim = c(0,max(Salsum$Salinity+Salsum$SD)))
  
  pMicro<-ggplot()+
    geom_bar(data=Microsum, aes(x=Year, y=Frequency, fill=Severity), stat="identity")+
    geom_bar(data=tibble(End_year), aes(x=End_year, y=1), stat="identity", color="firebrick3", fill=NA, size=1)+
    geom_vline(data=Micromissing, aes(xintercept=Year), linetype=2)+
    scale_fill_brewer(type="div", palette="RdYlBu", guide=guide_legend(keyheight=0.8, title=NULL, direction="horizontal", label.position="top", reverse=TRUE))+
    scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year,End_year+1), expand=expand_scale(0,0))+
    scale_y_continuous(expand=expand_scale(0,0))+
    facet_wrap(~Region, scales = "free_x")+
    ylab("Relative frequency")+
    xlab("Date")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20), legend.position=c(0.63, 0.13), legend.background=element_rect(fill="white", color="black"), legend.text = element_text(size=8))
  
  Datacleaner<-function(Data, Parameter){
    Parameter <- enquo(Parameter)
    out <- Data%>%
      mutate(SD=round(SD, 2),
             !!Parameter := round(!!Parameter, 2))%>%
      select(Year, Region, `Standard deviation` = SD, !!Parameter)
    return(out)
  }
  
  Tempdata <- Datacleaner(Tempsum, Temperature)
  Secchidata <- Datacleaner(Secchisum, Secchi)
  Saldata <- Datacleaner(Salsum, Salinity)
  Chldata <- Datacleaner(Chlsum, Chlorophyll)
  Microdata <- Microsum%>%
    mutate(Frequency = round(Frequency, 2))%>%
    rename(Samples = N_Microcystis, `Relative frequency`=Frequency)
  
  plots<-list(Temperature = list(Plot = pTemp, Data = Tempdata), 
              Secchi = list(Plot = pSecchi, Data = Secchidata),
              Salinity = list(Plot = pSal, Data = Saldata),
              Chlorophyll = list(Plot = pChla, Data = Chldata),
              Microcystis = list(Plot = pMicro, Data = Microdata))
  
  #sapply(1:length(plots), function(x) ggsave(plots[[x]], filename=paste0("Figures/", names(plots[x]), ".png"), device = "png", width = 7.5, height=4, units="in"))
  
  return(plots)
  
}