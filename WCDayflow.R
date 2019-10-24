WCDayFlower<-function(Start_year=2002, End_year=2018){
  
  
  # Setup -------------------------------------------------------------------
  
  require(tidyverse)
  require(lubridate)
  require(RColorBrewer)
  
  insert_minor <- function(major_labs, n_minor) {labs <- 
    c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  labs[1:(length(labs)-n_minor)]}
  

  # Load and summarise data ---------------------------------------------------------------
  
  DF<-read_csv("Data/Dayflow1997 2018.csv", col_types = "ddcdddddddddddddddddddddddddd")%>%
    mutate(Date=parse_date_time(Date, "%d-%b-%y"))%>%
    select(Date, OUT, X2)%>%
    bind_rows(read_csv("Data/Dayflow1984 1996.csv", col_types = "cddcddddddddddddddddddddddddd")%>%
                mutate(DATE=parse_date_time(DATE, "%d-%b-%y"))%>%
                select(Date=DATE, OUT))%>%
    filter(year(Date)>=Start_year)%>%
    mutate(MonthYear=floor_date(Date, unit = "month"))%>%
    group_by(MonthYear)%>%
    summarise(OUT=mean(OUT, na.rm=T), X2=mean(X2, na.rm=T))
  
  
  # Plot data ---------------------------------------------------------------
  
  Fallshade<-DF%>%
    mutate(Year=year(MonthYear))%>%
    select(Year)%>%
    distinct()%>%
    mutate(September=parse_date_time(paste0("09/", Year), "%m/%Y"),
           November=parse_date_time(paste0("11/", Year), "%m/%Y"))%>%
    mutate(X2min=min(DF$X2),
           X2max=max(DF$X2),
           OUTmin=min(DF$OUT),
           OUTmax=max(DF$OUT))
  
  p<-list()
  p$X2<-ggplot()+
    geom_line(data=DF, aes(x=MonthYear, y=X2), color="dodgerblue4")+
    geom_line(data=filter(DF, year(MonthYear)==End_year), aes(x=MonthYear, y=X2), color="firebrick3", size=2)+
    geom_rect(data=Fallshade, aes(xmin=September, xmax=November, ymin=X2min, ymax=X2max), alpha=0.4, fill="darkorange1")+
    coord_cartesian(expand=0)+
    ylab("X2 (km)")+
    xlab("Date")+
    ggtitle("X2")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20))
  
  p$Out<-ggplot()+
    geom_line(data=DF, aes(x=MonthYear, y=OUT), color="dodgerblue4")+
    geom_line(data=filter(DF, year(MonthYear)==End_year), aes(x=MonthYear, y=OUT), color="firebrick3", size=2)+
    geom_rect(data=Fallshade, aes(xmin=September, xmax=November, ymin=OUTmin, ymax=OUTmax), alpha=0.4, fill="darkorange1")+
    coord_cartesian(expand=0)+
    scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","))+
    ylab(bquote("Delta"~outflow~"("*ft^3*"/s)"))+
    xlab("Date")+
    ggtitle("Delta outflow")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size=20))
  
  ggsave(p$Out, filename="Figures/Outflow.png", device = "png", width = 7.5, height=4, units="in")
  ggsave(p$X2, filename="Figures/X2.png", device = "png", width = 7.5, height=4, units="in")
  return(p)
}