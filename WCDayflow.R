WCDayFlower<-function(Download=F){
  
  
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
    filter(year(Date)>1991)%>%
    mutate(MonthYear=floor_date(Date, unit = "month"))%>%
    group_by(MonthYear)%>%
    summarise(OUT=mean(OUT, na.rm=T), X2=mean(X2, na.rm=T))
  
  
  # Plot data ---------------------------------------------------------------
  
  p<-list()
  p$X2<-ggplot()+
    geom_line(data=DF, aes(x=MonthYear, y=X2))+
    coord_cartesian(expand=0)+
    ylab("X2 (km)")+
    xlab("Date")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank())
  
  p$Out<-ggplot()+
    geom_line(data=DF, aes(x=MonthYear, y=OUT))+
    coord_cartesian(expand=0)+
    ylab(bquote("Delta"~outflow~ft^3*"/s"))+
    xlab("Date")+
    theme_bw()+
    theme(panel.grid=element_blank(), strip.background = element_blank())
  return(p)
}