DSCmapper<-function(Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta")){
  
  require(sf)
  require(rgdal)
  require(leaflet)
  require(tidyverse)
  require(readxl)
  require(lubridate)
  require(RColorBrewer)
  
  Deltaregions<-st_read("Data/Delta regions", quiet=T)
  Deltaregions<-as(Deltaregions, "Spatial")
  Deltaregions <- spTransform(Deltaregions, CRS("+init=epsg:4326"))
  
  Deltaregions <- Deltaregions[Deltaregions@data$Stratum %in% Regions, ]
  
  Deltaregions$Stratum<-factor(Deltaregions$Stratum, levels=Regions)
  
  pal<-colorFactor(brewer.pal(8, "Dark2"), Deltaregions$Stratum)
  
  p<-leaflet(data=Deltaregions)%>%
    addProviderTiles("Esri.WorldGrayCanvas")%>%
    setView(lng=-121.804075, lat=38.188039, zoom=10)%>%
    addPolygons(fillColor = ~pal(Stratum), smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.3, weight=1, color = "black")%>%
    addLegend(pal=pal, values = ~Stratum, position = "topleft", title = "Region")
  
  #mapview::mapshot(p, file="Figures/map.png", vheight=850, vwidth=600, zoom=8)
  return(p)
}