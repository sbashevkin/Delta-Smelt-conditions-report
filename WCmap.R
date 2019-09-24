WCmapper<-function(){
  require(sf)
  require(rgdal)
  require(raster)
  require(tidyverse)
  require(readxl)
  require(lubridate)
  require(RColorBrewer)
  
  Deltaregions<-st_read("Data/Delta regions", quiet=T)
  Deltaregions<-as(Deltaregions, "Spatial")
  Deltaregions <- spTransform(Deltaregions, CRS("+init=epsg:4326"))
  
  centers <- data.frame(gCentroid(Deltaregions, byid = TRUE))
  centers$region <- Deltaregions$Stratum
  
  p<-leaflet(data=Deltaregions)%>%
    addProviderTiles("Esri.WorldGrayCanvas")%>%setView(lng=-121.774075, lat=38.170039, zoom=11)%>%
    addPolygons(fill = F, color = ~colorFactor(brewer.pal(8, "Dark2"), Stratum)(Stratum), smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.3, weight=5)%>%
    addLabelOnlyMarkers(data = centers, lng = ~x, lat = ~y, label = ~region, labelOptions = labelOptions(noHide = TRUE, textOnly = T, direction="right", textsize = "40px"))
  
  mapview::mapshot(p, file="Figures/map.png", vheight=1900, vwidth=1900)
}