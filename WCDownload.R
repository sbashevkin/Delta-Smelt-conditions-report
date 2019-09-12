WCdatadownloader<-function(Sources=c("FMWT", "STN", "EDSM 20mm", "EDSM KDTR", "EMP Zoop CB", "EMP Zoop Pump", "EMP Phytoplankton", "Dayflow 1997-2018", "Dayflow 1984-1996")){
  
  
  #FMWT
  if("FMWT"%in%Sources){
  temp <- tempfile()
  download.file("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%201967-2018%20Catch%20Matrix_updated.zip", temp)
  unzip (temp, exdir="C:/Users/sbashevkin/Documents/Water-conditions-report/Data")
  unlink(temp)
  }
  
  #STN
  if("STN"%in%Sources){
  download.file("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/TNS%20MS%20Access%20Data/TNS%20data/Townet_Data_1959-2018.xlsx", "Data/Townet_Data_1959-2018.xlsx", mode="wb")
  }
  
  #EDSM 20mm
  if("EDSM 20mm"%in%Sources){
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.1&entityid=7c76313e27c4ef4685e7fe016c1e4608", "Data/EDSM_20mm.csv", mode="wb")
  }
  
  #EDSM KDTR
  if("EDSM KDTR"%in%Sources){
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.1&entityid=93c01636cfc51919b8f363c7bfa829ca", "Data/EDSM_KDTR.csv", mode="wb")
  }
  
  #EMP Zoop CB
  if("EMP Zoop CB"%in%Sources){
  download.file("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2018CBMatrix.xlsx", 
                "Data/1972-2018CBMatrix.xlsx", mode="wb")
  }
  
  #EMP Zoop Pump
  if("EMP Zoop Pump"%in%Sources){
  download.file("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2018Pump Matrix.xlsx", 
                "Data/1972-2018Pump Matrix.xlsx", mode="wb")
  }
  
  #EMP Phytoplankton
  if("EMP Phytoplankton"%in%Sources){
  download.file("https://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/Phytoplankton_Algal_Type_Data_1975_-2016.csv", 
                "Data/Phytoplankton_Algal_Type_Data_1975_-2016.csv", mode="wb")
  }
  
  #Dayflow 1997-2018
  if("Dayflow 1997-2018"%in%Sources){
  download.file("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2018.csv", 
                "Data/Dayflow1997 2018.csv", mode="wb")
  }
  
  #Dayflow 1984-1996
  if("Dayflow 1984-1996"%in%Sources){
  download.file("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/wy1984-1996.csv", 
                "Data/Dayflow1984 1996.csv", mode="wb")
  }
}