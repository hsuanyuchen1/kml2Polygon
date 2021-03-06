library(sf)
#library(tmap)
library(rgdal)
library(dplyr)
library(xml2)
library(XML)
library(fpc)
source("D:/Test/clusterKml2Polygon/readKML.r")


# tfile <- list.files(zipfileDir, full.names = T, recursive = T, pattern = "zip")
# tfile <- tfile[!grepl("TAB", tfile)]

#tFileName <- tfile[1]

kml2Tab = function(tempKml, eps, minPts, poorRsrpThr, topTraffic){
  ######config parameters
  tabDsn <- "//192.168.1.12/e$/rf/polygonBackup/Backup/"
  fname <- strsplit(tempKml, split = "/") %>% unlist()
  fname <- fname[9]
  # eps = 0.0014
  # minPts = 3
  # poorRsrpThr = 20
  # topTraffic = 0.6
  # # tempFileDir <- "//192.168.1.12/e$/rf/kml2tab"
  # tabDsn <- "//192.168.1.12/f$/FTP Data/Cellrefs_and_NBR/Nokia/Cell Trace Reports/PoorCoveragePolygons_20200304/TAB/"
  #####################################################
  # fileName <- tools::file_path_sans_ext(basename(tFileName))
  # tempUnzipDir <- paste0(tempFileDir,"/", fileName)
  # 
  # cat(fileName, "\n")
  # 
  # unzip(tFileName, exdir = tempUnzipDir)
  
  # tempKml <- list.files(fileName, 
  #                       recursive = T, 
  #                       full.names = T, 
  #                       pattern = ".kml")
  
  #lyr <- ogrListLayers(tempKml)
  data <- readKML(tempKml, layer = "CE_FCN_PoorRSRP__RLP_over_PAST") %>% 
    st_as_sf()
  colnames(data)[3] <- "PoorRSRP"
  data$PoorRSRP <- as.numeric(as.character(data$PoorRSRP))
  data <- data[data$PoorRSRP > poorRsrpThr,]
  
  data2 <- readKML(tempKml, layer = "CE_FCN_TotalErlangsLTE_RLP_ove") %>% 
    st_as_sf()
  colnames(data2)[3] <- "TotalErlang"
  data2$TotalErlang <- as.numeric(as.character(data2$TotalErlang))
  #Urban: 0.6/suburban: 0.4/rural:0.2
  data2 <- data2[data2$TotalErlang > quantile(data2$TotalErlang, topTraffic, na.rm = T),]
  
  data.f <- data[data2, , op = st_intersects]
  
  data.f.point <- st_centroid(data.f) 
  
  
  data.f.point <- do.call(rbind, st_geometry(data.f.point)) %>% 
    as_tibble() %>% setNames(c("lon", "lat"))
  
  tcluster <- dbscan(cbind(data.f.point$lat, data.f.point$lon), 
                     eps = eps, MinPts = minPts)
  data.f.cluster <- cbind(data.f, tcluster$cluster)
  data.union <- st_buffer(data.f.cluster[data.f.cluster$tcluster.cluster > 0,], 
                          dist = 0.00001,
                          endCapStyle = "SQUARE") %>% 
    st_union()
  
  st_write(data.union, 
           dsn = paste0(tabDsn, fname,"minPts", minPts, 
                        "eps", eps/0.0014*100, "topTraffic", topTraffic), 
           driver = "MapInfo File", delete_dsn = T)
  
  #delete files under tempfile directory
  #unlink(tempFileDir, recursive = T)
  #delete the zip file from original directory
  #unlink(tFileName)
}

#===main===

#zipfileDir <- "//192.168.1.12/f$/FTP Data/Cellrefs_and_NBR/Nokia/Cell Trace Reports/PoorCoveragePolygons_20200304"
fileName <- list.files("//192.168.1.12/e$/rf/polygonBackup/Backup/",
                       recursive = T,
                       pattern = ".kml",
                       full.names = T)

eps = 0.0014
minPts = c(3, 10, 20)
poorRsrpThr = 20
topTraffic = c0.6


for (temp in fileName) {
  kml2Tab(temp, eps, minPts, poorRsrpThr, topTraffic)
  
}

