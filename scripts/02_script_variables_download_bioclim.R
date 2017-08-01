### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 16/06/2017

###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(RCurl, rgdal, GSIF, raster, plotKML, XML, lattice, 
               downloader, xml2, rvest, aqp, soiltexture)

## GDAL paths:
if(.Platform$OS.type == "windows"){
  gdal.dir <- shortPathName("C:/Program files/GDAL")
  gdal_translate <- paste0(gdal.dir, "/gdal_translate.exe")
  gdalwarp <- paste0(gdal.dir, "/gdalwarp.exe") 
  gdalinfo <- paste0(gdal.dir, "/gdalinfo.exe")
} else {
  gdal_translate = "gdal_translate"
  gdalwarp = "gdalwarp"
  gdalinfo = "gdalinfo"
}

###-----------------------------------------------------------------------------###
###                               soilgrids
###-----------------------------------------------------------------------------###

# directory
setwd("D:/environmental_data")
dir.create("soilgrids")
setwd("soilgrids")
getwd()

###-----------------------------------------------------------------------------###

# resolution
re <- c("250m", "1km", "5km", "10km")
re

# links
ftp.km <- "ftp://ftp.soilgrids.org/data/aggregated/"

ftp.m <- "ftp://ftp.soilgrids.org/data/recent/"

# download
for(i in re){
  
  # ftp
  if(i != "250m"){
    ftp <- paste0(ftp.km, i, "/")
    
    # directory
    dir.create(i)
    setwd(i)
    
    # files
    filenames <- getURL(ftp, ftp.use.epsv = F, dirlistonly = T)
    filenames <- strsplit(filenames, "\r*\n")[[1]]
    
    # download
    for(j in filenames){
      try(download(paste0(ftp, j), j, mode = "wb"))}
    
    setwd("..")
    
    # ftp
  } else{
    
    ftp <- ftp.m
    
    # directory
    dir.create(i)
    setwd(i)
    
    # files
    filenames <- getURL(ftp, ftp.use.epsv = F, dirlistonly = T)
    filenames <- strsplit(filenames, "\r*\n")[[1]]
    
    # download
    for(k in filenames){
      try(download(paste0(ftp, k), k, mode = "wb"))}
  }}

###-----------------------------------------------------------------------------###
