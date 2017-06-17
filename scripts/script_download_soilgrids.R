### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 16/06/2017

###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

###-----------------------------------------------------------------------------###
###                               soilgrids
###-----------------------------------------------------------------------------###

# SoilGrids tutorial
# Reference: [http://gsif.isric.org/doku.php?id=wiki:tutorial_soilgrids]
# Tom.Hengl@isric.org

# install.packages(c("RCurl", "rgdal", "GSIF", "raster", "plotKML", "XML", 
#                   "lattice", "aqp", "soiltexture"), dep = T)

library(RCurl)
library(rgdal)
library(GSIF)
library(raster)
library(plotKML)
library(XML)
library(lattice)
library(aqp)
library(soiltexture)

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

# directory
setwd("D:/environmental_data")
dir.create("soilgrids")
setwd("soilgrids")
getwd()

###-----------------------------------------------------------------------------###

## 10 km
dir.create("10km")
setwd("10km")
getwd()

# ftp
ftp <- "ftp://ftp.soilgrids.org/data/aggregated/10km/"

filenames <- getURL(ftp, ftp.use.epsv = F, dirlistonly = T)

filenames <- strsplit(filenames, "\r*\n")[[1]]

filenames[1:5]

# download to a local directory:
for(i in filenames){
  try(download.file(paste0(ftp, i), i))}

###-----------------------------------------------------------------------------###

## 5 km
setwd("..")
dir.create("5km")
setwd("5km")
getwd()

# ftp
ftp <- "ftp://ftp.soilgrids.org/data/aggregated/5km/"

filenames <- getURL(ftp, ftp.use.epsv = F, dirlistonly = T)

filenames <- strsplit(filenames, "\r*\n")[[1]]

filenames[1:5]

# download to a local directory:
for(i in filenames){
  try(download.file(paste0(ftp, i), i))}

###-----------------------------------------------------------------------------###

## 1 km
setwd("..")
dir.create("1km")
setwd("1km")
getwd()

# ftp
ftp <- "ftp://ftp.soilgrids.org/data/aggregated/1km/"

filenames <- getURL(ftp, ftp.use.epsv = F, dirlistonly = T)

filenames <- strsplit(filenames, "\r*\n")[[1]]

filenames[1:5]

# download to a local directory:
for(i in filenames){
  try(download.file(paste0(ftp, i), i))}

###-----------------------------------------------------------------------------###

## 250 m
setwd("..")
dir.create("250m")
setwd("250m")
getwd()

# ftp
ftp <- "ftp://ftp.soilgrids.org/data/recent/"

filenames <- getURL(ftp, ftp.use.epsv = F, dirlistonly = T)

filenames <- strsplit(filenames, "\r*\n")[[1]]

filenames[1:5]

# download to a local directory:
for(i in filenames){
  try(download.file(paste0(ftp, i), i))}

###-----------------------------------------------------------------------------###

