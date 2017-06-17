### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 17/06/2017

###-----------------------------------------------------------------------------###
###                             ecoclimate                                      ###
###-----------------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(downloader, xml2, rvest)

###-----------------------------------------------------------------------------###

# directory
setwd("D:/environmental_data")
dir.create("ecoclimate")
setwd("ecoclimate")
gewd()

# url
url <- "http://ecoclimate.org/downloads/"
url




# download
for(i in an){
  url.bi <- paste0(url.an, j, ".tif")
  download(url.bi, paste0(j, "_", i, ".tif"), mode = "wb")}


###-----------------------------------------------------------------------------###
