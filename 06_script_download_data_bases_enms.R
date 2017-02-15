### script download of data bases for enm ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 08/02/2017

###-----------------------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
memory.limit(size = 10000000000000) 

# install and require packages
# install.packages(c("downloader", "xml2", "rvest", "repmis"), dep = T)

library(downloader)
library(xml2)
library(rvest)
library(repmis)

###-----------------------------------------------------------------------------------------###

### bioclim

### 1. current ###

# directory
setwd("C:/Users/leec/Documents/MEGA/enm/02_scripts") # define directory to store the zips

# list of url
url <- "http://www.worldclim.org/current"

pg <- read_html(url)
pg

link <- html_attr(html_nodes(pg, "a"), "href")
link

zip <- link[grep("bil.zip", link)]
zip

names <- as.character(zip)
names

names.s <- sub("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/", "", names)
names.s

# download
for(i in 1:length(zip)){
  download(zip[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "present")}




### 2. past ###

# list of url
url <- "http://www.worldclim.org/paleo-climate1#com"

pg <- read_html(url)
pg

link <- html_attr(html_nodes(pg, "a"), "href")
link

zip <- link[grep(".zip", link)]
zip


# 2.1 mid
# directory
setwd("")

mid <- link[grep("mid", link)]
mid

names <- as.character(mid)
names

names.s <- sub("http://biogeo.ucdavis.edu/data/climate/cmip5/mid/", "", names)
names.s

# download
for(i in 1:length(mid)){
  download(mid[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "past_mid")}


# 2.2 lgm
# directory
setwd("") 

lgm <- link[grep("lgm", link)]
lgm

names <- as.character(lgm)
names

names.s <- sub("http://biogeo.ucdavis.edu/data/climate/cmip5/lgm/", "", names)
names.s

# download
for(i in 1:length(lgm)){
  download(lgm[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "past_lgm")}


# 2.3 lig
# directory
setwd("")

lig <- link[grep("lig", link)]
lig

names <- as.character(lig)
names

names.s <- sub("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/pst/lig/", "", names)
names.s

# download
for(i in 1:length(lig)){
  download(lig[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "past_lig")}



### 3. future ###

# 3.1 10 m

# list of url
url <- "http://www.worldclim.org/cmip5_10m"

pg <- read_html(url)
pg

link <- html_attr(html_nodes(pg, "a"), "href")
link

zip <- link[grep(".zip", link)]
zip


# 3.1.1 rcp26
# directory
setwd("")

rcp26 <- link[grep("26", link)]
rcp26

names <- as.character(rcp26)
names

names.s <- sub("http://biogeo.ucdavis.edu/data/climate/cmip5/10m/", "", names)
names.s

# download
for(i in 1:length(rcp26)){
  download(rcp26[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = "50.zip")
list

for(i in list){
  unzip(i, exdir = "10m_2050_rcp26")}

list <- list.files(patt = "70.zip")
list

for(i in list){
  unzip(i, exdir = "10m_2070_rcp26")}

###-----------------------------------------------------------------------------------------###

### chelsa

# directory
setwd("")

# list of url
url <- "https://cloud.s3it.uzh.ch:8080/v1/AUTH_5218a3a69ebf4a059c5a95889c5ee56e/CHELSA/"
url

pg <- read_html(url)
pg

list <- as.list(html_attr(html_nodes(pg, "a"), "href"))
list

# download
for(i in list[i]){
  download(paste0("https://cloud.s3it.uzh.ch:8080/v1/AUTH_5218a3a69ebf4a059c5a95889c5ee56e/CHELSA/", i), 
  paste0(i), mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "chelsa")}


###-----------------------------------------------------------------------------------------###

### earthenv

# 1. Global Habitat Heterogeneity

# directory
setwd("")

# list of url
url <- "http://www.earthenv.org/texture"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

tif <- link[grep(".tif", link)]
tif

names <- as.character(tif)
names

names.s <- sub("http://data.earthenv.org/habitat_heterogeneity/", "", names)
names.s

# download
for(i in 1:length(zip)){
  download(zip[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "present")}

# download
for(i in list[i]){
  download(i, paste0(i), mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "chelsa")}



# 2. Global 1-km Consensus Land Cover

# directory
setwd("")

# list of url
url <- "http://www.earthenv.org/landcover"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

tif <- link[grep(".tif", link)]
tif

names <- as.character(tif)
names

names.s <- sub("http://data.earthenv.org/habitat_heterogeneity/", "", names)
names.s

# download
for(i in 1:length(zip)){
  download(zip[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "present")}

# download
for(i in list[i]){
  download(i, paste0(i), mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "chelsa")}



# 3. Global 1-km Cloud Cover

# directory
setwd("")

# list of url
url <- "http://www.earthenv.org/cloud"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

tif <- link[grep(".tif", link)]
tif

names <- as.character(tif)
names

names.s <- sub("http://data.earthenv.org/cloud/", "", names)
names.s

# download
for(i in 1:length(zip)){
  download(zip[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "cloud")}


# 4. Near-global environmental information for freshwater ecosystems in 1km resolution

# directory
setwd("")

# list of url
url <- "http://www.earthenv.org/streams"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

nc <- link[grep("//data", link)]
nc

names <- as.character(tif)
names

names.s <- sub("http://data.earthenv.org/habitat_heterogeneity/", "", names)
names.s

# download
for(i in 1:length(zip)){
  download(zip[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "present")}

# download
for(i in list[i]){
  download(i, paste0(i), mode = "wb")} 

###-----------------------------------------------------------------------------------------###

### topodata

# directory
setwd("C:/Users/leec/Documents/MEGA/enm/02_scripts") # define directory to store the zips

# list of url
url <- "http://www.dsr.inpe.br/topodata/data/geotiff/"

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

# altitude
alt <- link[grep("_ZN", link)]
alt

# declividade
dec <- link[grep("_SN", link)]
dec

# orientacao
ori <- link[grep("_ON", link)]
ori


# download
for(i in 1:length(zip)){
  download(zip[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "present")}



###-----------------------------------------------------------------------------------------###

### ecoclimate

# directory
setwd("")

# list of url
url <- "http://ecoclimate.org/downloads/"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

drop <- link[grep(".dropbox", link)]
drop

# list of dropbox
pg.d <- read_html(as.character(drop[1]))
pg.d

link <- as.list(html_attr(html_nodes(pg.d, "a"), "href"))
link

drop <- link[grep(".dropbox", link)]
drop[2]

dl_from_dropbox <- function(x, key){
                     require(RCurl)
                     bin <- getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x),
                                         ssl.verifypeer = FALSE)
                     con <- file(x, open = "wb")
                     writeBin(bin, con)
                     close(con)
                     message(noquote(paste(x, "read into", getwd())))}

# Example:
dl_from_dropbox(x = "bio # CCSM_piControl(1760).zip", key = "gtqnk0bjjvpczhe")

for(i in drop){
  pg <- source_data("https://www.dropbox.com/sh/gtqnk0bjjvpczhe/AAALANP4yB9UQUdqTCzgj2nka?dl=0")
  
  link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
  link



# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "chelsa")}


download("https://www.dropbox.com/sh/pd6q87brzq4a5aj/AAAjbnPgvaaiVSnDisnNFXvja?dl=0&preview=raw+%23baseline_Modern(1950-1999)%23+pr_CCSM_rcp26(2080-2100).txt", "test2.txt", mode = "wb")
 

