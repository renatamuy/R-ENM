### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 08/02/2017

###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(downloader, xml2, rvest)


###-----------------------------------------------------------------------------###
###                                 chelsa
###-----------------------------------------------------------------------------###

## v 1.0
# directory
setwd("D:/environmental_data/chelsa")
dir.create("v1_0")
setwd("v1_0")
getwd()

# list of url
url <- "http://www.wsl.ch/lud/chelsa/archive/version1.0/"
url

pg <- read_html(url)
pg

list <- grep("CHELSA", html_attr(html_nodes(pg, "a"), "href"), value = T)
list

# download
for(i in list){
  download(paste0(url, i), paste0(i), mode = "wb")}


###------------------------------------------------------------------------------###

## v 1.1
# directory
setwd("..")
dir.create("v1_1")
setwd("v1_1")
getwd()

# list of url
url <- "http://www.wsl.ch/lud/chelsa/archive/version1.1/"
url

pg <- read_html(url)
pg

list <- grep("CHELSA", html_attr(html_nodes(pg, "a"), "href"), value = T)
list

# download
for(i in list){
  download(paste0(url, i), paste0(i), mode = "wb")}


###------------------------------------------------------------------------------###

### v1.2
# directory
setwd("..")
dir.create("v1_2b")
setwd("v1_2b")
getwd()

# lists
ce <- c("bioclim/", "climatologies/")
ce

ty <- c("float/", "integer/")
ty

cl <- c("prec/", "temp/")
cl

te <- c("temp/", "tmax/", "tmin/")
te

url <- "http://www.wsl.ch/lud/chelsa/data/"
url

# download
for(i in ce){
  
  if(i == "bioclim/"){
  
    dir.create(sub("/", "", i))
    setwd(sub("/", "", i))
    
    for(j in ty){
      
      dir.create(sub("/", "", j))
      setwd(sub("/", "", j))
      
      url.e <- paste0(url, i, j)
      
      pg <- read_html(url.e)

      list <- grep(".7z", html_attr(html_nodes(pg, "a"), "href"), value = T)
      
      print(list)

      #for(k in list){
        #download(paste0(url.e, k), paste0(k), mode = "wb")}
      
      setwd("..")}
    
  setwd("..")}

  
  if(i == "climatologies/"){
    
    dir.create(sub("/", "", i))
    setwd(sub("/", "", i))
    
    for(l in cl){
      
      if(l == "prec/"){
        
        dir.create(sub("/", "", l))
        setwd(sub("/", "", l))
    
        url.e <- paste0(url, i, l)
    
        pg <- read_html(url.e)
    
        list <- grep(".7z", html_attr(html_nodes(pg, "a"), "href"), value = T)
    
        print(list)
    
        # for(m in list){
         #download(paste0(url.e, m), paste0(m), mode = "wb")}
     
        setwd("..")
  
      } else{
    
        dir.create(sub("/", "", l))
        setwd(sub("/", "", l))
    
        for(n in te){
          
          dir.create(sub("/", "", n))
          setwd(sub("/", "", n))
      
          url.e <- paste0(url, i, n)
      
          pg <- read_html(url.e)
      
          list <- grep(".7z", html_attr(html_nodes(pg, "a"), "href"), value = T)
      
          print(list)
      
          #for(o in list){
            #download(paste0(url.e, o), paste0(o), mode = "wb")}
      
          setwd("..")}
    
    setwd("..")}}}}
      



###-----------------------------------------------------------------------------###

