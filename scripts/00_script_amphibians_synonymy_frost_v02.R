### script amphibians synonymys frost ###

# Maurício Humberto Vancine - mauricio.vancine@

###  amphibians synonymys frost	###

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(rvest, data.table, stringr, dplyr)

###---------------------------------------------------------------------###

## frost
# http of search
url <- "http://research.amnh.org/vz/herpetology/amphibia/index.php"

# page
pg <- read_html(url)

# link
fr <- html_nodes(pg, "a") %>%
  html_attr("href")

fr

###---------------------------------------------------------------------###

## anura
an <- grep("/Anura/", grep("ae$", fr, value = T), value = T)
an

# data
da <- data.table()

# for family
for(i in an){
  
  # http of search
  url <- paste0("http://research.amnh.org", i)
  
  # page
  pg <- read_html(url)
  
  # link
  li <- html_nodes(pg, "a") %>%
    html_attr("href")
  
  if(grep("nae/", li, value = T) != 0){
    
    # http of search
    url <- paste0("http://research.amnh.org", i)
    
    # page
    pg <- read_html(url)
    
    # link
    li <- html_nodes(pg, "a") %>%
      html_attr("href")
    
    # genus
    ge <- grep("nae/", li, value = T)
    
    } else {
      
      # genus
      ge <- grep("/Anura/", li, value = T)}
  
  
      # for genus
      for(j in ge){
        
        # http of search
        url <- paste0("http://research.amnh.org", j)
    
    # page
    pg <- read_html(url)
    
    # link
    li <- html_nodes(pg, "a") %>%
      html_attr("href")
    
    # species
    sp <- grep("-", grep("/Anura/", li, value = T), value = T)
    
    if(length(sp) == 0){
      te <- data.table(cbind(species = last(last(strsplit(url, "/"))), links = url))
      da <- rbind(da, te)
      print(last(last(strsplit(url, "/"))))
      
      } else{
        
        for(k in 1:length(sp)){
        te <- data.table(cbind(specie = last(last(strsplit(sp[[k]], "/"))), 
                               link = paste0("http://research.amnh.org/", sp[[k]])))
        da <- rbind(da, te)
        
        print(last(last(strsplit(sp[[k]], "/"))))}}}}

fwrite(da, "test.csv")
      
###---------------------------------------------------------------------###

## caudata
ca  <- grep("/Caudata/", fr, value = T)

###---------------------------------------------------------------------###

## gymnophiona
gy <- grep("/Gymnophiona/", fr, value = T)

###---------------------------------------------------------------------###

