### script amphibians synonymys frost ###

# Maurício Humberto Vancine - mauricio.vancine@

###  amphibians synonymys frost	###

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
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

###---------------------------------------------------------------------###

## anura
an <- grep("/Anura/", fr, value = T)

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
  
  # genus
  ge <- grep("/Anura/", li, value = T)
  
  
  # for genus
  for(j in ge){
    
    # http of search
    url <- paste0("http://research.amnh.org", j)
    
    # page
    pg <- read_html(url)
    
    # link
    li <- html_nodes(pg, "a") %>%
      html_attr("href")
    
    # genus
    sp <- grep("-", grep("/Anura/", li, value = T), value = T)
    
    if(length(sp) == 0){
      te <- data.table(cbind(specie = last(last(strsplit(url, "/"))), link = url))
      da <- rbind(da, te)
      print(last(last(strsplit(url, "/"))))
      
      } else{
        
        for(k in 1:length(sp)){
        te <- data.table(cbind(specie = last(last(strsplit(sp[[k]], "/"))), 
                               link = paste0("http://research.amnh.org/", sp[[k]])))
        da <- rbind(da, te)
        
        print(last(last(strsplit(sp[[k]], "/"))))}}}}
      
###---------------------------------------------------------------------###

## caudata
ca  <- grep("/Caudata/", fr, value = T)

###---------------------------------------------------------------------###

## gymnophiona
gy <- grep("/Gymnophiona/", fr, value = T)

###---------------------------------------------------------------------###

